get_abgov_data_raw <- function(
  stations,
  date_range,
  variables = "all",
  stations_per_call = 1,
  days_per_call = 90,
  quiet = FALSE
) {
  api_url <- "https://data.environment.alberta.ca/Services/AirQualityV2/AQHI.svc/"
  api_endpoint <- "StationMeasurements"

  # Handle input variables
  value_cols <- .abgov_columns$values[.abgov_columns$values != "PM2.5 Mass"] # qaqc API column
  all_variables <- names(value_cols) |>
    stringr::str_remove("_1hr")
  variables <- variables |>
    standardize_input_vars(all_variables)
  value_cols <- value_cols[names(value_cols) %in% paste0(variables, "_1hr")]

  # Make request(s) as needed to load all desired data
  api_args <- stations |>
    build_abgov_data_args(
      date_range = date_range,
      variables = variables,
      stations_per_call = stations_per_call,
      days_per_call = days_per_call
    )
  raw_data <- api_url |>
    paste0(api_endpoint, "?", api_args) |>
    handyr::for_each(
      abgov_get_raw_data_request,
      quiet = quiet,
      .bind = TRUE
    )

  if (length(variables) == 1 & nrow(raw_data)) {
    raw_data$ParameterName <- value_cols[1]
  }
  return(raw_data)
}

format_abgov_raw_data <- function(raw_data, date_range, desired_cols) {
  if (nrow(raw_data) == 0) {
    return(NULL)
  }
  pivot_cols <- c("ParameterName", "Value")

  raw_data |>
    # Convert dates, mark not quality assured, and filter to desired range
    dplyr::mutate(
      ReadingDate = .data$ReadingDate |>
        lubridate::ymd_hms(tz = tzone) |>
        lubridate::with_tz("UTC"),
      Value = as.numeric(Value),
      quality_assured = FALSE
    ) |>
    dplyr::filter(
      .data$ReadingDate |>
        dplyr::between(date_range[1], date_range[2])
    ) |>
    dplyr::distinct() |>
    # Long -> wide, standardize column names
    tidyr::pivot_wider(
      names_from = pivot_cols[1],
      values_from = pivot_cols[2]
    ) |>
    dplyr::select(dplyr::any_of(desired_cols)) |>
    # Insert units and standardize if needed
    standardize_obs_units(
      default_units = default_units,
      input_units = abgov_units
    )
}

build_abgov_data_args <- function(
  stations = "all",
  date_range,
  variables = "all",
  stations_per_call = 3,
  days_per_call = 3,
  time_out = 36000
) {
  # Handle input variables
  value_cols <- .abgov_columns$values[.abgov_columns$values != "PM2.5 Mass"] # qaqc API column
  all_variables <- names(value_cols) |>
    stringr::str_remove("_1hr")
  variables <- variables |>
    standardize_input_vars(all_variables)
  get_all_vars <- all(all_variables %in% variables)
  value_cols <- value_cols[names(value_cols) %in% paste0(variables, "_1hr")]

  # Build column selector
  selected_cols <- c("StationName", "ParameterName", "ReadingDate", "Value") |>
    paste(collapse = ",")
  if (length(variables) == 1) {
    selected_cols <- selected_cols |>
      stringr::str_remove(",ParameterName")
  }

  # Build station(s) filter
  station_filter_template <- "indexof('%s', StationName) ge %s" # site_name, -1|0
  if (stations == "all") {
    station_filters <- station_filter_template |>
      sprintf("t", -1)
  } else {
    station_filters <- seq(1, length(stations), stations_per_call) |>
      sapply(\(start) {
        end <- start + stations_per_call
        end <- (end > length(stations)) |>
          ifelse(length(stations), end)
        filter <- station_filter_template |>
          sprintf(stations[start:end], 0) |>
          paste(collapse = " or ")
        paste0("(", filter, ")")
      })
  }

  # Build date filter(s)
  tzone <- "America/Edmonton"
  date_filter_template <- "ReadingDate ge datetime'%s' and ReadingDate le datetime'%s'"
  date_filters <- (date_range - lubridate::hours(c(1, 0))) |> # TODO: Check if this is needed
    lubridate::with_tz(tzone) |>
    handyr::split_date_range(max_duration = paste(days_per_call, "days")) |>
    apply(1, \(desired_range) {
      desired_range <- desired_range |>
        lubridate::as_datetime() |>
        format("%FT%T")
      date_filter_template |>
        sprintf(desired_range[1], desired_range[2])
    })

  # Build parameter filter
  param_filter_template <- "indexof('%s', ParameterName) ge %s"
  if (get_all_vars) {
    param_filter <- param_filter_template |>
      sprintf("t", -1)
  } else {
    param_filter <- param_filter_template |>
      sprintf(value_cols, 0) |>
      paste(collapse = " or ")
    param_filter <- paste0("(", param_filter, ")")
  }

  # Combine filters
  filters <- station_filters |>
    sapply(\(station_filter) {
      date_filters |>
        sapply(\(date_filter) {
          paste(station_filter, date_filter, param_filter, sep = " and ")
        })
    }) |>
    unlist() |>
    unname() |>
    paste("and Value ne null")

  # Insert into template, cleanup symbols for url
  args_template <- "$filter=%s&$select=%s&Connection Timeout=%s"
  args_template |>
    sprintf(filters, selected_cols, time_out) |>
    utils::URLencode(reserved = TRUE) |> # TODO: reevaluate encoding here
    stringr::str_replace_all("%3D", "=") |>
    stringr::str_replace_all("%2C", ",") |>
    stringr::str_replace_all("%26", "&") |>
    stringr::str_replace_all("%24", "$")
}

abgov_get_raw_data_request <- function(api_request, quiet = FALSE) {
  if (!quiet) {
    message(api_request)
  }
  api_request <- api_request |>
    xml2::read_xml() |>
    xml2::as_list() |>
    # server sends 400 error when no data in query
    handyr::on_error(.return = NULL)

  if (is.null(api_request)) {
    return(NULL)
  }

  api_request$feed[-(1:4)] |>
    lapply(\(entry) {
      e <- unlist(entry$content$properties)
      if (!is.null(e)) tibble::tibble(data.frame(t(e)))
    }) |>
    dplyr::bind_rows()
}
