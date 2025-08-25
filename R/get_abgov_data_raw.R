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
  id_cols <- c("site_name", "date_utc", "quality_assured")
  value_cols <- abgov_col_names[!names(abgov_col_names) %in% id_cols]
  value_cols <- value_cols[value_cols !=  "PM2.5 Mass"] # qaqc API column
  all_variables <- names(value_cols) |>
    stringr::str_remove("_1hr")
  variables <- variables |>
    standardize_input_vars(all_variables)
  get_all_vars <- all(all_variables %in% variables)

  # Make request(s) as needed to load all desired data
  api_args <- stations |>
    build_abgov_data_args(
      date_range,
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
  
  if (length(variables) == 1) {
    raw_data$ParameterName <- value_cols[1]
  }
  return(raw_data)
}

format_abgov_raw_data <- function(raw_data, date_range, desired_cols) {
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
    dplyr::select(dplyr::any_of(abgov_col_names)) |>
    # Insert units and standardize if needed
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(names(abgov_units)), 
        convert_units,
        in_unit = abgov_units[names(abgov_units) == dplyr::cur_column()],
        out_unit = default_units[names(default_units) == dplyr::cur_column()]
      )
    )
}

build_abgov_data_args <- function(
  stations = "all",
  date_range,
  variables = "all",
  stations_per_call = 3,
  days_per_call = 3
) {
  # Handle input variables
  id_cols <- c("site_name", "date_utc", "quality_assured")
  value_cols <- abgov_col_names[!names(abgov_col_names) %in% id_cols]
  value_cols <- value_cols[value_cols != "PM2.5 Mass"] # qaqc API column
  all_variables <- names(value_cols) |>
    stringr::str_remove("_1hr")
  variables <- variables |>
    standardize_input_vars(all_variables)
  get_all_vars <- all(all_variables %in% variables)
  value_cols <- value_cols[names(value_cols) %in% paste0(variables, "_1hr")]

  time_out <- 36000
  args_template <- "$filter=%s&$select=%s&Connection Timeout=%s"

  # Only select cols we need
  selected_cols <- c("StationName", "ParameterName", "ReadingDate", "Value") |> 
    paste(collapse = ",")
  if (length(variables) == 1) {
    selected_cols <- selected_cols |>
      stringr::str_remove(",ParameterName")
  }

  # Build station filter(s)
  if (stations == "all") {
    station_filters <- "(indexof('test', StationName) ge -1)"
  } else {
    station_filters <- seq(1, length(stations), stations_per_call) |>
      sapply(\(s) {
        is_past_n <- (s + stations_per_call) > length(stations)
        end <- !is_past_n |>
          ifelse(
            s + stations_per_call,
            length(stations)
          )
        prefix <- "(indexof('"
        seperator <- "', StationName) ge 0 or indexof('"
        suffix <- "', StationName) ge 0)"
        s_query <- stations[s:end] |> paste0(collapse = seperator)
        paste0(prefix, s_query, suffix)
      })
  }
  
  # Build date filter(s)
  steps <- paste(days_per_call, "days")
  starts <- (date_range[1] - lubridate::hours(1)) |>
    seq(date_range[2], steps)
  ends <- starts + lubridate::days(days_per_call)
  ends[ends > date_range[2]] <- date_range[2]
  date_filters <- 1:length(starts) |>
    sapply(\(i) {
      s <- starts[i] |> format("%FT%T")
      e <- ends[i] |> format("%FT%T")
      prefix <- "(ReadingDate ge datetime'"
      seperator <- "' and ReadingDate le datetime'"
      suffix <- "')"
      paste0(prefix, s, seperator, e, suffix)
    })
  # Combine arguments
  station_filters |>
    sapply(\(station_filter) {
      date_filters |>
        sapply(\(date_filter) {
          if (get_all_vars) {
            # Parameter filter is required, so do a dummy one that allows for any parameter
            param_filter <- "indexof('Fine Particulate Matter', ParameterName) ge -1"
          } else {
            param_filter <- paste0("indexof('", value_cols, "', ParameterName) ge 0") |> 
              paste(collapse = " or ")
          }
          station_filter |>
            paste(date_filter, param_filter, sep = " and ") |> 
            paste("and Value ne null")
        })
    }) |>
    unlist() |>
    unname()
  
  # Insert into template, cleanup symbols for url
  args_template |>
    sprintf(filters, selected_cols, time_out) |>
    utils::URLencode(reserved = TRUE) |>
    stringr::str_replace_all("%3D", "=") |>
    stringr::str_replace_all("%2C", ",") |>
    stringr::str_replace_all("%26", "&")
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
