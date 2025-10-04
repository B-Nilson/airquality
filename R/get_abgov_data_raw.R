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
  value_cols <- .abgov_columns$values[
    .abgov_columns$values != "PM2.5 Mass" # drop qaqc API column
  ]
  variables <- variables |>
    standardize_input_vars(
      all_variables = names(value_cols) |>
        stringr::str_remove("_1hr")
    )
  value_cols <- value_cols[names(value_cols) %in% paste0(variables, "_1hr")]

  # Make request(s) as needed to load all desired data
  api_args <- date_range |>
    build_abgov_data_args(
      stations = stations,
      value_cols = value_cols,
      stations_per_call = stations_per_call,
      days_per_call = days_per_call
    )
  raw_data <- api_url |>
    paste0(api_endpoint, "?", api_args) |>
    handyr::for_each(
      abgov_get_raw_data_request,
      .show_progress = !quiet,
      .bind = TRUE
    )

  if (length(variables) == 1 & nrow(raw_data)) {
    raw_data$ParameterName <- value_cols[1]
  }
  return(raw_data)
}

abgov_get_raw_data_request <- function(api_request) {
  stopifnot(is.character(api_request), length(api_request) == 1)

  # Make request
  api_request <- api_request |>
    xml2::read_xml() |>
    xml2::as_list()

  # Extract data from response
  api_request$feed[-(1:4)] |>
    lapply(\(entry) {
      e <- unlist(entry$content$properties)
      if (!is.null(e)) tibble::tibble(data.frame(t(e)))
    }) |>
    dplyr::bind_rows()
}

format_abgov_raw_data <- function(raw_data, date_range, desired_cols) {
  stopifnot(is.data.frame(raw_data), nrow(raw_data) > 0)
  stopifnot(lubridate::is.POSIXct(date_range), length(date_range) == 2)
  stopifnot(is.character(desired_cols), length(desired_cols) > 0)

  tzone <- "America/Edmonton" # TODO: confirm this?

  raw_data |>
    # Convert dates, mark not quality assured, and filter to desired range
    dplyr::mutate(
      ReadingDate = .data$ReadingDate |>
        lubridate::ymd_hms(tz = tzone) |>
        lubridate::with_tz("UTC"),
      quality_assured = FALSE
    ) |>
    dplyr::filter(
      .data$ReadingDate |>
        dplyr::between(date_range[1], date_range[2])
    ) |>
    dplyr::distinct() |>
    # Long -> wide, standardize column names
    tidyr::pivot_wider(
      names_from = "ParameterName",
      values_from = "Value"
    ) |>
    dplyr::select(dplyr::any_of(desired_cols)) |>
    # Insert units and standardize if needed
    standardize_obs_units(
      input_units = abgov_units,
      default_units = default_units
    )
}

build_abgov_data_args <- function(
  date_range,
  stations = "all",
  value_cols, # TODO: rename to parameters?
  stations_per_call = 3,
  days_per_call = 3,
  time_out = 36000
) {
  stopifnot(is.character(value_cols), length(value_cols) > 0)
  stopifnot(is.numeric(time_out), length(time_out) == 1)

  # Build column selector
  selected_cols <- c("StationName", "ParameterName", "ReadingDate", "Value") |>
    paste(collapse = ",")
  if (length(value_cols) == 1) { # TODO: what if "all"?
    selected_cols <- selected_cols |>
      stringr::str_remove(",ParameterName")
  }

  # Build filters
  station_filters <- stations |>
    abgov_make_raw_station_filter(stations_per_call = stations_per_call)
  date_filters <- date_range |>
    abgov_make_raw_date_filter(days_per_call = days_per_call)
  param_filter <- value_cols |> # TODO: will never be "all", but expects that
    abgov_make_raw_param_filter()
  filters <- station_filters |>
    sapply(\(station_filter) {
      station_filter |>
        paste(date_filters, param_filter, sep = " and ")
    }) |>
    unlist() |>
    unname() |> # TODO: needed?
    paste("and Value ne null")

  # Insert into template, cleanup symbols for url
  "$filter=%s&$select=%s&Connection Timeout=%s" |>
    sprintf(filters, selected_cols, time_out) |>
    utils::URLencode(reserved = TRUE) |> # TODO: reevaluate encoding here
    stringr::str_replace_all("%3D", "=") |>
    stringr::str_replace_all("%2C", ",") |>
    stringr::str_replace_all("%26", "&") |>
    stringr::str_replace_all("%24", "$")
}

abgov_make_raw_station_filter <- function(
  stations = "all",
  stations_per_call = 3
) {
  stopifnot(is.character(stations), length(stations) > 0)
  stopifnot(is.numeric(stations_per_call), length(stations_per_call) == 1)

  station_filter_template <- "indexof('%s', StationName) ge %s" # site_name, -1|0

  # Don't filter by site if "all" stations
  if (any(stations == "all")) {
    return(
      station_filter_template |> sprintf("t", -1)
    )
  }

  # Split stations up as needed, and build filter for each
  seq(1, length(stations), stations_per_call) |>
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

abgov_make_raw_date_filter <- function(
  date_range,
  days_per_call = 3
) {
  stopifnot(lubridate::is.POSIXct(date_range), length(date_range) == 2)
  stopifnot(is.numeric(days_per_call), length(days_per_call) == 1)

  # Setup
  tzone <- "America/Edmonton" # TODO: confirm
  max_duration <- paste(days_per_call, "days")
  date_filter_template <- "ReadingDate ge datetime'%s'" |> 
    paste("ReadingDate le datetime'%s'", sep = " and ")

  # Split date_range as needed, and build filter for each
  date_range |>
    lubridate::with_tz(tzone) |>
    handyr::split_date_range(max_duration = max_duration) |>
    apply(1, \(dr) {
      dr <- dr |>
        lubridate::as_datetime() |>
        format("%FT%T")
      date_filter_template |> sprintf(dr[1], dr[2])
    })
}

abgov_make_raw_param_filter <- function(parameters = "all") {
  stopifnot(is.character(parameters), length(parameters) > 0)

  param_filter_template <- "indexof('%s', ParameterName) ge %s"

  # Don't filter by site if "all" parameters
  if (any(parameters == "all")) {
    return(param_filter_template |> sprintf("t", -1))
  }

  # Build filter
  param_filter <- param_filter_template |>
    sprintf(parameters, 0) |>
    paste(collapse = " or ")
  paste0("(", param_filter, ")")
}
