#' Download air quality station observations from the US EPA "AirNow" platform
#'
#' @param stations (Optional).
#'   Either "all" or a character vector specifying AQS IDs for stations to filter data to.
#'   If "all" not provided, data for all stations for each hour in `date_range` are still downloaded,
#'   but only data for desired stations is returned.
#'   Default is "all".
#' @param date_range (Optional).
#'   A datetime vector (or a character vector with UTC dates in "YYYY-MM-DD HH" format, or "now" for current hour) with either 1 or 2 values.
#'   Providing a single value will return data for that hour only,
#'   whereas two values will return data between (and including) those times.
#'   Dates are "backward-looking", so a value of "2019-01-01 01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00".
#'   Default is "now" (the current hour).
#' @param variables (Optional).
#'   A character vector of one or more variables to try and get data for.
#'   All variables are downloaded regardless of this parameter, but only data for desired variables is returned.
#'   Default is "all", i.e. all available variables.
#' @param raw (Optional).
#'   A single logical (TRUE or FALSE) value indicating if
#'   raw data files are desired (i.e. without a standardized format).
#'   Default is FALSE.
#' @param fast (Optional).
#'   A single logical (TRUE or FALSE) value indicating if, where possible, time-intensive code should be skipped and parallel processing should be used.
#'   Default is FALSE.
#' @param quiet (Optional).
#'   A single logical (TRUE or FALSE) value indicating if
#'   non-critical messages/warnings should be silenced.
#'   Default is FALSE.
#'
#' @description
#' AirNow is a US EPA nationwide voluntary program which hosts non-validated air quality
#' observation data from stations in the US and many other countries globally.
#'
#' The AirNow API provides access to hourly raw observation files which are updated
#' as data are received from the various monitoring agencies. Due to the real-time,
#' non-validated nature of these data great care must be taken if using these
#' data to support regulation, trends, guidance, or any other government or public decision making.
#' It is highly recommended to seek out quality assured data where possible.
#'
#' [get_airnow_data] provides an easy way to retrieve these observations using
#' AirNow's station IDs (see [get_airnow_stations]) and a specified date or date range.
#'
#' Due to the API's file structure, data retrieval time is proportional to the
#' number of hours of data desired, regardless of the number of stations or variables requested.
#'
#' @return
#' A tibble of hourly observation data for desired station(s) and date range where available.
#' Columns returned will vary depending on available data from station(s).
#' If `raw = FALSE` (default), the returned data will be in a standardized format and units will be attached to each variable.
#'
#' Dates are UTC time and "backward-looking", so a value of "2019-01-01 01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00".
#' @export
#'
#' @family Data Collection
#'
#' @examples
#' \donttest{
#' # Get data for all stations for first 3 hours (UTC) of Jan 2019
#' get_airnow_data("all", c("2019-01-01 01", "2019-01-01 03"))
#'
#' # Get data for two specific stations for first 3 hours (UTC) of Jan 2019
#' get_airnow_data(c("000010102", "000010401"), c("2019-01-01 01", "2019-01-01 03"))
#'
#' # Get non-standardized data for all stations for first 3 hours (PST) of Jan 2019
#' date_range <- lubridate::ymd_h(c("2019-01-01 01", "2019-01-01 03"), tz = "Etc/GMT+8")
#' get_airnow_data("all", date_range, raw = TRUE)
#' }
get_airnow_data <- function(
  stations = "all",
  date_range = "now",
  variables = "all",
  raw = FALSE,
  fast = FALSE,
  quiet = FALSE
) {
  stopifnot(is.character(stations))
  stopifnot(is.character(date_range) | lubridate::is.POSIXct(date_range))
  stopifnot(is.character(variables))
  stopifnot(is.logical(raw))
  stopifnot(is.logical(fast))
  stopifnot(is.logical(quiet))

  # Constants/setup
  allowed_date_range <- c("2014-01-01 01", "now") |> # TODO: confirm this
    handyr::check_date_range()
  data_citation("AirNow", quiet = quiet)

  # Handle date_range inputs
  date_range <- date_range |>
    handyr::check_date_range(within = allowed_date_range)

  # Handle input variables
  all_variables <- names(.airnow_columns$values) |>
    stringr::str_remove("_1hr")
  variables <- variables |>
    standardize_input_vars(all_variables)
  get_all_vars <- all(all_variables %in% variables)
  value_cols_to_drop <- .airnow_columns$values[
    !names(.airnow_columns$values) %in% paste0(variables, "_1hr")
  ]

  # Get hourly data files for desired date range
  airnow_data <- date_range |>
    make_airnow_filepaths(quiet = quiet) |>
    handyr::for_each(
      .bind = TRUE,
      .parallel = fast,
      \(airnow_file_path) {
        airnow_file_path |>
          read_airnow_data_file(quiet = quiet) |>
          handyr::on_error(.return = NULL)
      }
    ) |>
    tibble::as_tibble()

  if (nrow(airnow_data) == 0) {
    stop(paste(
      "No data available for provided date range.",
      "Ensure `date_range` is valid and AirNow is not offline",
      "(see: https://www.airnowtech.org/ for AirNow status)."
    ))
  }

  # Filter for desired stations if "all" not supplied
  if (!"all" %in% stations) {
    airnow_data <- airnow_data |>
      dplyr::filter(.data$siteID %in% stations)
  }

  # Drop non-desired variables
  airnow_data <- airnow_data |>
    dplyr::filter(!.data$param %in% value_cols_to_drop)

  # If raw data desired, end function and return data
  if (raw) {
    return(airnow_data)
  }

  # Get meta for stations within date_range for adding date_local
  if (!fast) {
    known_stations <- date_range |>
      get_airnow_stations(time_step = "25 days")
  } else {
    known_stations <- NULL
  }

  if (nrow(airnow_data) == 0) {
    stop("No data available for desired stations during specified date range.")
  }

  # Standardize formatting
  airnow_data |>
    dplyr::mutate(
      date_utc = paste(.data$date, .data$time) |>
        lubridate::mdy_hm(tz = "UTC") +
        lubridate::hours(1), # from forward -> backward looking averages
      unit = stringr::str_to_lower(.data$unit),
      quality_assured = FALSE
    ) |>
    widen_with_units(
      unit_col = "unit",
      value_col = "value",
      name_col = "param",
      desired_cols = unlist(unname(.airnow_columns))
    ) |>
    standardize_data_format(
      date_range = date_range,
      known_stations = known_stations,
      fast = fast,
      raw = raw
    )
}

make_airnow_filepaths <- function(date_range, quiet = FALSE) {
  stopifnot("POSIXct" %in% class(date_range) | "Date" %in% class(date_range))
  stopifnot(is.logical(quiet), length(quiet) == 1)

  airnow_site <- "https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow"

  # Data may be missing for most recent hourly files - depending on data transfer delays
  # Warn user of this if requesting data in past 48 hours, especially if last 55 minutes
  allowed_date_range <- handyr::check_date_range("now", tz = "UTC")
  if (max(date_range) - allowed_date_range[2] > lubridate::hours(-48)) {
    if (max(date_range) - allowed_date_range[2] > lubridate::minutes(-55)) {
      if (!quiet) {
        warning(paste(
          "The current hour AirNow files is updated twice per hour",
          "(at 25 and 55 minutes past the hour) or more frequently if possible.",
          "All hourly files for the preceding 48 hours will be updated every hour",
          "to ensure data completeness and quality.",
          "\n\tData may be missing from stations for any hours in the past 48, especially for the current hour."
        ))
      }
    } else {
      # if date_range in past 48 hours but not past 55 minutes
      if (!quiet) {
        warning(paste(
          "All hourly AirNow files for the preceding 48 hours will be updated every hour",
          "to ensure data completeness and quality.",
          "\n\tData may be missing from stations for any hours in the past 48, especially for the current hour."
        ))
      }
    }
  }

  # Get forward looking dates for each hour in range
  dates <- date_range[1] |>
    seq(date_range[2], "1 hours") -
    lubridate::hours(1)
  dates <- dates |> lubridate::with_tz("UTC")

  # Build file paths for each hour
  airnow_site |>
    file.path(
      dates |> format("%Y"),
      dates |> format("%Y%m%d"),
      "HourlyData_%s.dat" |> sprintf(dates |> format("%Y%m%d%H"))
    )
}

read_airnow_data_file <- function(airnow_file_path, quiet = FALSE) {
  stopifnot(is.character(airnow_file_path), length(airnow_file_path) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)

  file_header <- c(
    "date",
    "time",
    "siteID",
    "site",
    "tz_offset",
    "param",
    "unit",
    "value",
    "operator"
  )
  data.table::fread(
    file = airnow_file_path,
    showProgress = !quiet
  ) |>
    stats::setNames(file_header)
}


.airnow_columns <- list(
  # Meta
  meta = c(
    date_utc = "date_utc", # Added by get_airnow_data()
    site_id = "siteID",
    site_name = "site",
    quality_assured = "quality_assured" # Added by get_airnow_data()
  ),
  values = c(
    # Particulate Matter
    pm25_1hr = "PM2.5",
    pm10_1hr = "PM10",
    bc_1hr = "BC",
    # Ozone
    o3_1hr = "OZONE",
    # Nitrogen Pollutants
    no_1hr = "NO",
    no2t_1hr = "NO2T", # t == "true measure"
    no2_1hr = "NO2",
    no2y_1hr = "NO2Y", # y == "reactive"
    nox_1hr = "NOX",
    noy_1hr = "NOY", # y == "reactive"
    no3_1hr = "NO3",
    # Sulfur Pollutants
    so4_1hr = "SO4",
    so2_1hr = "SO2",
    so2t_1hr = "SO2T", # t == "trace"
    # Carbon Monoxide
    co_1hr = "CO",
    cot_1hr = "COT", # t == "trace"
    # Met data
    rh_1hr = "RHUM",
    t_1hr = "TEMP",
    wd_1hr = "WD",
    ws_1hr = "WS",
    precip_1hr = "PRECIP",
    pressure_1hr = "BARPR",
    solar_1hr = "SRAD"
  )
)
