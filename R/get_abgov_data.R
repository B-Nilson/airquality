#' Download air quality station observations from the Alberta (Canada) Government
#'
#' @param stations A character vector of one or more station names to try and get data desired for (see [get_abgov_stations()]).
#' @param date_range A datetime vector (or a character vector with UTC dates in "YYYY-MM-DD HH" format, or "now" for current hour) with either 1 or 2 values.
#' Providing a single value will return data for that hour only,
#' whereas two values will return data between (and including) those times.
#' Dates are "backward-looking", so a value of "2019-01-01 01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00".
#' @param raw (Optional) A single logical (TRUE or FALSE) value indicating
#' if raw data files desired (i.e. without a standardized format). Default is FALSE.
#' @param fast (Optional) A single logical (TRUE or FALSE) value indicating if time-intensive code should be skipped where possible.
#' Default is FALSE.
#' @param quiet (Optional) A single logical (TRUE or FALSE) value indicating if
#' non-critical messages/warnings should be silenced
#' @param stations_per_call (Optional) A single numeric value indicating the maximum number of stations to request per API call.
#' The API header requires station names to be passed as a comma-separated list, too manyu stations may cause an eror depending on station name length.
#' Default is 1.
#' @param days_per_call (Optional) A single numeric value indicating the maximum number of days (per station) to request per API call.
#' This is a safety measure to prevent the API from timing out by requesting too many days at once.
#' Default is 90.
#'
#' @description
#' Air pollution monitoring in Canada is done by individual Provinces/Territories,
#' primarily as a part of the federal National Air Pollution Surveillance (NAPS) program.
#' The Province of Alberta hosts it's hourly air quality observations
#' through a public API, providing both historic and real-time raw data.
#'
#' [get_abgov_data()] provides an easy way to retrieve these observations using
#' station name(s) (see [get_abgov_stations()]) and a specified date or date range.
#'
#' @seealso [get_abgov_stations()]
#' @return
#' A tibble of hourly observation data for desired station(s) and date range where available.
#' Columns returned will vary depending on available data from station(s).
#'
#' Dates UTC time and are "backward-looking", so a value of "2019-01-01 01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00".
#' @family Data Collection
#' @family Canadian Air Quality
#'
#' @export
#' @examples
#' \donttest{
#' get_abgov_data(
#'   stations = "Calgary Southeast",
#'   date_range = c("2024-01-05 00", "2025-01-05 23")
#' )
#' }
get_abgov_data <- function(
  stations = "all",
  date_range = "now",
  variables = "all",
  raw = FALSE,
  fast = FALSE,
  quiet = FALSE,
  stations_per_call = 2,
  days_per_call = 10
) {
  stopifnot(is.character(stations))
  stopifnot(is.character(date_range) | lubridate::is.POSIXct(date_range))
  stopifnot(is.character(variables))
  stopifnot(is.logical(raw))
  stopifnot(is.logical(fast))
  stopifnot(is.logical(quiet))
  stopifnot(is.numeric(stations_per_call))
  stopifnot(is.numeric(days_per_call))

  # Constants
  tzone <- "America/Edmonton" # TODO: confirm this
  allowed_date_range <- c("1980-01-01 00", "now") # TODO: confirm this

  # Output citation message to user
  data_citation("ABgov", quiet = quiet)

  # Handle date_range inputs
  date_range <- date_range |>
    handle_date_range(within = allowed_date_range, tz = tzone)

  # Handle input variables
  all_variables <- names(.abgov_columns$values) |>
    stringr::str_remove("_1hr")
  variables <- variables |>
    standardize_input_vars(all_variables)

  # Only get data for stations that exist on the APIs
  if (!fast & !"all" %in% stations) {
    known_stations <- get_abgov_stations()
    stations <- stations |>
      check_stations_exist(
        known_stations = known_stations$site_name, # (error if no stations in known_stations)
        source = "the Alberta Gov. API"
      )
  }

  # Get QAQC'ed data if any
  qaqc_data <- stations |>
    get_abgov_data_qaqc(
      date_range = date_range,
      variables = variables,
      fast = fast,
      quiet = quiet
    ) |>
    abgov_format_qaqc_data(
      date_range = date_range,
      desired_cols = unlist(unname(.abgov_columns))
    ) |>
    handyr::on_error(.return = data.frame())

  # Alter date_range to account for retrieved QAQC data
  date_range_new <- date_range
  if (nrow(qaqc_data)) {
    max_qaqc_date <- max(qaqc_data$date_utc)
    if (max_qaqc_date >= date_range[2]) {
      date_range_new <- NULL
    } else {
      date_range_new[1] <- max_qaqc_date
    }
  }

  # Get raw data if needed
  if (!is.null(date_range_new)) {
    raw_data <- stations |>
      get_abgov_data_raw(
        date_range = date_range_new,
        variables = variables,
        stations_per_call = stations_per_call,
        days_per_call = days_per_call,
        quiet = quiet
      ) |>
      format_abgov_raw_data(
        date_range = date_range_new,
        desired_cols = unlist(unname(.abgov_columns))
      ) |>
      handyr::on_error(.return = NULL)
  }

  # Combine and standardize formatting
  stations_data <- list(qaqc_data, raw_data) |>
    dplyr::bind_rows() |>
    standardize_data_format(
      date_range = date_range,
      known_stations = known_stations,
      fast = fast,
      raw = raw
    )

  return(stations_data)
}

## AB MoE Helpers ---------------------------------------------------------

.abgov_columns <- list(
  meta = c(
    site_name = "StationName",
    quality_assured = "quality_assured",
    date_utc = "ReadingDate"
  ),
  values = c(
    # Particulate Matter
    pm25_1hr = "PM2.5 Mass", # qaqc
    pm25_1hr = "Fine Particulate Matter", # raw
    # pm10_1hr_ugm3 = "PM10", # TODO: check this
    # Ozone
    o3_1hr = "Ozone",
    # Nitrogen Pollutants
    no_1hr = "Nitric Oxide",
    no2_1hr = "Nitrogen Dioxide",
    nox_1hr = "Total Oxides of Nitrogen", # TODO: check this
    nh3_1hr = "Ammonia", # TODO: check this
    # Sulfur Pollutants
    so2_1hr = "Sulphur Dioxide",
    trs_1hr = "Total Reduced Sulphur",
    h2s_1hr = "Hydrogen Sulphide",
    # # Carbon Monoxide
    co_1hr = "Carbon Monoxide",
    # Methane
    ch4_1hr = "Methane",
    # Hydrocarbons
    hc_1hr = "Total Hydrocarbons",
    hc_nm_1hr = "Non-methane Hydrocarbons",
    # Met data
    rh_1hr = "Relative Humidity",
    t_1hr = "Outdoor Air Temperature",
    wd_1hr = "Wind Direction",
    wd_sd_1hr = "Std. Dev. of Wind Direction",
    ws_1hr = "Wind Speed",
    solar_1hr = "Solar Radiation",
    pressure_1hr = "Barometric Pressure (non-adjusted)"
  )
)

# TODO: check these are right (compare raw with qaqc since qaqc units are provided)
abgov_units <- c(
  # Particulate Matter
  pm25_1hr = "ug/m3",
  # pm10_1hr_ugm3 = "ug/m3",
  # Ozone
  o3_1hr = "ppm",
  # Nitrogen Pollutants
  no_1hr = "ppb",
  no2_1hr = "ppm", # TODO: check
  nox_1hr = "ppb",
  nh3_1hr = NA, # TODO: check
  # Sulfur Pollutants
  so2_1hr = "ppb",
  trs_1hr = "ppb",
  h2s_1hr = "ppm", # TODO: check
  # # Carbon Monoxide
  co_1hr = "ppm",
  # Methane
  ch4_1hr = "ppm",
  # Hydrocarbons
  hc_1hr = "ppm",
  hcnm_1hr = "ppm",
  # Met data
  rh_1hr = "percent",
  t_1hr = "degC",
  wd_1hr = "degrees",
  ws_1hr = "km/h",
  solar_1hr = NA # check
)
