#' Download air quality station observations from the British Columbia (Canada) Government
#'
#' @param stations A character vector of one or more station IDs (BC EMS IDs) to try and get data desired for (see [get_bcgov_stations()]).
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
#'
#' @description
#' Air pollution monitoring in Canada is done by individual Provinces/Territories,
#' primarily as a part of the federal National Air Pollution Surveillance (NAPS) program.
#' The Province of British Columbia hosts it's hourly air quality observations
#' through a public FTP site, providing both historic QA/QC'ed and real-time raw data.
#'
#' Annual QA/QC'ed files are available on the BC FTP site for each monitoring station,
#' however these are usually 1-2 years out of date due to the QA/QC process.
#' A single file is available for each station for all non-QA/QC'ed years, which has
#' potentially 0-2+ years of data depending on the time since the last QA/QC'ed dataset was created).
#'
#' [get_bcgov_data()] provides an easy way to retrieve these observations using
#' BC's station "EMS IDs"  (see [get_bcgov_stations()]) and a specified date or date range.
#'
#' Due to the FTP site's file structure, data retrieval time is proportional to the number of stations requested
#' as well as the number of years of data (PST timezone) desired. There can be potentially longer retrieval times
#' for non-QA/QC'ed years depending on the current time since the last QA/QC'ed year due to larger file sizes.
#'
#' @seealso [get_bcgov_stations()]
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
#' # For a single station
#' station <- "0450307" # EMS IDs - see get_bcgov_stations()
#' # For the years 2019 and 2020
#' date_range <- lubridate::ymd_h(c("2019-01-01 00", "2020-12-31 23"), tz = "Etc/GMT+8")
#' get_bcgov_data(station, date_range)
#'
#' # For multiple stations
#' stations <- c("0450307", "E206898") # EMS IDs - see get_bcgov_stations()
#' # For first week of January 2019
#' date_range <- lubridate::ymd_h(c("2019-01-01 00", "2019-01-07 23"), tz = "Etc/GMT+8")
#' get_bcgov_data(stations, date_range)
#' }
get_bcgov_data <- function(
  stations,
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

  # Output citation message to user
  if (!quiet) {
    data_citation("BCgov")
  }

  # Handle date_range inputs
  qaqc_years <- bcgov_get_qaqc_years()
  min_date <- min(qaqc_years) |>
    paste("01-01 01") |>
    lubridate::ymd_h(tz = bcgov_tzone)
  max_date <- Sys.time() |> lubridate::floor_date("hours")
  date_range <- date_range |> handle_date_range(within = c(min_date, max_date))
  original_date_range <- date_range # copy for later

  # Handle variables input
  all_variables <- names(bcgov_col_names)[endsWith(
    bcgov_col_names,
    "_INSTRUMENT"
  )] |>
    stringr::str_remove("_1hr_instrument")
  variables <- variables |>
    standardize_input_vars(all_variables)

  # Get all years in desired date range and drop all but the first in qaqc_years
  years_to_get <- date_range[1] |>
    seq(date_range[2], by = "1 days") |>
    lubridate::with_tz(bcgov_tzone) |>
    lubridate::year() |>
    bcgov_determine_years_to_get(qaqc_years)

  # Filter to existing stations only
  if (!fast) {
    known_stations <- years_to_get |>
      get_bcgov_stations(use_sf = FALSE, quiet = quiet)

    stations <- stations |>
      check_stations_exist(
        known_stations = known_stations$site_id,
        source = "the BC FTP site"
      )
  }

  # Get realtime data if needed
  realtime_start <- lubridate::now(tz = bcgov_tzone) -
    lubridate::days(30)
  need_realtime <- any(date_range > realtime_start)
  if (need_realtime) {
    is_value_or_instrument <- names(bcgov_col_names) |>
      stringr::str_starts(variables |> paste0(collapse = "|"))
    is_instrument <- names(bcgov_col_names) |> endsWith("_instrument")
    variable_cols <- bcgov_col_names[is_value_or_instrument & !is_instrument]
    realtime_data <- stations |>
      bcgov_get_raw_data(
        variables = variables,
        quiet = quiet,
        mode = "realtime"
      ) |>
      dplyr::filter(
        !dplyr::if_all(dplyr::any_of(variable_cols), is.na),
        lubridate::ymd_hm(.data$DATE_PST, tz = bcgov_tzone) > realtime_start
      ) |>
      handyr::on_error(.return = NULL)
    if (nrow(realtime_data) == 0) {
      warning(
        "No realtime data available for provided variables, stations and date_range."
      )
      realtime_data <- NULL
    } else if (is.null(realtime_data)) {
      warning(
        "No realtime data available for provided stations and date_range."
      )
    } else {
      first_realtime_date <- realtime_data |>
        dplyr::group_by(EMS_ID) |>
        dplyr::summarise(
          first_realtime_date = DATE_PST |>
            lubridate::ymd_hm(tz = bcgov_tzone) |>
            min()
        ) |>
        dplyr::pull(first_realtime_date) |>
        max()
      date_range <- c(original_date_range[1], first_realtime_date)
      is_all_realtime <- date_range[1] >= date_range[2]
    }
  } else {
    realtime_data <- NULL
    is_all_realtime <- FALSE
  }

  # Get raw/qaqc data as needed
  if (!is_all_realtime) {
    # Update years to get in case realtime covers all of last year (rare)
    years_to_get <- date_range[1] |>
      seq(date_range[2], by = "1 days") |>
      lubridate::with_tz(bcgov_tzone) |>
      lubridate::year() |>
      bcgov_determine_years_to_get(qaqc_years)
    # Get data for each year for all desired stations
    archived_data <- years_to_get |>
      handyr::for_each(
        .bind = TRUE,
        .parallel = fast,
        bcgov_get_annual_data,
        stations = stations,
        qaqc_years = qaqc_years,
        variables = variables,
        quiet = quiet
      )
  } else {
    archived_data <- NULL
  }

  # Combine raw/qaqc data with realtime
  stations_data <- archived_data |>
    dplyr::bind_rows(realtime_data)
  if (nrow(stations_data) == 0) {
    stop("No data available for provided stations and date_range")
  }
  if (raw) {
    return(stations_data)
  }

  # Standardize formatting
  stations_data <- stations_data |>
    dplyr::mutate(
      date_utc = .data$DATE_PST |>
        lubridate::ymd_hm(tz = bcgov_tzone) |>
        # Some files use HMS instead of HM for some reason..
        tryCatch(warning = function(w) {
          .data$DATE_PST |> lubridate::ymd_hms(tz = bcgov_tzone)
        }) |>
        lubridate::with_tz("UTC")
    ) |>
    dplyr::select(dplyr::any_of(bcgov_col_names)) |>
    dplyr::filter(
      .data$date_utc |>
        dplyr::between(original_date_range[1], original_date_range[2])
    ) |>
    remove_na_placeholders(na_placeholders = c("", "UNSPECIFIED")) |>
    dplyr::select_if(~ !all(is.na(.))) |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("_instrument"), factor)) |>
    dplyr::arrange(.data$site_id, .data$date_utc) |>
    dplyr::distinct(.data$site_id, .data$date_utc, .keep_all = TRUE)

  if (nrow(stations_data) == 0) {
    stop("No data available for provided stations and date_range")
  }

  if (!fast) {
    stations_data <- stations_data |>
      insert_date_local(stations_meta = known_stations, by = "site_id")
  }

  return(stations_data)
}

# BCgov Constants ---------------------------------------------------------

bcgov_ftp_site <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"
bcgov_tzone <- "Etc/GMT+8" # PST (confirmed: raw/qaqc data files have col "DATE_PST")
bcgov_col_names <- c(
  # Meta
  date_utc = "date_utc", # Added by bcgov_get_annual_data()
  site_id = "EMS_ID",
  quality_assured = "quality_assured", # Added by bcgov_get_annual_data()
  # Particulate Matter
  pm25_1hr = "PM25",
  pm25_1hr_instrument = "PM25_INSTRUMENT",
  pm10_1hr = "PM10",
  pm10_1hr_instrument = "PM10_INSTRUMENT",
  # Ozone
  o3_1hr = "O3",
  o3_1hr_instrument = "O3_INSTRUMENT",
  # Nitrogen Pollutants
  no_1hr = "NO",
  no_1hr_instrument = "NO_INSTRUMENT",
  no2_1hr = "NO2",
  no2_1hr_instrument = "NO2_INSTRUMENT",
  nox_1hr = "NOx",
  nox_1hr_instrument = "NOx_INSTRUMENT",
  # Sulfur Pollutants
  so2_1hr = "SO2",
  so2_1hr_instrument = "SO2_INSTRUMENT",
  trs_1hr = "TRS",
  trs_1hr_instrument = "TRS_INSTRUMENT",
  h2s_1hr = "H2S",
  h2s_1hr_instrument = "H2S_INSTRUMENT",
  # Carbon Monoxide
  co_1hr = "CO",
  co_1hr_instrument = "CO_INSTRUMENT",
  # Met data
  rh_1hr = "HUMIDITY",
  rh_1hr_instrument = "HUMIDITY_INSTRUMENT",
  temp_1hr = "TEMP_MEAN",
  temp_1hr_instrument = "TEMP_MEAN_INSTRUMENT",
  wd_1hr = "WDIR_VECT",
  wd_1hr_instrument = "WDIR_VECT_INSTRUMENT",
  wd_unitvector_1hr = "WDIR_UVEC",
  wd_unitvector_1hr_instrument = "WDIR_UVEC_INSTRUMENT",
  ws_1hr = "WSPD_SCLR",
  ws_1hr_instrument = "WSPD_SCLR_INSTRUMENT",
  ws_vector_1hr = "WSPD_VECT",
  ws_vector_1hr_instrument = "WSPD_VECT_INSTRUMENT",
  precip_1hr = "PRECIP",
  precip_1hr_instrument = "PRECIP_INSTRUMENT",
  snow_1hr = "SNOW",
  snow_1hr_instrument = "SNOW_INSTRUMENT",
  pressure_1hr = "PRESSURE",
  pressure_1hr_instrument = "PRESSURE_INSTRUMENT",
  vapour_pressure_1hr = "VAPOUR",
  vapour_pressure_1hr_instrument = "VAPOUR_INSTRUMENT"
)

# BCgov Helpers ---------------------------------------------------------

# Drop all raw years except the first
bcgov_determine_years_to_get <- function(years, qaqc_years = NULL) {
  if (is.null(qaqc_years)) {
    qaqc_years <- bcgov_get_qaqc_years()
  }
  years <- sort(unique(years))
  is_qaqc_year <- years %in% qaqc_years
  years_to_get <- years[is_qaqc_year] |> # keep all years that are qaqced
    c(years[!is_qaqc_year][1]) # only keep first non-qaqc year
  years_to_get[!is.na(years_to_get)]
}

# BCgov Observations ------------------------------------------------------

bcgov_get_annual_data <- function(
  stations = "all",
  year,
  qaqc_years = NULL,
  variables = "all",
  quiet = FALSE
) {
  # Determine file to get for this year
  if (is.null(qaqc_years)) {
    qaqc_years <- bcgov_get_qaqc_years()
  }
  is_qaqc_year <- year %in% qaqc_years

  # Get data for desired stations for this year
  if (is_qaqc_year) {
    stations_data <- stations |>
      bcgov_get_qaqc_data(year = year, variables = variables, quiet = quiet)
  } else {
    stations_data <- stations |>
      bcgov_get_raw_data(variables = variables, quiet = quiet)
  }

  # Handle no data returned
  if (nrow(stations_data) == 0) {
    stop(paste("No data available for provided station(s) for", year))
  }

  # Add quality assured flag
  stations_data |>
    dplyr::mutate(quality_assured = is_qaqc_year)
}

standardize_units <- function(units) {
  dplyr::case_when(
    units == "% RH" ~ "%",
    units == "\xb0C" ~ "degC",
    units == "Deg." ~ "degrees",
    units == "deg" ~ "degrees",
    units == "deg c" ~ "degC",
    TRUE ~ units
  )
}
