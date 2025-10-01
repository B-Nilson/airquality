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

  # Constants/setup
  qaqc_years <- bcgov_get_qaqc_years()
  allowed_date_range <- min(qaqc_years) |>
    paste("01-01 01") |>
    lubridate::ymd_h(tz = bcgov_tzone)
  allowed_date_range[2] <- lubridate::now(tz = bcgov_tzone) |>
    lubridate::floor_date("hours")
  realtime_start <- lubridate::now(tz = bcgov_tzone) -
    lubridate::days(30)
  data_citation("BCgov", quiet = quiet)

  # Handle date_range inputs
  date_range <- date_range |>
    handle_date_range(within = c(min_date, max_date))

  # Handle variables input
  all_variables <- names(.bcgov_columns$values) |>
    stringr::str_remove("_1hr")
  variables <- variables |>
    standardize_input_vars(all_variables)

  # Filter to existing stations only
  # TODO: what if "all" in stations?
  if (!fast) {
    known_stations <- date_range |>
      bcgov_determine_years_to_get(qaqc_years) |>
      get_bcgov_stations(use_sf = FALSE, quiet = quiet)
    stations <- stations |>
      check_stations_exist(
        known_stations = known_stations$site_id,
        source = "the BC FTP site"
      )
  }

  # Get realtime data if needed
  need_realtime <- any(date_range > realtime_start)
  if (need_realtime) {
    realtime_data <- stations |>
      # TODO: remove old files before loading in
      bcgov_get_raw_data(
        variables = variables,
        quiet = quiet,
        mode = "realtime"
      ) |>
      handyr::on_error(.return = NULL)
  } else {
    realtime_data <- NULL
    is_all_realtime <- FALSE
  }

  # Alter date_range to account for retrieved realtime data
  date_range_new <- date_range
  if (!is.null(realtime_data)) {
    first_realtime_date <- realtime_data |>
      dplyr::group_by(site_id) |>
      dplyr::summarise(min_date = min(date_utc)) |>
      dplyr::pull(min_date) |>
      max()
    date_range_new[2] <- first_realtime_date
    is_all_realtime <- date_range_new[1] >= date_range_new[2]
  }

  # Get raw/qaqc data as needed
  if (!is_all_realtime) {
    archived_data <- date_range_new |>
      bcgov_determine_years_to_get(qaqc_years) |>
      handyr::for_each(
        .bind = TRUE,
        .parallel = fast,
        .show_progress = !quiet,
        bcgov_get_annual_data,
        stations = stations,
        qaqc_years = qaqc_years,
        variables = variables,
        quiet = quiet
      )
  } else {
    archived_data <- NULL
  }

  # Combine and standardize formatting
  list(archived_data, realtime_data) |>
    dplyr::bind_rows() |>
    standardize_data_format(
      date_range = date_range,
      known_stations = all_stations,
      fast = fast,
      raw = raw
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(names(.bcgov_columns$instruments)),
      factor
    ))
}

format_bcgov_raw_data <- function(
  raw_data,
  desired_cols,
  mode = "stations"
) {
  if (nrow(raw_data) == 0) {
    stop("No data available before reformatting.")
  }

  if (nrow(raw_data) == 0) {
    return(raw_data)
  }
  if (mode == "variables") {
    return(
      bcgov_format_qaqc_data(raw_data, use_rounded_value = TRUE)
    )
  }

  # Get column names and insert unit columns if needed
  meta_cols <- .bcgov_columns$meta[
    names(.bcgov_columns$meta) != "quality_assured"
  ]
  value_cols <- names(raw_data)[
    names(raw_data) %in% .bcgov_columns$values
  ]
  unit_cols <- names(raw_data)[
    names(raw_data) %in% .bcgov_columns$units
  ]
  if (mode == "stations") {
    instrument_cols <- names(raw_data)[
      names(raw_data) %in% .bcgov_columns$instruments
    ]
  } else {
    instrument_cols <- character(0)
    # Insert default units as unit columns as no units provided
    # TODO: confirm units are correct here
    for (i in 1:length(unit_cols)) {
      is_value_col <- .bcgov_columns$values %in% value_cols[i]
      raw_data[[unit_cols[i]]] <- default_units[
        names(.bcgov_columns$values)[is_value_col]
      ]
    }
  }

  # Assign units to value columns
  for (i in 1:length(unit_cols)) {
    default_unit <- default_units[
      names(.bcgov_columns$values[
        .bcgov_columns$values %in% c(value_cols[i], names(value_cols[i])) # TODO: is this needed?
      ])
    ]
    raw_data <- raw_data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(value_cols[i]),
          \(x) {
            as.numeric(x) |>
              suppressWarnings() |> # NAs introduced by coercion
              convert_units(
                in_unit = .data[[unit_cols[i]]][1],
                out_unit = default_unit
              )
          }
        )
      )
  }

  formatted <- raw_data |>
    dplyr::mutate(
      quality_assured = FALSE,
      date_utc = .data$DATE_PST |>
        lubridate::ymd_hm(tz = bcgov_tzone) |>
        # Some files use HMS instead of HM for some reason..
        tryCatch(warning = function(w) {
          .data$DATE_PST |> lubridate::ymd_hms(tz = bcgov_tzone)
        }) |>
        lubridate::with_tz("UTC")
    ) |>
    dplyr::select(dplyr::any_of(desired_cols)) |>
    remove_na_placeholders(na_placeholders = c("", "UNSPECIFIED")) |>
    drop_missing_obs_rows(where_fn = \(x) x %in% names(.bcgov_columns$values))

  if (nrow(formatted) == 0) {
    stop("No data available after reformatting.")
  }
  return(formatted)
}

# BCgov Constants ---------------------------------------------------------

bcgov_ftp_site <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"
bcgov_tzone <- "Etc/GMT+8" # PST (confirmed: raw/qaqc data files have col "DATE_PST")
.bcgov_columns <- list(
  meta = c(
    date_utc = "date_utc", # Added by bcgov_get_annual_data()
    site_id = "EMS_ID",
    quality_assured = "quality_assured" # Added by bcgov_get_annual_data()
  ),
  values = c(
    # Particulate Matter
    pm25_1hr = "PM25",
    pm10_1hr = "PM10",
    # Ozone
    o3_1hr = "O3",
    # Nitrogen Pollutants
    no_1hr = "NO",
    no2_1hr = "NO2",
    nox_1hr = "NOx",
    # Sulfur Pollutants
    so2_1hr = "SO2",
    trs_1hr = "TRS",
    h2s_1hr = "H2S",
    # Carbon Monoxide
    co_1hr = "CO",
    # Met data
    rh_1hr = "HUMIDITY",
    temp_1hr = "TEMP_MEAN",
    wd_1hr = "WDIR_VECT",
    wd_unitvector_1hr = "WDIR_UVEC",
    ws_1hr = "WSPD_SCLR",
    ws_vector_1hr = "WSPD_VECT",
    precip_1hr = "PRECIP",
    snow_1hr = "SNOW",
    pressure_1hr = "PRESSURE",
    vapour_pressure_1hr = "VAPOUR"
  ),
  instruments = c(
    # Particulate Matter
    pm25_1hr_instrument = "PM25_INSTRUMENT",
    pm10_1hr_instrument = "PM10_INSTRUMENT",
    # Ozone
    o3_1hr_instrument = "O3_INSTRUMENT",
    # Nitrogen Pollutants
    no_1hr_instrument = "NO_INSTRUMENT",
    no2_1hr_instrument = "NO2_INSTRUMENT",
    nox_1hr_instrument = "NOx_INSTRUMENT",
    # Sulfur Pollutants
    so2_1hr_instrument = "SO2_INSTRUMENT",
    trs_1hr_instrument = "TRS_INSTRUMENT",
    h2s_1hr_instrument = "H2S_INSTRUMENT",
    # Carbon Monoxide
    co_1hr_instrument = "CO_INSTRUMENT",
    # Met data
    rh_1hr_instrument = "HUMIDITY_INSTRUMENT",
    temp_1hr_instrument = "TEMP_MEAN_INSTRUMENT",
    wd_1hr_instrument = "WDIR_VECT_INSTRUMENT",
    wd_unitvector_1hr_instrument = "WDIR_UVEC_INSTRUMENT",
    ws_1hr_instrument = "WSPD_SCLR_INSTRUMENT",
    ws_vector_1hr_instrument = "WSPD_VECT_INSTRUMENT",
    precip_1hr_instrument = "PRECIP_INSTRUMENT",
    snow_1hr_instrument = "SNOW_INSTRUMENT",
    pressure_1hr_instrument = "PRESSURE_INSTRUMENT",
    vapour_pressure_1hr_instrument = "VAPOUR_INSTRUMENT"
  )
)

# BCgov Helpers ---------------------------------------------------------

# Drop all raw years except the first
bcgov_determine_years_to_get <- function(date_range, qaqc_years = NULL) {
  years <- date_range[1] |>
    seq(date_range[2], by = "1 days") |>
    lubridate::with_tz(bcgov_tzone) |>
    lubridate::year() |>
    unique() |>
    sort()
  if (is.null(qaqc_years)) {
    qaqc_years <- bcgov_get_qaqc_years()
  }
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
