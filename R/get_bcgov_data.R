#' Download air quality station observations from the British Columbia (Canada) Government
#'
#' @param stations (Optional). 
#'   A character vector of one or more station IDs (BC EMS IDs) to try and get data desired for (see [get_bcgov_stations()]).
#'   Default is "all", i.e. all available stations.
#' @inheritParams get_airnow_data
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
  stations = "all",
  date_range = "now",
  variables = "all",
  raw = FALSE,
  fast = FALSE,
  quiet = FALSE
) {
  stopifnot(is.character(stations), length(stations) > 0)
  stopifnot(is.logical(raw), length(raw) == 1)
  stopifnot(is.logical(fast), length(fast) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)

  data_citation("BCgov", quiet = quiet)

  # Constants/setup
  bcgov_tzone <- "Etc/GMT+8" # PST (confirmed: raw/qaqc data files have col "DATE_PST")
  qaqc_years <- bcgov_get_qaqc_years()
  allowed_date_range <- min(qaqc_years) |>
    paste0("-01-01 01:00:00")
  allowed_date_range[2] <- "now"
  realtime_start <- handyr::check_date_range("now") -
    lubridate::days(30)

  # Handle date_range input
  date_range <- date_range |>
    handyr::check_date_range(
      within = allowed_date_range,
      now_time_step = "1 hours",
      tz = bcgov_tzone
    )

  # Filter search to existing stations only
  if (!fast) {
    known_stations <- date_range |>
      get_bcgov_stations(use_sf = FALSE, quiet = quiet) |> 
      suppressWarnings() # in case getting stations prior to 1998
    if (!"all" %in% stations) {
      stations <- stations |>
        check_stations_exist(
          known_stations = known_stations$site_id,
          source = "the BC FTP site"
        )
    }
  } else {
    known_stations <- NULL
  }

  # Get realtime data if needed
  need_realtime <- any(date_range > realtime_start)
  if (need_realtime) {
    realtime_data <- stations |>
      bcgov_get_raw_data(
        variables = variables,
        quiet = quiet,
        mode = "realtime"
      ) |>
      handyr::on_error(.return = NULL, .warn = "Could not get realtime data:")
  } else {
    realtime_data <- NULL
    is_all_realtime <- FALSE
  }

  # Alter date_range to account for retrieved realtime data
  date_range_new <- date_range
  if (!is.null(realtime_data)) {
    date_range_new[2] <- realtime_data |>
      dplyr::summarise(min_date = min(.data$date_utc)) |>
      dplyr::pull("min_date")
  }
  is_all_realtime <- date_range_new[1] >= date_range_new[2]

  # Get raw/qaqc data as needed
  if (!is_all_realtime) {
    archived_data <- date_range_new |>
      bcgov_determine_years_to_get(qaqc_years = qaqc_years) |>
      handyr::for_each(
        .bind = TRUE,
        .parallel = fast,
        .show_progress = !quiet,
        bcgov_get_annual_data,
        stations = stations,
        variables = variables,
        qaqc_years = qaqc_years,
        quiet = quiet
      )
  } else {
    archived_data <- NULL
  }

  # Combine and standardize formatting
  archived_data |>
    dplyr::bind_rows(realtime_data) |>
    standardize_data_format(
      date_range = date_range,
      known_stations = known_stations,
      fast = fast,
      raw = raw
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(names(.bcgov_columns$instruments)),
      factor
    ))
}

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
    snow_1hr = "SNOW", # mm of SWE
    snowdepth_1hr = "SNOWDEPTH", # cm of snow
    pressure_1hr = "PRESSURE",
    vapour_pressure_1hr = "VAPOUR"
  )
)

# Also grab any matching _instrument columns for reference
.bcgov_columns$instruments <- .bcgov_columns$values |>
  paste0("_INSTRUMENT") |>
  setNames(.bcgov_columns$values |> paste0("_instrument"))

bcgov_get_qaqc_years <- function() {
  bcgov_ftp_site <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"
  qaqc_directory <- bcgov_ftp_site |> paste0("/AnnualSummary/")
  # Get directories in QAQC directory
  qaqc_dirs <- qaqc_directory |>
    readLines() |>
    stringr::str_subset("<DIR>")
  # Extract years from dir names
  qaqc_dirs |>
    stringr::str_extract("\\d{4}$") |>
    as.numeric()
}

# Returns all years in qaqc_years and one additional year that is not qaqc (if present)
bcgov_determine_years_to_get <- function(date_range, qaqc_years = NULL) {
  bcgov_tzone <- "Etc/GMT+8" # PST (confirmed: raw/qaqc data files have col "DATE_PST")

  # Get years in date_range
  date_range <- handyr::check_date_range(date_range, tz = bcgov_tzone)
  years <- date_range[1] |>
    seq(date_range[2], by = "1 days") |>
    lubridate::year() |>
    unique() |>
    sort()

  # Determine years with qaqc data if not provided
  if (is.null(qaqc_years)) {
    qaqc_years <- bcgov_get_qaqc_years()
  }
  is_qaqc_year <- years %in% qaqc_years

  # Keep all years that are qaqced, and the first non-qaqc year
  years_to_get <- years[is_qaqc_year] |>
    c(years[!is_qaqc_year][1])
  years_to_get[!is.na(years_to_get)]
}

# Workhorse function for getting annual raw/qaqc data as needed
bcgov_get_annual_data <- function(
  stations = "all",
  variables = "all",
  year,
  qaqc_years = NULL,
  quiet = FALSE
) {
  # Determine mode (raw or qaqc) for this year
  if (is.null(qaqc_years)) {
    qaqc_years <- bcgov_get_qaqc_years()
  }
  is_qaqc_year <- year %in% qaqc_years

  # Get data for desired stations for this year
  if (is_qaqc_year) {
    # Get qaqc data
    stations_data <- stations |>
      bcgov_get_qaqc_data(
        years = year,
        variables = variables,
        quiet = TRUE
      )
  } else {
    # Get raw data
    stations_data <- stations |>
      bcgov_get_raw_data(
        variables = variables,
        quiet = quiet
      )
  }

  # Handle no data returned
  if (nrow(stations_data) == 0) {
    stop(paste("No data available for provided station(s) for", year))
  }

  # Add quality assured flag
  stations_data |>
    dplyr::mutate(quality_assured = is_qaqc_year)
}
