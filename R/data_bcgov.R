#' Download air quality station metadata from the British Columbia (Canada) Government
#'
#' @param years (Optional) one or more integer values indicating the year(s) to get metadata for.
#'   Default is the current year.
#' @param use_sf (Optional) a single logical (TRUE/FALSE) value indicating whether or not to return a spatial object. using the `sf` package
#'
#' @description
#' Air pollution monitoring in Canada is done by individual Provinces/Territories,
#' primarily as a part of the federal National Air Pollution Surveillance (NAPS) program.
#' The Province of British Columbia hosts it's air quality metadata
#' through a public FTP site.
#'
#' [get_bcgov_stations()] provides an easy way to retrieve this metadata (typically to determine station id's to pass to `get_bcgov_data()`)
#'
#' @seealso [get_bcgov_data()]
#' @return
#' A tibble of metadata for British Columbia air quality monitoring stations.
#'
#' @family Data Collection
#' @family Canadian Air Quality
#'
#' @export
#' @examples
#' \donttest{
#' # Normal usage
#' get_bcgov_stations()
#' # if spatial object required
#' get_bcgov_stations(use_sf = TRUE)
#' # if data for past/specific years required
#' get_bcgov_stations(years = 1998:2000)
#' }
# stations = get_bcgov_stations(years = 1998:2024)
get_bcgov_stations <- function(years = lubridate::year(Sys.time()), use_sf = FALSE) {
  # Get station metadata for all requested years
  qaqc_years <- get_bcgov_qaqc_years()
  stations <- years |>
    determine_years_to_get(qaqc_years) |>
    lapply(\(year) get_annual_bcgov_stations(year, qaqc_years)) |>
    dplyr::bind_rows()

  # Standardize formatting
  col_names <- c(
    site_id = "EMS_ID", site_name = "STATION_NAME",
    city = "CITY", lat = "LAT", lng = "LONG", elev = "ELEVATION",
    date_created = "OPENED", date_removed = "CLOSED"
  )
  stations <- stations |>
    # Fix reversed lat/lng entries
    dplyr::mutate(
      lat2 = ifelse(.data$LAT |> dplyr::between(45, 60), .data$LAT, .data$LONG),
      LONG = ifelse(.data$LAT |> dplyr::between(45, 60), .data$LONG, -.data$LAT),
      LAT = .data$lat2
    ) |>
    # Choose and rename columns
    standardize_colnames(col_names = col_names) |>
    dplyr::filter(!is.na(.data$lat), !is.na(.data$lng)) |>
    dplyr::arrange(.data$site_name) |>
    unique() |>
    # Replace blank values with NA
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      \(x) ifelse(x == "", NA, x)
    )) |>
    # Convert date_created and date_removed to date objects
    dplyr::mutate(dplyr::across(
      c("date_created", "date_removed"),
      \(x) lubridate::ymd(stringr::str_sub(x, end = 10))
    )) |>
    dplyr::mutate(tz_local = get_timezone(.data$lng, .data$lat))

  # Convert to spatial if desired
  if (use_sf) {
    stations <- stations |> sf::st_as_sf(coords = c("lng", "lat"))
  }

  return(stations)
}

#' Download air quality station observations from the British Columbia (Canada) Government
#'
#' @param stations A character vector of one or more station IDs (BC EMS IDs) to try and get data desired for (see [get_bcgov_stations()]).
#' @param date_range A datetime vector (or a character vector with UTC dates in "YYYY-MM-DD HH" format) with either 1 or 2 values.
#' Providing a single value will return data for that hour only,
#' whereas two values will return data between (and including) those times.
#' Dates are "backward-looking", so a value of "2019-01-01 01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00".
#' @param raw (Optional) A single logical (TRUE or FALSE) value indicating
#' if raw data files desired (i.e. without a standardized format). Default is FALSE.
#' @param verbose (Optional) A single logical (TRUE or FALSE) value indicating if
#' non-critical messages/warnings should be printed
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
get_bcgov_data <- function(stations, date_range, raw = FALSE, verbose = TRUE) {
  # TODO: handle multiple instruments for same pollutant
  # Output citation message to user
  if (verbose) data_citation("BCgov")

  # Handle date_range inputs
  qaqc_years <- get_bcgov_qaqc_years()
  min_date <- min(qaqc_years) |>
    paste("01-01 01") |>
    lubridate::ymd_h(tz = bcmoe_tzone)
  max_date <- Sys.time() |> lubridate::floor_date("hours")
  date_range <- date_range |> handle_date_range(min_date, max_date)

  # Get all years in desired date range and drop all but the first in qaqc_years
  years_to_get <- date_range[1] |>
    seq(date_range[2], by = "1 days") |>
    lubridate::with_tz(bcmoe_tzone) |>
    lubridate::year() |>
    determine_years_to_get(qaqc_years)

  # Get all stations during period
  known_stations <- years_to_get |>
    lapply(get_bcgov_stations) |>
    dplyr::bind_rows() |>
    # TODO: Handle this in get_bcgov_stations?
    dplyr::arrange(dplyr::desc(.data$date_created)) |>
    dplyr::filter(!duplicated(.data$site_id))

  # Handle if any/all don't exist in meta data
  stations <- stations |>
    check_stations_exist(known_stations$site_id, source = "the BC FTP site")

  # Get data for each year for all desired stations
  stations_data <- years_to_get |>
    lapply(\(year) get_annual_bcgov_data(stations, year, qaqc_years)) |>
    dplyr::bind_rows()
  if (nrow(stations_data) == 0) {
    stop("No data available for provided stations and date_range")
  }

  # Standardize formatting
  stations_data <- stations_data |>
    dplyr::filter(.data$date_utc |>
      dplyr::between(date_range[1], date_range[2])) |>
    dplyr::filter(!duplicated(.data$date_utc), .by = "EMS_ID") |>
    standardize_colnames(bcmoe_col_names, raw = raw) |>
    dplyr::mutate(dplyr::across(-(1:2), \(x) ifelse(x == "", NA, x))) |>
    # Output as tibble
    tibble::as_tibble()

  if (nrow(stations_data) & !raw) {
    stations_data <- stations_data |>
      insert_date_local(stations_meta = known_stations)
  } else {
    if (!raw) stop("No data available for provided stations and date_range")
  }

  return(stations_data)
}

## BC MoE Helpers ---------------------------------------------------------

bcmoe_tzone <- "Etc/GMT+8"

bcmoe_col_names <- c(
  # Meta
  date_utc = "date_utc", # Added by get_annual_bcgov_data()
  site_id = "EMS_ID",
  site_name = "STATION_NAME",
  quality_assured = "quality_assured", # Added by get_annual_bcgov_data()
  # Particulate Matter
  pm25_1hr_ugm3 = "PM25",
  pm25_1hr_ugm3_instrument = "PM25_INSTRUMENT",
  pm10_1hr_ugm3 = "PM10",
  pm10_1hr_ugm3_instrument = "PM10_INSTRUMENT",
  # Ozone
  o3_1hr_ppb = "O3",
  o3_1hr_ppb_instrument = "O3_INSTRUMENT",
  # Nitrogen Pollutants
  no_1hr_ppb = "NO",
  no_1hr_ppb_instrument = "NO_INSTRUMENT",
  no2_1hr_ppb = "NO2",
  no2_1hr_ppb_instrument = "NO2_INSTRUMENT",
  nox_1hr_ppb = "NOx",
  nox_1hr_ppb_instrument = "NOx_INSTRUMENT",
  # Sulfur Pollutants
  so2_1hr_ppb = "SO2",
  so2_1hr_ppb_instrument = "SO2_INSTRUMENT",
  trs_1hr_ppb = "TRS",
  trs_1hr_ppb_instrument = "TRS_INSTRUMENT",
  h2s_1hr_ppb = "H2S",
  h2s_1hr_ppb_instrument = "H2S_INSTRUMENT",
  # Carbon Monoxide
  co_1hr_ppb = "CO",
  co_1hr_ppb_instrument = "CO_INSTRUMENT",
  # Met data
  rh_1hr_percent = "HUMIDITY",
  rh_1hr_percent_instrument = "HUMIDITY_INSTRUMENT",
  t_1hr_celcius = "TEMP_MEAN",
  t_1hr_celcius_instrument = "TEMP_MEAN_INSTRUMENT",
  wd_1hr_degrees = "WDIR_VECT",
  wd_1hr_degrees_instrument = "WDIR_VECT_INSTRUMENT",
  ws_1hr_ms = "WSPD_SCLR",
  ws_1hr_ms_instrument = "WSPD_SCLR_INSTRUMENT",
  precip_1hr_mm = "PRECIP",
  precip_1hr_mm_instrument = "PRECIP_INSTRUMENT",
  snowDepth_1hr_cm = "SNOW",
  snowDepth_1hr_cm_instrument = "SNOW_INSTRUMENT",
  pressure_1hr_kpa = "PRESSURE", # TODO: Ensure pressure proper units ....
  pressure_1hr_kpa_instrument = "PRESSURE_INSTRUMENT",
  vapourPressure_1hr_kpa = "VAPOUR",
  vapourPressure_1hr_kpa_instrument = "VAPOUR_INSTRUMENT" # ,
)

# Checks the years in the QA/QC'ed data archive for BC MoE data
# (usually 1-2 years out of date)
get_bcgov_qaqc_years <- function() {
  ftp_site_qaqc <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Archieved/"
  # Load file/dir details and extract names
  qaqc_dirs <- ftp_site_qaqc |>
    readLines() |>
    stringr::str_split("\\s", simplify = T)
  qaqc_dirs <- qaqc_dirs[, ncol(qaqc_dirs)]
  # Extract years from file/dir names
  years <- suppressWarnings(qaqc_dirs |> # suppress 'NAs introduced due to coercion' warning
    stringr::str_remove("STATION_DATA_") |>
    as.numeric())
  years[!is.na(years)]
}

get_annual_bcgov_data <- function(stations, year, qaqc_years = NULL) {
  # Where BC MoE AQ/Met data are stored
  ftp_site <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"
  qaqc_url <- ftp_site |> # "Archieved" lol
    paste0("Archieved/STATION_DATA_{year}/{station}.csv")
  raw_url <- ftp_site |> # actually since qa/qc to date, not just this year
    paste0("Hourly_Raw_Air_Data/Year_to_Date/STATION_DATA/{station}.csv")

  # Determine file to get for this year
  if (is.null(qaqc_years)) qaqc_years <- get_bcgov_qaqc_years()
  if (year %in% qaqc_years) {
    data_url <- qaqc_url |>
      stringr::str_replace("\\{year\\}", as.character(year))
  } else {
    data_url <- raw_url
  }

  # Get each stations data for this year
  stations_data <- data_url |>
    stringr::str_replace("\\{station\\}", stations) |>
    lapply_and_bind(\(p) suppressWarnings(on_error(
      return = NULL,
      read_data(file = p, colClasses = c(
        DATE_PST = "character",
        EMS_ID = "character", STATION_NAME = "character"
      ))
    )))

  if (nrow(stations_data) == 0) {
    stop(paste("No data available for provided stations for", year))
  }
  # Standardize formatting
  stations_data |>
    dplyr::mutate(
      DATE_PST = tryCatch(
        .data$DATE_PST |> lubridate::ymd_hms(tz = bcmoe_tzone),
        warning = \(...) .data$DATE_PST |> lubridate::ymd_hm(tz = bcmoe_tzone)
      ),
      date_utc = lubridate::with_tz(.data$DATE_PST, "UTC"),
      DATE_PST = format(.data$DATE_PST, "%F %H:%M -8"),
      quality_assured = data_url != raw_url
    ) |>
    dplyr::relocate("date_utc", .before = "DATE_PST") |>
    dplyr::select(-"DATE", -"TIME")
}

get_annual_bcgov_stations <- function(year, qaqc_years = NULL) {
  bc_ftp_site <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"
  qaqc_url <- paste0(bc_ftp_site, "AnnualSummary/{year}/")
  raw_url <- paste0(bc_ftp_site, "Hourly_Raw_Air_Data/Year_to_Date/")
  stations_file <- "bc_air_monitoring_stations.csv"

  # Determine file to get for this year
  if (is.null(qaqc_years)) qaqc_years <- get_bcgov_qaqc_years()
  if (year %in% qaqc_years) {
    data_url <- qaqc_url |>
      stringr::str_replace("\\{year\\}", as.character(year))
  } else {
    data_url <- raw_url
  }
  on_error(
    return = NULL,
    read_data(
      file = paste0(data_url, stations_file), data.table = FALSE,
      colClasses = c("OPENED" = "character", "CLOSED" = "character")
    )
  )
}

# Drop all QAQC years except the first
determine_years_to_get <- function(years, qaqc_years) {
  years <- sort(unique(years))
  is_qaqc_year <- years %in% qaqc_years # find years that are qa/qc'ed years
  years_to_get <- c(years[is_qaqc_year], years[!is_qaqc_year][1])
  # drop NA from case when no non qa/qced years provided
  years_to_get[!is.na(years_to_get)]
}
