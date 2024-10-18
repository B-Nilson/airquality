# AirNow Data -------------------------------------------------------------

# See https://docs.airnowapi.org/docs/HourlyDataFactSheet.pdf
#' Download air quality station observations from the US EPA "AirNow" platform
#'
#' @param stations (Optional) Either "all" or a character vector specifying AQS IDs for stations to filter data to.
#' If "all" not provided, data for all stations for each hour in `date_range` are still downloaded,
#' but only data for desired stations is returned. Default is "all".
#' @param date_range A datetime vector (or a character vector with UTC dates in "YYYY-MM-DD HH" format) with either 1 or 2 values.
#' Providing a single value will return data for that hour only,
#' whereas two values will return data between (and including) those times.
#' Dates are "backward-looking", so a value of "2019-01-01 01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00".
#' @param raw (Optional) A single logical (TRUE or FALSE) value indicating if
#' raw data files desired (i.e. without a standardized format). Default is FALSE.
#' @param verbose (Optional) A single logical (TRUE or FALSE) value indicating if
#' non-critical messages/warnings should be printed
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
#' [get_airnow_data()] provides an easy way to retrieve these observations using
#' AirNow's station IDs (see [get_airnow_stations()]) and a specified date or date range.
#'
#' Due to the API's file structure, data retrieval time is proportional to the
#' number of hours of data desired, regardless of the number of stations.
#'
#' @return
#' A tibble of hourly observation data for desired station(s) and date range where available.
#' Columns returned will vary depending on available data from station(s).
#'
#' Dates are UTC time and "backward-looking", so a value of "2019-01-01 01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00".
#' @export
#'
#' @family Data Collection
#' @family USA Air Quality
#'
#' @examples
#' \donttest{
#' # Get data for all stations for first 3 hours (UTC) of Jan 2019
#' get_airnow_data("all", c("2019-01-01 01", "2019-01-01 03"))
#'
#' # Get data for two specific stations for first 3 hours (UTC) of Jan 2019
#' get_airnow_data(c("000010102", "000010401"),  c("2019-01-01 01", "2019-01-01 03"))
#'
#' # Get non-standardized data for all stations for first 3 hours (PST) of Jan 2019
#' date_range = lubridate::ymd_h(c("2019-01-01 01", "2019-01-01 03"), tz = "Etc/GMT+8")
#' get_airnow_data("all", date_range, raw = TRUE)
#' }
get_airnow_data = function(stations = "all", date_range, raw = FALSE, verbose = TRUE){
  # Output citation message to user
  if(verbose) data_citation("AirNow")

  ## Handle date_range inputs
  min_date = lubridate::ymd_h("2014-01-01 01", tz = "UTC")
  max_date = lubridate::floor_date(lubridate::with_tz(Sys.time(), "UTC"), "hours")
  date_range = handle_date_range(date_range, min_date, max_date)
  # Data may be missing for most recent hourly files - depending on data transfer delays
  # Warn user of this if requesting data in past 48 hours, especially if last 55 minutes
  if(max(date_range) - max_date > lubridate::hours(-48)){ # if date_range in past 48 hours
    if(max(date_range) - max_date > lubridate::minutes(-55)){ # if date_range in past 55 minutes
      if(verbose) warning(paste("The current hour AirNow files is updated twice per hour",
                    "(at 25 and 55 minutes past the hour) or more frequently if possible.",
                    "All hourly files for the preceding 48 hours will be updated every hour",
                    "to ensure data completeness and quality.",
                    "\n\tData may be missing from stations for any hours in the past 48, especially for the current hour."))
    }else{ # if date_range in past 48 hours but not past 55 minutes
      if(verbose) warning(paste("All hourly AirNow files for the preceding 48 hours will be updated every hour",
                    "to ensure data completeness and quality.",
                    "\n\tData may be missing from stations for any hours in the past 48, especially for the current hour."))
    }
  }

  # Get all stations during period
  known_stations =  seq(date_range[1], date_range[2], "25 days") |>
    get_airnow_stations()
  # If specific stations desired
  if(! "all" %in% stations){
    # Handle if any/all don't exist in meta data
    check_stations_exist(stations, known_stations$site_id, "AirNow")
  }

  ## Main ---
  # Make hourly file paths
  date_range = sort(date_range) # Ensure date_range is correct order
  dates = seq(date_range[1], date_range[2], "1 hours") +
    lubridate::hours(-1) # files are forward looking averages
  airnow_paths = make_airnow_filepaths(dates)

  airnow_data = lapply(airnow_paths, \(pth){
    # Try downloading data for each hour, returning NULL if failed (no file)
    on_error(return = NULL, read_data(file = pth))}) |>
    # Combine rowise into a single dataframe
    dplyr::bind_rows() |>
    # Set the file header
    stats::setNames(
      c('date','time','siteID','site',
        'tz_offset','param','unit','value','operator'))

  # If no data (should not happen unless AirNow is offline and requesting current data)
  if(nrow(airnow_data) == 0){
    # Error and quit here
    stop(paste("No data available for provided date range.",
            "Ensure `date_range` is valid and AirNow is not offline",
            "(see: https://www.airnowtech.org/ for AirNow status)."))
  }

  # Basic formatting
  airnow_data = airnow_data |>
    # Drop duplicate rows if any
    unique() |>
    # Datetime formatting
    dplyr::mutate(
      # Join date and time columns, convert to datetime
      date = lubridate::mdy_hm(paste(.data$date, .data$time), tz = "UTC") +
        lubridate::hours(1)
    ) # from forward -> backward looking averages,

  # Filter for desired stations if "all" not supplied
  if(! "all" %in% stations){
    airnow_data = dplyr::filter(airnow_data, .data$siteID %in% stations)
  }

  # If raw data desired, end function and return data
  if(raw) return(airnow_data)
  
  if(nrow(airnow_data) == 0) {
    # Error and quit here
    stop("No data available for desired stations during speciifed date range.")
  }
  ## Otherwise
  # Convert from long format to wide format ("param_unit" column for each param/unit)
  airnow_data = airnow_data |>
    # Convert date_local to local time
    dplyr::left_join(known_stations |> dplyr::select(siteID = "site_id", "tz_local"), by = "siteID") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      date_local = lubridate::with_tz(.data$date, as.character(.data$tz_local)) |>
                    format("%F %H:%M %z")) |>
    dplyr::ungroup() |>
    # drop now erroneous time and tz_offset columns
    dplyr::select(-"time", -"tz_offset", -"tz_local") |>
    # long to wide
    tidyr::pivot_wider(names_from = c("param", "unit"), values_from = "value")

  # Standardize units if needed
  if("BARPR_MILLIBAR" %in% names(airnow_data))
    airnow_data$barpr_1hr_kpa = airnow_data$BARPR_MILLIBAR / 10
  if("CO_PPM" %in% names(airnow_data))
    airnow_data$co_1hr_ppb = airnow_data$CO_PPM * 1000

  # Standardize column names/order
  airnow_data = airnow_data |>
    # All AirNow data is not QA/QC'ed - mark it as such
    dplyr::mutate(quality_assured = FALSE) |>
    # Rename and select desired columns
    standardize_colnames(airnow_col_names)

  return(airnow_data)
}

# TODO: document
get_airnow_stations = function(dates = Sys.time(), use_sf = FALSE){
  # Make path to each supplied hours meta file
  dates = sort(dates, decreasing = TRUE) # Newest first
  dates = dates - lubridate::days(1) # in case current days file not made yet
  airnow_paths = make_airnow_metapaths(dates)

  # For each date
  stations = lapply(
      names(airnow_paths), \(d){
        p = airnow_paths[names(airnow_paths) == as.character(d)]
        # Download meta file, returning NULL if failed
        on_error(return = NULL,
          read_data(file = p) |>
            # Flag file date for later
            dplyr::mutate(file_date = d))
      }) |>
    # Combine rowise into a single dataframe
    dplyr::bind_rows() |>
    # Set header names
    stats::setNames(
      c('siteID', 'param', 'site_location_code', 'site', 'status', 'operator_code',
        'operator', 'usa_region', 'lat', 'lon', 'elev', 'tz_offset', 'country',
        'UNKNOWN', 'UNKNOWN', 'location_code', 'location', 'UNKNOWN', 'region',
        'UNKNOWN', 'city', 'UNKNOWN', 'UNKNOWN', "file_date")) |>
    # Choose and reorder colummns, standardizing names
    dplyr::select(
      site_id = 'siteID', site_name = "site", city = 'city',
      lat = 'lat', lng = 'lon', elev = 'elev',
      status = 'status', operator = 'operator',
      tz_offset = 'tz_offset', as_of = "file_date") |>
    # Replace placeholders with proper NA values
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character), \(x) ifelse(x %in% c("N/A", "na", "n/a"), NA, x))) |>
    # Drop duplicated entries
    dplyr::distinct(dplyr::across(-"as_of"), .keep_all = TRUE) |>
    # Lookup local timezones
    dplyr::mutate(tz_local = get_timezone(.data$lng, .data$lat))

  # Convert to spatial if desired
  if(use_sf) stations = sf::st_as_sf(stations, coords = c("lng", "lat"))

  return(stations)
}

## AirNow Helpers ----------------------------------------------------------

airnow_col_names = c(
  # Meta
  date_utc = "date",
  date_local = "date_local", # Added by get_airnow_data()
  site_id = "siteID",
  site_name = "site",
  quality_assured = "quality_assured", # Added by get_airnow_data()
  # Particulate Matter
  pm25_1hr_ugm3 = "PM2.5_UG/M3",
  pm10_1hr_ugm3 = "PM10_UG/M3",
  bc_1hr_ugm3 = "BC_UG/M3",
  # Ozone
  o3_1hr_ppb = "OZONE_PPB",
  # Nitrogen Pollutants
  no_1hr_ppb = "NO_PPB",
  no2t_1hr_ppb = "NO2T_PPB", # t == "true measure"
  no2_1hr_ppb = "NO2_PPB",
  no2y_1hr_ppb = "NO2Y_PPB", # y == "reactive"
  nox_1hr_ppb = "NOX_PPB",
  noy_1hr_ppb = "NOY_PPB", # y == "reactive"
  no3_1hr_ugm3 = "NO3_UG/M3",
  # Sulfur Pollutants
  so4_1hr_ugm3 = "SO4_UG/M3",
  so2_1hr_ppb = "SO2_PPB",
  so2t_1hr_ppb = "SO2T_PPB", # t == "trace"
  # Carbon Monoxide
  co_1hr_ppb = "co_ppb", # Converted from ppm by get_airnow_data()
  cot_1hr_ppb = "COT_PPB", # t == "trace"
  # Met data
  rh_1hr_percent = "RHUM_PERCENT",
  t_1hr_celcius = "TEMP_C",
  wd_1hr_degrees = "WD_DEGREES",
  ws_1hr_ms = "WS_MS",
  precip_1hr_mm = "PRECIP_MM",
  pressure_1hr_kpa = "barpr_kpa", # Converted from mb by get_airnow_data()
  solar_1hr_wm2 = "SRAD_WATTS/M2"
)

make_airnow_filepaths = function(dates){
  dates = lubridate::with_tz(dates, "UTC")
  airnow_site = 'https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow'
  airnow_files = paste0("HourlyData_", format(dates, "%Y%m%d%H.dat"))
  file.path(airnow_site, lubridate::year(dates),
            format(dates, "%Y%m%d"), airnow_files)
}

make_airnow_metapaths = function(dates){
  min_date = lubridate::ymd_h("2016-06-21 00", tz = "UTC")
  if(any(dates < min_date)){
    warning(paste("Metadata files on AirNow only available from",
            format(min_date, "%F %H (UTC)"), "onwards.",
            "Station information returned may be inaccurate for dates before this."))
    dates = c(min_date, dates[dates > min_date])
  }
  airnow_site = 'https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow'
  file_name = "monitoring_site_locations.dat"
  paths = file.path(airnow_site, lubridate::year(dates),
            format(dates, "%Y%m%d"), file_name)
  names(paths) = dates
  return(paths)
}
