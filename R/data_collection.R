# TODO: Add data citation message to each main function call
# TODO: Add optional dependence on future.apply - if installed, allow for parallel data collection (responsibly)
# TODO: Add station localization (country - prov/terr/state - county? region? - nearest community?)

# [Canada] BC MoE Data ----------------------------------------------------

#' Download air quality station observations from the British Columbia (Canada) Government
#'
#' @param stations A character vector of one or more station IDs (BC EMS IDs) identifying stations data desired for. See also: get_bc_stations()
#' @param date_range A datetime vector with two values indicating the start and end dates of desired data window. Will be converted to PST.
#' @param raw (Optional) A single logical (TRUE or FALSE) value indicating if raw data files desired (i.e. without standardized column names). Default is FALSE.
#'
#' @description
#' A short description...
#'
#' @seealso [get_bc_stations()]
#' @return
#' A tibble of hourly observation data for desired station(s) and date range where available.
#' Columns available will vary depending on available data from station(s).
#'
#' @family Data Collection
#' @family Canadian Air Quality
#'
#' @export
#' @examples
#' # For a single station
#' station = "0450307" # EMS IDs - see get_bc_stations()
#' # For the years 2019 and 2020
#' date_range = lubridate::ymd_h(c("2019-01-01 00", "2020-12-31 23"), tz = "Etc/GMT+8")
#' get_bc_data(station, date_range)
#'
#' # For multiple stations
#' stations = c("0450307", "E206898") # EMS IDs - see get_bc_stations()
#' # For first week of January 2019
#' date_range = lubridate::ymd_h(c("2019-01-01 00", "2019-01-07 23"), tz = "Etc/GMT+8")
#' get_bc_data(stations, date_range)
get_bc_data = function(stations, date_range, raw = FALSE){
  # TODO: date_utc and date_local
  # TODO: stop/warn if date range not date or datetime
  # TODO: add description
  # TODO: ensure date times match what BC webmap displays (check for DST and backward/forward averages)

  . = NULL # so build check doesn't yell at me
  # Get list of years currently QA/QC'ed
  qaqc_years = get_bc_qaqc_years()

  # Get all years in desired date range
  desired_years = seq( date_range[1], date_range[2], by = "1 days") %>%
    # convert to timezone of BCMoE data
    lubridate::with_tz(bcmoe_tzone) %>%
    # extract unique years
    lubridate::year(.) %>%
    unique() %>%
    sort()

  # Drop all years not in qaqc_years except the first
  is_qaqc_year = desired_years %in% qaqc_years # find years that are qa/qc'ed years
  # Only need to get each qa/qc year plus one for all non qa/qc years
  years_to_get = c(desired_years[is_qaqc_year], desired_years[!is_qaqc_year][1])
  # drop NA from case when no non qa/qced years provided
  years_to_get = years_to_get[!is.na(years_to_get)]

  # Get data for each year for each station
  stations_data = years_to_get %>%
    # Loop through years and get data for stations
    lapply(\(year) get_annual_bc_data(stations, year, qaqc_years)) %>%
    # Combine annual datasets
    dplyr::bind_rows()

  if(nrow(stations_data) == 0){
    warning("No data available for provided stations and date_range")
    return(NULL)
  }

  # Filter to desired date range
  stations_data = dplyr::filter(stations_data,
      .data$DATE_PST %>% dplyr::between(date_range[1], date_range[2])) %>%
    # Drop duplicated dates for a particular station
    dplyr::filter(!duplicated(.data$DATE_PST), .by = "EMS_ID") %>%
    # Replace blank values with NA
    dplyr::mutate_at(-1, \(x) ifelse(x == "", NA, x)) %>%
    # Rename and select desired columns
    standardize_colnames(bcmoe_col_names, raw = raw)

  return(stations_data)
}

# TODO: clean up and document
# stations = get_bc_stations(years = 1998:2024)
get_bc_stations = function(dates = Sys.time()){
  years = unique(lubridate::year(dates))

  # Define metadata file locations
  stations_file ="bc_air_monitoring_stations.csv"
  ftp_site_qaqc = paste0(bc_ftp_site, "AnnualSummary/{year}/")
  ftp_site_raw  = paste0(bc_ftp_site, "Hourly_Raw_Air_Data/Year_to_Date/")

  # Get list of years currently QA/QC'ed
  qaqc_years = get_bc_qaqc_years()

  # Drop all years not in qaqc_years except the first
  years = sort(years) # ensure years are in order
  is_qaqc_year = years %in% qaqc_years # find years that are qa/qc'ed years
  # Only need to get each qa/qc year plus one for all non qa/qc years
  years_to_get = c(years[is_qaqc_year], years[!is_qaqc_year][1])
  # drop NA from case when no non qa/qced years provided
  years_to_get = years_to_get[!is.na(years_to_get)]
  # For each year to get station info for
  stations = lapply(years_to_get, \(year){
    # Use ftp_site_qaqc path for qa/qc years
    if(year %in% qaqc_years){
      ftp_site = stringr::str_replace(
        ftp_site_qaqc, "\\{year\\}", as.character(year))
    # And ftp_site_raw otherwise
    }else ftp_site = ftp_site_raw
    # download and read in meta file for this year
    data.table::fread(paste0(ftp_site, stations_file), data.table = FALSE,
                      showProgress = FALSE,
                      colClasses = c("OPENED" = "character", "CLOSED" = "character"))
  }) %>%
    # Combine meta files for each desired year
    dplyr::bind_rows()

  # Clean up and export
  stations %>%
    # Choose and rename columns
    dplyr::select(
      site_id = .data$EMS_ID, site_name = .data$STATION_NAME,
      city = .data$CITY, lat = .data$LAT, lng = .data$LONG, elev = .data$ELEVATION,
      date_created = .data$OPENED, date_removed = .data$CLOSED
    ) %>%
    # Replace blank values with NA
    dplyr::mutate_all(\(x) ifelse(x == "", NA, x)) %>%
    # Convert date_created and date_removed to date objects
    dplyr::mutate_at(c('date_created', 'date_removed'),
      \(x) lubridate::ymd(stringr::str_sub(x, end = 10))) %>%
    # Sort alphabetically
    dplyr::arrange(.data$site_name) %>%
    # Drop duplicated meta entries
    unique() %>%
    # Drop missing lat/lng rows
    dplyr::filter(!is.na(lat), !is.na(lng))
}

## BC MoE Helpers ---------------------------------------------------------

bc_ftp_site = "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"

bcmoe_tzone = "Etc/GMT+8"

bcmoe_col_names = c(
  # Meta
  date_local = "DATE_PST",
  site_id = "EMS_ID",
  site_name = "STATION_NAME",
  quality_assured = "quality_assured", # Added by get_annual_bc_data()
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
  vapourPressure_1hr_kpa_instrument = "VAPOUR_INSTRUMENT"#,
)

# Checks the years in the QA/QC'ed data archive for BC MoE data
# (usually 1-2 years out of date)
get_bc_qaqc_years = function(){
  . = NULL # so build check doesn't yell at me
  ftp_site_qaqc = "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Archieved/"

  qaqc_dirs = ftp_site_qaqc %>%
    # Load file details
    readLines() %>%
    # Split on white space
    stringr::str_split("\\s", simplify = T) %>%
    # Keep last column only
    .[, ncol(.)]

  years = suppressWarnings(qaqc_dirs %>% # suppress 'NAs introduced due to coercion' warning
    # Drop directory name prefix
    stringr::str_remove("STATION_DATA_") %>%
    # Convert years from character to numeric, dropping NAs (non-year dirs)
    as.numeric()) %>%
    .[!is.na(.)]

  return(years)
}

get_annual_bc_data = function(stations, year, qaqc_years = NULL){
  . = NULL # so build check doesn't yell at me
  # Where BC MoE AQ/Met data are stored
  ftp_site = "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"
  # Where to get the QA/QC'ed obs - usually a few years out of date
  loc_qaqc = ftp_site %>% # "Archieved" lol
    paste0("Archieved/STATION_DATA_{year}/{station}.csv")
  # Where to get the raw obs
  loc_raw = ftp_site %>% # actually since qa/qc to date, not just this year
    paste0("Hourly_Raw_Air_Data/Year_to_Date/STATION_DATA/{station}.csv")

  # Classes of specific columns found in all files
  colClasses = c(
    DATE_PST = "character", # will be converted to date
    EMS_ID = "character",
    STATION_NAME = "character")

  # Get list of years that have been qaqc'ed if needed
  if (is.null(qaqc_years)) qaqc_years = get_bc_qaqc_years()

  # If year has qaqc'ed data
  if (year %in% qaqc_years) {
    # Use qaqc location
    loc = loc_qaqc %>%
      stringr::str_replace("\\{year\\}", as.character(year))
    # Otherwise, use raw location
  }else loc = loc_raw

  # Get each stations data for this year
  stations_data = stations %>%
    # Insert station ids into file location
    stringr::str_replace(loc, "\\{station\\}", .) %>%
    # Load each stations data if it exists and combine
    lapply(\(p) on_error(return =  NULL,
      suppressWarnings(data.table::fread(file = p,
                                         showProgress = FALSE,
                                         colClasses = colClasses))
    )) %>%
    # Combine rowise into a single dataframe
    dplyr::bind_rows()

  if(nrow(stations_data) == 0){
    warning(paste("No data available for provided stations for", year))
    return(NULL)
  }else {
    stations_data  %>%
      # Format date time properly and add a flag for if data are qa/qc'ed
      dplyr::mutate(
        DATE_PST = tryCatch(
          lubridate::ymd_hms(.data$DATE_PST, tz = bcmoe_tzone),
          warning = \(...) lubridate::ymd_hm(.data$DATE_PST, tz = bcmoe_tzone)),
        quality_assured = loc != loc_raw
      ) %>%
      # Drop DATE and TIME columns (erroneous)
      dplyr::select(-'DATE', -'TIME') %>%
      return()
  }

}


# AirNow Data -------------------------------------------------------------

# See https://docs.airnowapi.org/docs/HourlyDataFactSheet.pdf
#' Download air quality station observations from the US EPA "AirNow" platform
#'
#' @param stations (Optional) Either "all" or a character vector specifying AQS IDs for stations to filter data to.
#' If "all" not provided, data for all stations for each hour in `date_range` are still downloaded,
#' but only data for desired stations is returned. Default is "all".
#' @param date_range A datetime vector (or a character vector with UTC dates in "YYYY-MM-DD HH" format) with either 1 or 2 values.
#' Providing a single value will return data for that hour only,
#' whereas two values will return data between (and including) those times
#' @param raw (Optional) A single logical (TRUE or FALSE) value indicating if
#' raw data files desired (i.e. without standardized column names). Default is FALSE.
#'
#' @description
#' A short description...
#'
#' @return
#' A tibble of hourly observation data for desired station(s) and date range where available.
#' Columns available will vary depending on available data from station(s).
#' @export
#'
#' @family Data Collection
#' @family USA Air Quality
#'
#' @examples
#' # Get data for all stations for first 3 hours of Jan 2019
#' date_range = lubridate::ymd_h(c("2019-01-01 00", "2019-01-01 02"), tz = "Etc/GMT+8")
#' get_airnow_data("all", date_range)
#'
#' # Get data for two specific stations for first 3 hours of Jan 2019
#' date_range = lubridate::ymd_h(c("2019-01-01 00", "2019-01-01 02"), tz = "Etc/GMT+8")
#' get_airnow_data(c("000010102", "000010401"), date_range)
#'
#' # Get non-standardized data for all stations for first 3 hours of Jan 2019
#' date_range = lubridate::ymd_h(c("2019-01-01 00", "2019-01-01 02"), tz = "Etc/GMT+8")
#' get_airnow_data("all", date_range, raw = TRUE)
get_airnow_data = function(stations = "all", date_range, raw = FALSE){

  ## Handle date_range inputs ---
  # If only a single value provided, repeat it
  if(length(date_range) == 1){
    date_range = c(date_range, date_range)
  }
  # If not 1/2 values provided, stop and say why
  if(length(date_range) != 2){
    stop("`date_range` must have a length of either 1 or 2.")
  }
  # If characters provided for date range, try to convert and stop if that fails
  if(is.character(date_range)){
    date_range = lubridate::ymd_h(date_range, tz = "UTC")
    if(any(is.na(date_range)))
      stop("Ensure `date_range` is either a datetime or a character (UTC only) with this format: YYYY-MM-DD HH")
  }
  # AirNow hourly data only available for 2014 onwards - warn user if date_range before
  min_date = lubridate::ymd_h("2014-01-01 00", tz = "UTC")
  if(any(date_range < min_date)){
    warning(paste0(
      "No hourly data available on AirNow prior to 2014.\n",
      "Set the `date_range` to a period from 2014-01-01 (UTC) onwards to stop this warning."))
    # End the function here and return NULL if all requested data before min date
    if(all(date_range < min_date)) return(NULL)
    # Otherwise set the one that is before min date to the min date
    # (i.e. still try to get data from min_date onwards if the provided period straddles it)
    date_range[date_range < min_date] = min_date
  }
  # AirNow hourly data only available for the current hour and prior - warn user if date_range in the future
  max_date = lubridate::floor_date(lubridate::with_tz(Sys.time(), "UTC"), "hours")
  if(any(date_range > max_date)){
    warning(paste0(
      "No hourly data available on AirNow beyond the current hour (UTC).\n",
      "Set the `date_range` to a period from ", format(max_date, "%F %H:00"),
      " (UTC) and earlier to stop this warning."))
    # End the function here and return NULL if all requested data after max date
    if(all(date_range > max_date)) return(NULL)
    #
    date_range[date_range > max_date] = max_date
  }
  # Data may be missing for most recent hourly files - depending on data transfer delays
  # Warn user of this if requesting data in past 48 hours, especially if last 55 minutes
  if(max(date_range) - max_date > lubridate::hours(-48)){ # if date_range in past 48 hours
    if(max(date_range) - max_date > lubridate::minutes(-55)){ # if date_range in past 55 minutes
      warning(paste("The current hour AirNow files is updated twice per hour",
                    "(at 25 and 55 minutes past the hour) or more frequently if possible.",
                    "All hourly files for the preceding 48 hours will be updated every hour",
                    "to ensure data completeness and quality.",
                    "Data may be missing from stations for any hours in the past 48, especially for the current hour."))
    }else{ # if date_range in past 48 hours but not past 55 minutes
      warning(paste("All hourly AirNow files for the preceding 48 hours will be updated every hour",
                    "to ensure data completeness and quality.",
                    "Data may be missing from stations for any hours in the past 48, especially for the current hour."))
    }
  }

  ## Main ---
  # Make hourly file paths
  date_range = sort(date_range) # Ensure date_range is correct order
  dates = seq(date_range[1], date_range[2], "1 hours") +
    lubridate::hours(-1) # files are forward looking averages
  airnow_paths = make_airnow_filepaths(dates)

  airnow_data = lapply(airnow_paths, \(pth){
    # Try downloading data for each hour, returning NULL if failed (no file)
    on_error(return = NULL, data.table::fread(file = pth, showProgress = FALSE))}) %>%
    # Combine rowise into a single dataframe
    dplyr::bind_rows() %>%
    # Set the file header
    stats::setNames(
      c('date','time','siteID','site',
        'tz_offset','param','unit','value','operator'))

  # If no data (should not happen unless AirNow is offline and requesting current data)
  if(nrow(airnow_data) == 0){
    # Warn user and end the function here, returning NULL
    warning(paste("No data available for provided date range.",
            "Ensure `date_range` is valid and AirNow is not offline",
            "(see: https://www.airnowtech.org/ for AirNow status)."))
    return(NULL)
  }

  # Basic formatting
  airnow_data = airnow_data %>%
    # Drop duplicate rows if any
    unique() %>%
    # Datetime formatting
    dplyr::mutate(
      # Join date and time columns, convert to datetime
      date = lubridate::mdy_hm(paste(.data$date, .data$time), tz = "UTC") +
        lubridate::hours(1), # from forward -> backward looking averages,
      # Add local time column (STANDARD TIME) - format as character due to timezone variations
      # (datetimes only support a single timezone in a column)
      date_local = format(date + lubridate::hours(.data$tz_offset), "%F %H:%M")) %>%
    # drop now erroneous time and tz_offset columns
    dplyr::select(-"time", -"tz_offset")

  # Filter for desired stations if "all" not supplied
  if(! "all" %in% stations){
    airnow_data = subset(airnow_data, .data$siteID %in% stations)
  }

  # If raw data desired, end function and return data
  if(raw) return(airnow_data)
  ## Otherwise
  # Convert from long format to wide format ("param_unit" column for each param/unit)
  airnow_data = tidyr::pivot_wider(
    airnow_data, names_from = c("param", "unit"), values_from = "value")

  # Standardize units if needed
  if("BARPR_MILLIBAR" %in% names(airnow_data))
    airnow_data$barpr_1hr_kpa = airnow_data$BARPR_MILLIBAR / 10
  if("CO_PPM" %in% names(airnow_data))
    airnow_data$co_1hr_ppb = airnow_data$CO_PPM * 1000

  # Standardize column names/order
  airnow_data = airnow_data %>%
    # All AirNow data is not QA/QC'ed - mark it as such
    dplyr::mutate(quality_assured = FALSE) %>%
    # Rename and select desired columns
    standardize_colnames(airnow_col_names)

  return(airnow_data)
}

# TODO: document
get_airnow_stations = function(dates = lubridate::floor_date(Sys.time(), "hours")){
  # Make path to each supplied hours meta file
  dates = sort(dates, decreasing = TRUE) # Newest first
  airnow_paths = make_airnow_metapaths(dates)
  names(airnow_paths) = dates

  # For each path
  airnow_meta = lapply(
      dates, \(d){
        p = airnow_paths[names(airnow_paths) == as.character(d)]
        # Download meta file, returning NULL if failed
        on_error(return = NULL,
          data.table::fread(file = p, showProgress = FALSE) %>%
            # Flag file date for later
            dplyr::mutate(file_date = d))
      }) %>%
    # Combine rowise into a single dataframe
    dplyr::bind_rows() %>%
    # Set header names
    stats::setNames(
      c('siteID', 'param', 'site_location_code', 'site', 'status', 'operator_code',
        'operator', 'usa_region', 'lat', 'lon', 'elev', 'tz_offset', 'country',
        'UNKNOWN', 'UNKNOWN', 'location_code', 'location', 'UNKNOWN', 'region',
        'UNKNOWN', 'city', 'UNKNOWN', 'UNKNOWN', "file_date")) %>%
    # Choose and reorder colummns, standardizing names
    dplyr::select(
      site_id = 'siteID', site_name = "site", 'city', 'lat', lng = 'lon', 'elev',
      'status', 'operator', 'tz_offset', as_of = "file_date") %>%
    # Replace placeholders with proper NA values
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character), \(x) ifelse(x %in% c("N/A", "na", "n/a"), NA, x))) %>%
    # Drop duplicated entries
    dplyr::distinct(dplyr::across(-"as_of"), .keep_all = TRUE)
  return(airnow_meta)
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
  airnow_site = 'https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow'
  airnow_files = paste0("HourlyData_", format(dates, "%Y%m%d%H.dat"))
  file.path(airnow_site, lubridate::year(dates),
            format(dates, "%Y%m%d"), airnow_files)
}

make_airnow_metapaths = function(dates){
  airnow_site = 'https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow'
  file.path(airnow_site, lubridate::year(dates),
            format(dates, "%Y%m%d"), "monitoring_site_locations.dat")
}
