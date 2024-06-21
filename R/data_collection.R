
# [Canada] BC MoE Data ----------------------------------------------------

#' Gather air quality station observations from the British Columbia (Canada) Government
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
#' A tibble of hourly observation data for desired (a) station(s) and date range where available.
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
#' # For January 2019
#' date_range = lubridate::ymd_h(c("2019-01-01 00", "2019-01-31 23"), tz = "Etc/GMT+8")
#' get_bc_data(stations, date_range)
get_bc_data = function(stations, date_range, raw = FALSE){
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
get_bc_stations = function(years = lubridate::year(Sys.time())){
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
  years_to_get = years_to_get[!is.na(years_to_get)
                              ]
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
                      colClasses = c("OPENED" = "character", "CLOSED" = "character"))
  }) %>%
    # Combine meta files for each desired year
    dplyr::bind_rows()

  # Clean up and export
  stations %>%
    # Choose and rename columns
    dplyr::select(
      site_name = .data$STATION_NAME, site_id = .data$EMS_ID,
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
    unique()

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
     lapply(\(p) tryCatch(suppressWarnings(data.table::fread(file = p, colClasses = colClasses)),
              error = \(...) NULL)) %>%
     dplyr::bind_rows()

  if(nrow(stations_data) == 0){
    warning(paste("No data available for provided stations for", year))
    return(NULL)
  }else stations_data  %>%
     # Format date time properly and add a flag for if data are qa/qc'ed
     dplyr::mutate(
       DATE_PST = tryCatch(
         lubridate::ymd_hms(.data$DATE_PST, tz = bcmoe_tzone),
         warning = \(...) lubridate::ymd_hm(.data$DATE_PST, tz = bcmoe_tzone)),
       quality_assured = loc != loc_raw
     ) %>%
     # Drop DATE and TIME columns (erroneous)
     dplyr::select(-.data$DATE, -.data$TIME)

}


# AirNow Data -------------------------------------------------------------


