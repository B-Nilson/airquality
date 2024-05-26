
# [Canada] BC MoE Data ----------------------------------------------------

# TODO: setup loading in and parsing station metadata
get_bcmoe_meta = function(){
  meta_file = "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Hourly_Raw_Air_Data/Year_to_Date/bc_air_monitoring_stations.csv"
  # ORRR
  meta_file = "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/{year}/bc_air_monitoring_stations.csv"
}

get_bcmoe_qaqc_years = function(){
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

get_bcmoe_data = function(stations, date_range){
  # Timezone of data on ftp site
  tzone = "Etc/GMT+8"

  # Get list of available years in loc_qaqc
  qaqc_years = get_bcmoe_qaqc_years()

  # Get data for each year for each station
  stations_data = date_range[1] %>%
    # Get all years in desired date range
    seq(to = date_range[2], by = "1 days") %>%
    lubridate::with_tz(tzone) %>%
    lubridate::year() %>%
    unique() %>%
    # Loop through years and get data for stations
    lapply(\(year){
      # After getting the loc_raw data we don't need to again
      if (!(year - 1) %in% qaqc_years) return(NULL)
      get_annual_bcmoe_data(stations, year, qaqc_years)
    }) %>%
    # Combine annual datasets
    dplyr::bind_rows() %>%
    # Filter to desired date range
    dplyr::filter(DATE_PST %>% dplyr::between(date_range[1], date_range[2])) %>%
    # Drop columns with all NAs
    dplyr::select(dplyr::where(~!all(is.na(.x))))

  return(stations_data)
}

get_annual_bcmoe_data = function(stations, year, qaqc_years = NULL){
  # Where BC MoE AQ/Met data are stored
  ftp_site = "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"
  # Where to get the QA/QC'ed obs - usually a few years out of date
  loc_qaqc = ftp_site %>% # "Archieved" lol
    paste0("Archieved/STATION_DATA_{year}/{station}.csv")
  # Where to get the raw obs
  loc_raw = ftp_site %>% # actually since qa/qc to date, not just this year
    paste0("Hourly_Raw_Air_Data/Year_to_Date/STATION_DATA/{station}.csv")

  # Timezone of data on ftp site
  tzone = "Etc/GMT+8"
  # Classes of specific columns found in all files
  colClasses = c(
    DATE_PST = "character", # will be converted to date
    EMS_ID = "character",
    STATION_NAME = "character")

  # Get list of years that have been qaqc'ed if needed
  if (is.null(qaqc_years)) qaqc_years = get_bcmoe_qaqc_years()

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
     # Load each stations data and combine
     lapply(data.table::fread, colClasses = colClasses) %>%
     dplyr::bind_rows() %>%
     # Format date time properly
     dplyr::mutate(DATE_PST = tryCatch(
       lubridate::ymd_hms(DATE_PST, tz = tzone),
       warning = \(...) lubridate::ymd_hm(DATE_PST, tz = tzone))
     ) %>%
     # Drop DATE and TIME columns (erroneous)
     dplyr::select(-DATE, -TIME)

  return(stations_data)
}
