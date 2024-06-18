
# [Canada] BC MoE Data ----------------------------------------------------

get_bc_data = function(stations, date_range, raw = FALSE){
  . = NULL # so build check doesn't yell at me
  # Get list of years currently QA/QC'ed
  qaqc_years = get_bc_qaqc_years()

  # Get all years in desired date range
  desired_years = date_range[1] %>%
    # sequence of all days in period
    seq(to = date_range[2], by = "1 days") %>%
    # convert to timezone of BCMoE data
    lubridate::with_tz("Etc/GMT+8") %>%
    # extract unique years
    unique(lubridate::year(.))

  # Get data for each year for each station
  stations_data = desired_years %>%
    # Loop through years and get data for stations
    lapply(\(year){
      # There is only a single file for non-qaqc_years
      # After getting that once, we don't need to again
      collected_raw_data_already = !(year - 1) %in% qaqc_years # TODO: fails if no years in qaqc years?
      if (collected_raw_data_already){
        return(NULL)
      }else get_annual_bcmoe_data(stations, year, qaqc_years)
    }) %>%
    # Combine annual datasets
    dplyr::bind_rows()

  if(nrow(stations_data) == 0){
    warning("No data available for provided stations and date_range")
    return(NULL)
  }

  # Reformat data
  stations_data = stations_data %>%
    # Filter to desired date range
    dplyr::filter(.data$DATE_PST %>% dplyr::between(date_range[1], date_range[2])) %>%
    # Drop duplicated dates for a particular station
    dplyr::filter(!duplicated(DATE_PST), .by = "EMS_ID") %>%
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

  # TODO: drop all years not in qaqc_years except the first (and do the same above)
  stations = lapply(years, \(year){
    if(year %in% qaqc_years){
      meta_path = stringr::str_replace(ftp_site_qaqc, "\\{year\\}",
                                       as.character(year)) %>%
        paste0(stations_file)
    }else{
      if(raw_downloaded) return(NULL)
      raw_downloaded = TRUE
      meta_path = paste0(ftp_site_raw, stations_file)
    }
    data.table::fread(meta_path, data.table = FALSE,
                      colClasses = c("OPENED" = "character", "CLOSED" = "character"))
  }) %>%
    dplyr::bind_rows()

  stations %>%
    dplyr::select(
      site_name = STATION_NAME, site_id = EMS_ID,
      city = CITY, lat = LAT, lng = LONG, elev = ELEVATION,
      date_created = OPENED, date_removed = CLOSED
    ) %>%
    dplyr::mutate_all(\(x) ifelse(x == "", NA, x)) %>%
    dplyr::mutate_at(c('date_created', 'date_removed'),
                     \(x) stringr::str_sub(x, end = 10)) %>%
    dplyr::arrange(site_name) %>%
    unique()

}

## BC MoE Helpers ---------------------------------------------------------

bc_ftp_site = "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"

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

  # Timezone of data on ftp site
  tzone = "Etc/GMT+8"
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
     lapply(\(p) tryCatch(suppressWarnings(data.table::fread(p, colClasses = colClasses)),
              error = \(...) NULL)) %>%
     dplyr::bind_rows()

  if(nrow(stations_data) == 0){
    warning(paste("No data available for provided stations for", year))
    return(NULL)
  }else stations_data  %>%
     # Format date time properly and add a flag for if data are qa/qc'ed
     dplyr::mutate(
       DATE_PST = tryCatch(
         lubridate::ymd_hms(.data$DATE_PST, tz = tzone),
         warning = \(...) lubridate::ymd_hm(.data$DATE_PST, tz = tzone)),
       quality_assured = loc != loc_raw
     ) %>%
     # Drop DATE and TIME columns (erroneous)
     dplyr::select(-DATE, -TIME)

}
