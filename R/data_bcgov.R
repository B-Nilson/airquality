#' Download air quality station metadata from the British Columbia (Canada) Government
#'
#' @param years (Optional) one or more integer values indicating the year(s) to get metadata for.
#'   Default is the current year.
#' @param use_sf (Optional) a single logical (TRUE/FALSE) value indicating whether or not to return a spatial object. using the `sf` package
#' @param quiet (Optional) a single logical (TRUE/FALSE) value indicating whether or not to suppress non-critical messages.
#' Default is FALSE
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
get_bcgov_stations <- function(
  years = lubridate::year(Sys.time() |> lubridate::with_tz(bcgov_tzone)),
  use_sf = FALSE,
  quiet = FALSE
) {
  # Get station metadata for all requested years
  qaqc_years <- bcgov_get_qaqc_years()
  stations <- years |>
    bcgov_determine_years_to_get(qaqc_years) |>
    handyr::for_each(
      .bind = TRUE,
      .as_list = TRUE,
      bcgov_get_annual_stations,
      qaqc_years = qaqc_years,
      quiet = quiet
    )

  # Standardize formatting
  col_names <- c(
    site_id = "EMS_ID",
    naps_id = "NAPS_ID",
    site_name = "STATION_NAME",
    city = "CITY",
    lat = "LAT",
    lng = "LONG",
    elev = "ELEVATION",
    date_created = "OPENED",
    date_removed = "CLOSED"
  )
  stations <- stations |>
    # Fix reversed lat/lng entries
    dplyr::mutate(
      lat2 = .data$LAT |>
        dplyr::between(45, 60) |>
        ifelse(.data$LAT, .data$LONG),
      LONG = .data$LAT |>
        dplyr::between(45, 60) |>
        ifelse(.data$LONG, -.data$LAT),
      LAT = .data$lat2
    ) |>
    # Choose and rename columns
    dplyr::select(dplyr::any_of(col_names)) |>
    dplyr::arrange(.data$site_name, date_created) |>
    # Drop duplicates and NA placeholders
    dplyr::distinct(site_id, naps_id, .keep_all = TRUE) |> # TODO: is this right? What if duplicates update the lat/lng?
    remove_na_placeholders(na_placeholders = "") |>
    dplyr::filter(!is.na(.data$lat), !is.na(.data$lng)) |>
    # Cleanup dates and add local_tz
    dplyr::mutate(
      # Convert date_created and date_removed to date objects
      dplyr::across(
        c("date_created", "date_removed"),
        \(x) lubridate::ymd(stringr::str_sub(x, end = 10))
      ),
      # get local_tz from lat/lng
      tz_local = handyr::get_timezone(lng = .data$lng, lat = .data$lat)
    )

  # Convert to spatial if desired
  if (use_sf) {
    stations <- stations |>
      sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")
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
  date_range,
  variables = "all",
  raw = FALSE,
  fast = FALSE,
  quiet = FALSE
) {
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
  date_range <- date_range |> handle_date_range(min_date, max_date)

  # Handle variables input
  variables <- tolower(variables)
  all_variables <- bcgov_col_names[endsWith(bcgov_col_names, "_INSTRUMENT")] |>
    names() |>
    stringr::str_remove("_1hr_instrument")
  if ("all" %in% variables) {
    variables <- all_variables
  } else {
    variables <- variables[variables %in% all_variables]
  }
  if (length(variables) == 0) {
    stop("No valid variables specified.")
  }

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
      check_stations_exist(known_stations$site_id, source = "the BC FTP site")
  }

  # Get data for each year for all desired stations
  stations_data <- years_to_get |>
    handyr::for_each(
      .as_list = TRUE, # TODO: remove once handyr update merged (.bind =TRUE will set this by default)
      .bind = TRUE,
      .parallel = fast,
      bcgov_get_annual_data,
      stations = stations,
      qaqc_years = qaqc_years,
      variables = variables,
      quiet = quiet
    )
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
        lubridate::with_tz("UTC")
    ) |>
    dplyr::select(dplyr::any_of(bcgov_col_names)) |>
    dplyr::filter(
      .data$date_utc |> dplyr::between(date_range[1], date_range[2])
    ) |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("_instrument"), factor)) |>
    remove_na_placeholders(na_placeholders = c("", "UNSPECIFIED")) |>
    dplyr::select_if(~ !all(is.na(.))) |>
    dplyr::distinct()

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
  t_1hr = "TEMP_MEAN",
  t_1hr_instrument = "TEMP_MEAN_INSTRUMENT",
  wd_1hr = "WDIR_VECT",
  wd_1hr_instrument = "WDIR_VECT_INSTRUMENT",
  ws_1hr = "WSPD_SCLR",
  ws_1hr_instrument = "WSPD_SCLR_INSTRUMENT",
  precip_1hr = "PRECIP",
  precip_1hr_instrument = "PRECIP_INSTRUMENT",
  snow_1hr = "SNOW",
  snow_1hr_instrument = "SNOW_INSTRUMENT",
  pressure_1hr = "PRESSURE", # TODO: Ensure pressure proper units ....
  pressure_1hr_instrument = "PRESSURE_INSTRUMENT",
  vapourPressure_1hr = "VAPOUR",
  vapourPressure_1hr_instrument = "VAPOUR_INSTRUMENT" # ,
)

# BCgov Helpers ---------------------------------------------------------

bcgov_get_qaqc_years <- function() {
  qaqc_directory <- bcgov_ftp_site |>
    paste0("/AnnualSummary/")
  # Get directories in QAQC directory
  qaqc_dirs <- qaqc_directory |>
    readLines() |>
    stringr::str_subset("<DIR>")
  # Extract years from dir names
  qaqc_dirs |>
    stringr::str_extract("\\d{4}$") |>
    as.numeric()
}

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

# TODO: Combine with duplicate of this made for ABgov once push
join_list <- function(df_list, by = NULL) {
  df_list <- df_list[which(!sapply(df_list, is.null))]
  if (length(df_list) < 2) {
    return(df_list[[1]])
  }

  df_list |>
    Reduce(f = \(...) {
      dplyr::full_join(..., by = by)
    }) |>
    # Prevent the message when joining by matching columns
    handyr::silence(
      output = FALSE,
      warnings = FALSE,
      errors = FALSE
    )
}

# BCgov Metadata ---------------------------------------------------------

bcgov_get_annual_stations <- function(
  year = lubridate::year(Sys.time() |> lubridate::with_tz(bcgov_tzone)),
  qaqc_years = NULL,
  quiet = FALSE
) {
  if (year < 1980) {
    stop("The BC FTP site does not store data prior to 1980.")
  } else if (year < 1998) {
    if (!quiet) {
      warning(
        "Metadata for years prior to 1998 is not available, using 1998 instead."
      )
    }
    year <- 1998
  }

  # File details
  qaqc_directory <- paste0("/AnnualSummary/", year, "/")
  raw_directory <- "/Hourly_Raw_Air_Data/Year_to_Date/"
  stations_file <- "bc_air_monitoring_stations.csv"
  force_col_classes <- c(OPENED = "character", CLOSED = "character")

  # Determine if should use qaqc or raw dir
  if (is.null(qaqc_years)) {
    qaqc_years <- bcgov_get_qaqc_years()
  }
  is_qaqc_year <- year %in% qaqc_years

  # Make path to this years station metadata file
  data_directory <- is_qaqc_year |>
    ifelse(qaqc_directory, raw_directory)
  metadata_path <- bcgov_ftp_site |>
    paste0(data_directory, stations_file)

  # Download the file
  metadata_path |>
    data.table::fread(colClasses = force_col_classes, showProgress = !quiet) |>
    dplyr::tibble() |>
    handyr::on_error(.return = NULL)
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

bcgov_get_qaqc_data <- function(
  stations = "all",
  years,
  variables = "all",
  use_rounded_value = TRUE,
  quiet = FALSE
) {
  if(any(variables == "pm2.5")) {
    variables[variables == "pm2.5"] <- "pm25"
  }
  if (any(variables == "all")) {
    is_instrument_col <- bcgov_col_names |> endsWith("_INSTRUMENT")
    variables <- bcgov_col_names[is_instrument_col] |>
      stringr::str_remove("_INSTRUMENT")
  } else {
    variables <- bcgov_col_names[
      names(bcgov_col_names) %in% (tolower(variables) |> paste0("_1hr"))
    ] |>
      unname()
  }

  force_col_class <- c(
    DATE_PST = "character",
    EMS_ID = "character"
  )

  # Make paths to files to get
  qaqc_directory <- bcgov_ftp_site |>
    paste0("/AnnualSummary/")
  qaqc_paths <- qaqc_directory |>
    paste0(years) |>
    paste0("/", variables, ".csv")
  # Download, format, and join data
  qaqc_data <- qaqc_paths |>
    handyr::for_each(
      .as_list = TRUE,
      .enumerate = TRUE,
      # .parallel = fast, # TODO: test if works
      \(path, i) {
        if (!quiet) {
          "Downloading file" |>
            handyr::log_step(i, "/", length(qaqc_paths))
        }
        withr::with_options(
          list(timeout = 3600),
          path |>
            data.table::fread(
              showProgress = !quiet,
              colClasses = force_col_class
            ) |>
            bcgov_format_qaqc_data(use_rounded_value = use_rounded_value) |>
            handyr::on_error(.return = NULL)
        )
      }
    ) |>
    join_list() # TODO: use .join when implemented in for_each

  if (!"all" %in% stations) {
    qaqc_data <- qaqc_data |>
      dplyr::filter(.data$EMS_ID %in% stations)
  }
  return(qaqc_data)
}

bcgov_format_qaqc_data <- function(qaqc_data, use_rounded_value = TRUE) {
  if (nrow(qaqc_data) == 0) {
    return(qaqc_data)
  }

  value_cols <- c("RAW_VALUE", "ROUNDED_VALUE")
  value_col <- value_cols[use_rounded_value + 1]
  erroneous_cols <- c(
    "NAPS_ID",
    "STATION_NAME",
    "STATION_NAME_FULL",
    "OWNER",
    "REGION",
    "DATE",
    "TIME",
    value_cols[(!use_rounded_value) + 1]
  )

  parameter <- qaqc_data$PARAMETER[1]
  qaqc_data |>
    # drop unnecessary rows/columns for memory-saving
    dplyr::filter(!is.na(.data[[value_col]])) |>
    dplyr::select(-dplyr::any_of(erroneous_cols)) |>
    # Set units of value column
    dplyr::mutate(
      UNIT = bcgov_fix_units(UNIT),
      dplyr::across(dplyr::all_of(value_col), \(x) {
        x |> units::set_units(.data$UNIT[1], mode = "standard")
      })
    ) |>
    dplyr::select(-UNIT) |>
    # PARAMETER, INSTRUMENT, VALUE -> `PARAMETER`, `PARAMETER`_INSTRUMENT
    tidyr::pivot_wider(
      names_from = "PARAMETER",
      values_from = value_col
    ) |>
    dplyr::rename_with(
      .cols = "INSTRUMENT",
      \(col_name) paste0(parameter, "_", col_name)
    )
}

bcgov_fix_units <- function(units) {
  dplyr::case_when(
    units == "% RH" ~ "%",
    units == "\xb0C" ~ "degC",
    units == "Deg." ~ "degrees",
    TRUE ~ units
  )
}

bcgov_get_raw_data <- function(stations, variables = "all", quiet = FALSE) {
  raw_directory <- bcgov_ftp_site |>
    paste0(
      "/Hourly_Raw_Air_Data/Year_to_Date/STATION_DATA/"
    )

  force_col_class <- c(
    DATE_PST = "character",
    EMS_ID = "character",
    STATION_NAME = "character"
  )

  if (any(stations == "all")) {
    stations <- bcgov_get_raw_stations()
  }

  if(any(variables == "pm2.5")) {
    variables[variables == "pm2.5"] <- "pm25"
  }
  if (any(variables == "all")) {
    variables_to_drop = character(0)
  } else {
    variables_to_drop <- names(bcgov_col_names)[
      !(names(bcgov_col_names) |>
        stringr::str_starts(variables |> paste0(collapse = "|"))) &
        !(names(bcgov_col_names) %in%
          c('date_utc', 'site_id', 'quality_assured'))
    ]
  }

  # Download each stations file and bind together
  station_paths <- raw_directory |>
    paste0(stations, ".csv")
  station_paths |>
    handyr::for_each(
      .as_list = TRUE, # TODO: remove once handyr updated (should be default when .bind = TRUE)
      .bind = TRUE,
      \(path) {
        withr::with_options(
          list(timeout = 3600),
          data.table::fread(
            file = path,
            colClasses = force_col_class,
            showProgress = !quiet
          ) |>
            bcgov_format_raw_data() |>
            dplyr::select(-dplyr::any_of(variables_to_drop)) |>
            handyr::on_error(.return = NULL)
        )
      }
    )
}

bcgov_format_raw_data <- function(raw_data) {
  if (nrow(raw_data) == 0) {
    return(raw_data)
  }
  meta_cols <- c("EMS_ID", "DATE_PST")
  instrument_cols <- stringr::str_subset(names(raw_data), "_INSTRUMENT$")
  unit_cols <- stringr::str_subset(names(raw_data), "_UNITS$")
  value_cols <- unit_cols |>
    stringr::str_remove("_UNITS$")

  # Assign units to value columns
  for (i in 1:length(unit_cols)) {
    value_col <- value_cols[i]
    unit_col <- unit_cols[i]
    raw_data <- raw_data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(value_col),
          \(x) {
            x |>
              units::set_units(
                bcgov_fix_units(.data[[unit_col]][1]),
                mode = "standard"
              )
          }
        )
      )
  }
  # Drop unnecessary columns for memory-saving
  # TODO: see if they will do in source? files are quite bloated...
  raw_data |>
    dplyr::select(dplyr::all_of(c(meta_cols, value_cols, instrument_cols)))
}

bcgov_get_raw_stations <- function() {
  raw_directory <- bcgov_ftp_site |>
    paste0(
      "/Hourly_Raw_Air_Data/Year_to_Date/STATION_DATA/"
    )
  # Pull stations from directory listing
  raw_directory |>
    readLines() |>
    stringr::str_extract("[\\w\\-]+\\.(csv|CSV)$") |>
    stringr::str_remove("\\.csv")
}

get_annual_bcgov_stations <- function(year, qaqc_years = NULL) {
  bc_ftp_site <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"
  qaqc_url <- paste0(bc_ftp_site, "AnnualSummary/{year}/")
  raw_url <- paste0(bc_ftp_site, "Hourly_Raw_Air_Data/Year_to_Date/")
  stations_file <- "bc_air_monitoring_stations.csv"

  # Determine file to get for this year
  if (is.null(qaqc_years)) {
    qaqc_years <- get_bcgov_qaqc_years()
  }
  if (year %in% qaqc_years) {
    data_url <- qaqc_url |>
      stringr::str_replace("\\{year\\}", as.character(year))
  } else {
    data_url <- raw_url
  }
  read_data(
    file = paste0(data_url, stations_file),
    data.table = FALSE,
    colClasses = c("OPENED" = "character", "CLOSED" = "character")
  ) |>
    handyr::on_error(.return = NULL)
}
