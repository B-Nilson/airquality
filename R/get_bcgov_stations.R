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
  years_to_get <- years |>
    bcgov_determine_years_to_get(qaqc_years)
  if(any(years_to_get < 1998)) {
    warning("Metadata for years prior to 1998 is not available, using 1998 instead.")
    years_to_get[years_to_get < 1998] <- 1998
    years_to_get <- unique(years_to_get)
  }

  stations <- years_to_get |>
    handyr::for_each(
      .bind = TRUE,
      .as_list = TRUE, # TODO: remove once handyr updated (should be default when .bind = TRUE)
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
    # Fix NAPS ID placeholder = 10
    dplyr::mutate(NAPS_ID = .data$NAPS_ID |> handyr::swap(10, NA)) |>
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
    dplyr::arrange(.data$site_id, .data$naps_id, .data$date_created) |>
    # Drop duplicates and NA placeholders
    remove_na_placeholders(na_placeholders = "") |>
    dplyr::distinct(site_id, .keep_all = TRUE) |> 
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
    rlang::check_installed("sf")
    stations <- stations |>
      sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")
  }

  return(stations)
}


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

bcgov_get_raw_stations <- function(realtime = FALSE) {
  raw_directory <- bcgov_ftp_site |>
    paste0(
      "/Hourly_Raw_Air_Data/",
      ifelse(realtime, "Station/", "Year_to_Date/STATION_DATA/")
    )
  # Pull stations from directory listing
  stations <- raw_directory |>
    readLines() |>
    stringr::str_extract("[\\w\\-]+\\.(csv|CSV)$") |>
    stringr::str_remove("\\.csv")

  stations[
    !is.na(stations) & ! stringr::str_starts(stations, "AQHI")
  ]
}

get_annual_bcgov_stations <- function(year, qaqc_years = NULL) {
  bc_ftp_site <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"
  qaqc_url <- paste0(bc_ftp_site, "AnnualSummary/{year}/")
  raw_url <- paste0(bc_ftp_site, "Hourly_Raw_Air_Data/Year_to_Date/")
  stations_file <- "bc_air_monitoring_stations.csv"

  # Determine file to get for this year
  if (is.null(qaqc_years)) {
    qaqc_years <- bcgov_get_qaqc_years()
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
