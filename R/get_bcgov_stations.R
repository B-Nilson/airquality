#' Download air quality station metadata from the British Columbia (Canada) Government
#'
#' @inheritParams get_airnow_stations
#' @inheritParams get_airnow_data
#'
#' @description
#' Air pollution monitoring in Canada is done by individual Provinces/Territories,
#' primarily as a part of the federal National Air Pollution Surveillance (NAPS) program.
#' The Province of British Columbia hosts it's air quality metadata
#' through a public FTP site.
#'
#' [get_bcgov_stations] provides an easy way to retrieve this metadata (typically to determine station id's to pass to [get_bcgov_data])
#'
#' @seealso [get_bcgov_data]
#' @return
#' A tibble of metadata for British Columbia air quality monitoring stations.
#'
#' @family Data Collection
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
  date_range = "now",
  use_sf = FALSE,
  quiet = FALSE
) {
  stopifnot(is.logical(use_sf), length(use_sf) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)

  bcgov_tzone <- "Etc/GMT+8" # PST (confirmed: raw/qaqc data files have col "DATE_PST")

  # Handle date_range input
  allowed_date_range <- c("1998-01-01 00:00:00", "now")
  date_range <- date_range |>
    handyr::check_date_range(
      within = allowed_date_range,
      tz = bcgov_tzone
    )

  # Determine years to get (files are annual, raw data covers recent 1+ year(s))
  qaqc_years <- bcgov_get_qaqc_years()
  years_to_get <- date_range |>
    get_bcgov_data_years(qaqc_years)

  # Get annual station metadata as needed
  stations <- years_to_get |>
    handyr::for_each(
      .bind = TRUE,
      .show_progress = !quiet,
      bcgov_get_annual_stations,
      qaqc_years = qaqc_years,
      quiet = quiet
    ) |>
    handyr::on_error(.return = NULL)

  # Handle failed retrievals
  if (nrow(stations) == 0) {
    stations <- NULL
  }
  if (is.null(stations)) {
    stop("Failed to retrieve station metadata for provided years.")
  }

  # Standardize formatting
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
    dplyr::select(dplyr::any_of(.bcgov_meta_columns)) |>
    # Drop duplicates and NA placeholders
    remove_na_placeholders(na_placeholders = "") |>
    dplyr::arrange(
      .data$site_id,
      .data$naps_id,
      .data$site_name,
      dplyr::desc(.data$date_created)
    ) |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::filter(stats::complete.cases(.data$site_id, .data$lat, .data$lng)) |>
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
      sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") # TODO: confirm CRS and standardize if needed
  }

  return(stations)
}


bcgov_get_annual_stations <- function(
  year = lubridate::year(Sys.time() |> lubridate::with_tz("Etc/GMT+8")),
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
  bcgov_ftp_site <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"
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
    data.table::fread(
      colClasses = force_col_classes,
      showProgress = FALSE
    ) |>
    dplyr::tibble() |>
    handyr::on_error(.return = NULL)
}

.bcgov_meta_columns <- c(
  site_id = "EMS_ID",
  naps_id = "NAPS_ID",
  site_name = "STATION_NAME",
  lat = "LAT",
  lng = "LONG",
  elev = "ELEVATION",
  date_created = "OPENED",
  date_removed = "CLOSED"
)
