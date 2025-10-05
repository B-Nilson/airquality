#' Download air quality station metadata from the US EPA "AirNow" platform
#'
#' @inheritParams get_airnow_data
#' @param time_step (Optional).
#'   A character string specifying the time step to check data for within `date_range`.
#'   Default is "1 days" (i.e. check every day within `date_range` - may be slow for long date ranges).
#' @param use_sf (Optional) a single logical (TRUE/FALSE) value indicating whether or not to return a spatial object. using the `sf` package
#'
#' @description
#' AirNow is a US EPA nationwide voluntary program which hosts non-validated air quality
#' observation data from stations in the US and many other countries globally.
#'
#' The AirNow API provides access to daily metadata files for the available stations at that time.
#'
#' [get_airnow_stations] provides an easy way to retrieve this metadata (typically to determine station id's to pass to [get_airnow_data])
#'
#' @seealso [get_airnow_data]
#' @return
#' A tibble of metadata for the air quality monitoring stations on AirNow.
#'
#' @family Data Collection
#'
#' @export
#' @examples
#' \donttest{
#' # Normal usage
#' get_airnow_stations()
#' # if spatial object required
#' get_airnow_stations(use_sf = TRUE)
#' # if data for some time in the past required
#' get_airnow_stations(date_range = lubridate::ymd("2022-01-01"))
#' # Or a range of time
#' get_airnow_stations(date_range = lubridate::ymd(c("2022-01-01","2022-01-05")))
#' }
get_airnow_stations <- function(
  date_range = "now",
  time_step = "1 days",
  use_sf = FALSE,
  quiet = FALSE
) {
  stopifnot(
    "Date" %in%
      class(date_range) |
      "POSIXct" %in% class(date_range) |
      is.character(date_range)
  )
  stopifnot(is.logical(use_sf), length(use_sf) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)

  desired_columns <- c(
    site_id = "siteID",
    site_name = "site",
    city = "city",
    lat = "lat",
    lng = "lon",
    elev = "elev",
    status = "status",
    operator = "operator",
    tz_offset = "tz_offset",
    as_of = "file_date"
  )
  na_placeholders <- c("N/A", "na", "n/a")

  # Handle date range input
  date_range <- handyr::check_date_range(date_range, tz = "UTC")
  dates <- date_range[1] |>
    seq(to = date_range[2], by = time_step)

  # Get data for each date
  airnow_paths <- make_airnow_metapaths(dates)
  stations <- names(airnow_paths) |>
    handyr::for_each(
      .bind = TRUE,
      .show_progress = !quiet,
      \(d) {
        path <- airnow_paths[names(airnow_paths) == as.character(d)]
        path |>
          read_airnow_meta_file(quiet = TRUE) |>
          dplyr::mutate(file_date = d) |>
          handyr::on_error(
            .return = NULL,
            .warn = paste0("Could not read file: ", path)
          )
      }
    ) |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::any_of(desired_columns)) |>
    remove_na_placeholders(na_placeholders = na_placeholders) |>
    dplyr::filter(stats::complete.cases(.data$site_id, .data$lat, .data$lng)) |>
    dplyr::distinct(dplyr::across(-"as_of"), .keep_all = TRUE) |>
    dplyr::mutate(
      tz_local = .data$lng |>
        handyr::get_timezone(lat = .data$lat)
    )

  # Convert to spatial if desired
  if (use_sf) {
    rlang::check_installed("sf")
    stations <- stations |>
      sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")
  }

  return(stations)
}

read_airnow_meta_file <- function(airnow_file_path, quiet = FALSE) {
  stopifnot(is.character(airnow_file_path), length(airnow_file_path) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)

  file_header <- c(
    "siteID",
    "param",
    "site_location_code",
    "site",
    "status",
    "operator_code",
    "operator",
    "usa_region",
    "lat",
    "lon",
    "elev",
    "tz_offset",
    "country",
    "UNKNOWN",
    "UNKNOWN",
    "location_code",
    "location",
    "UNKNOWN",
    "region",
    "UNKNOWN",
    "city",
    "UNKNOWN",
    "UNKNOWN"
  )
  data.table::fread(
    file = airnow_file_path,
    encoding = "UTF-8",
    showProgress = !quiet
  ) |>
    stats::setNames(file_header) |>
    dplyr::select(-dplyr::starts_with("UNKNOWN"))
}

make_airnow_metapaths <- function(dates) {
  stopifnot("POSIXct" %in% class(dates) | "Date" %in% class(dates))

  airnow_site <- "https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow"
  file_name <- "monitoring_site_locations.dat"

  # Make sure dates are from yesterday and earlier in case todays data is not yet available
  dates <- dates |> sort(decreasing = TRUE)
  yesterday <- lubridate::now(tz = "America/Vancouver") |>
    lubridate::floor_date("days") -
    lubridate::minutes(1)
  dates[dates > yesterday] <- yesterday

  # Ensure dates are UTC and after 2016-06-21
  dates <- dates |> lubridate::with_tz("UTC")
  min_date <- "2016-06-21 00" |> lubridate::ymd_h(tz = "UTC")
  if (any(dates < min_date)) {
    warning(paste(
      "Metadata files on AirNow only available from",
      min_date |> format("%F %H (UTC)"),
      "onwards.",
      "Station information returned may be inaccurate for dates before this."
    ))
    dates <- c(min_date, dates[dates > min_date])
  }

  # Build file paths
  paths <- airnow_site |>
    file.path(
      dates |> format("%Y"),
      dates |> format("%Y%m%d"),
      file_name
    ) |>
    stats::setNames(dates)
  paths[!duplicated(paths)]
}
