#' Download air quality station metadata from the US EPA "AirNow" platform
#'
#' @param dates (Optional) one or more date values indicating the day(s) to get metadata for.
#'   Default is the current date.
#' @param use_sf (Optional) a single logical (TRUE/FALSE) value indicating whether or not to return a spatial object. using the `sf` package
#'
#' @description
#' AirNow is a US EPA nationwide voluntary program which hosts non-validated air quality
#' observation data from stations in the US and many other countries globally.
#'
#' The AirNow API provides access to daily metadata files for the available stations at that time.
#'
#' [get_airnow_stations()] provides an easy way to retrieve this metadata (typically to determine station id's to pass to `get_airnow_data()`)
#'
#' @seealso [get_airnow_data()]
#' @return
#' A tibble of metadata for the air quality monitoring stations on AirNow.
#'
#' @family Data Collection
#' @family USA Air Quality
#'
#' @export
#' @examples
#' \donttest{
#' # Normal usage
#' get_airnow_stations()
#' # if spatial object required
#' get_airnow_stations(use_sf = TRUE)
#' # if data for past/specific years required
#' get_airnow_stations(dates = lubridate::ymd("2022-01-01"))
#' }
get_airnow_stations <- function(dates = Sys.time(), use_sf = FALSE) {
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
    "UNKNOWN",
    "file_date"
  )
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

  dates <- sort(dates, decreasing = TRUE)
  dates <- dates - lubridate::days(1) # in case current days file not made yet TODO: improve this
  airnow_paths <- make_airnow_metapaths(dates)
  stations <- names(airnow_paths) |>
    handyr::for_each(
      .as_list = TRUE,
      .bind = TRUE,
      \(d) {
        p <- airnow_paths[names(airnow_paths) == as.character(d)]
        read_data(file = p, encoding = "UTF-8") |>
          handyr::on_error(.return = NULL) |>
          dplyr::mutate(file_date = d)
      }
    ) |>
    stats::setNames(file_header) |>
    dplyr::select(dplyr::any_of(desired_columns)) |>
    remove_na_placeholders(na_placeholders = na_placeholders) |>
    dplyr::filter(!is.na(.data$lat), !is.na(.data$lng)) |>
    dplyr::distinct(dplyr::across(-"as_of"), .keep_all = TRUE) |>
    dplyr::mutate(
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

make_airnow_metapaths <- function(dates) {
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
  airnow_site <- "https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow"
  file_name <- "monitoring_site_locations.dat"
  paths <- file.path(
    airnow_site,
    dates |> lubridate::year(),
    dates |> format("%Y%m%d"),
    file_name
  )
  names(paths) <- dates
  return(paths)
}
