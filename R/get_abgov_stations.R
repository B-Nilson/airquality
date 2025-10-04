#' Download air quality station metadata from the Alberta (Canada) Government
#'
#' @param ... Not used. For compatibility with other metadata functions and future expansion.
#' @inheritParams get_airnow_stations
#' @inheritParams get_airnow_data
#'
#' @description
#' Air pollution monitoring in Canada is done by individual Provinces/Territories,
#' primarily as a part of the federal National Air Pollution Surveillance (NAPS) program.
#' The Province of Alberta hosts it's hourly air quality metadata
#' through a public API.
#'
#' [get_abgov_stations] provides an easy way to retrieve this metadata (typically to determine station names to pass to [get_abgov_data])
#'
#' @seealso [get_abgov_data]
#' @return
#' A tibble of metadata for Alberta air quality monitoring stations.
#'
#' @family Data Collection
#'
#' @export
#' @examples
#' \donttest{
#' # Normal usage
#' get_abgov_stations()
#' # if spatial object required
#' get_abgov_stations(use_sf = TRUE)
#' }
get_abgov_stations <- function(..., use_sf = FALSE, quiet = FALSE) {
  stopifnot(is.logical(use_sf), length(use_sf) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)

  # Constants
  api_url <- "https://data.environment.alberta.ca/Services/AirQualityV2/AQHI.svc/"
  api_endpoint <- "Stations"
  numeric_cols <- c("lat", "lng", "elev")
  placeholders <- c("Not Available", "Unknown")

  # TODO: what about qaqc API stations?

  # Get raw station metadata from the AB gov API
  raw_stations <- api_url |>
    paste0(api_endpoint, "?") |>
    abgov_get_raw_data_request()

  # Standardize formatting
  stations <- raw_stations |>
    dplyr::select(dplyr::any_of(.abgov_meta_columns)) |>
    remove_na_placeholders(na_placeholders = placeholders) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(numeric_cols),
        as.numeric
      ),
      tz_local = .data$lng |>
        handyr::get_timezone(lat = .data$lat)
    ) |>
    dplyr::arrange(.data$site_id) |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::filter(stats::complete.cases(.data$site_id, .data$lat, .data$lng))

  # Convert to spatial if desired
  if (use_sf) {
    rlang::check_installed("sf")
    stations <- stations |>
      sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") # TODO: confirm CRS, transform to WGS84 if necessary
  }

  return(stations)
}

.abgov_meta_columns <- c(
  site_id = "Abbreviation",
  site_name = "Name",
  type = "Type",
  description = "Description",
  operated_by = "URL",
  address = "Address",
  airshed = "AirshedName",
  lat = "Latitude",
  lng = "Longitude",
  elev = "Elevation"
)
