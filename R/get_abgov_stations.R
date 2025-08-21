#' Download air quality station metadata from the Alberta (Canada) Government
#'
#' @param ... Not used. For compatibility with other metadata functions and future expansion.
#' @param use_sf (Optional) a single logical (TRUE/FALSE) value indicating whether or not to return a spatial object. using the `sf` package
#'
#' @description
#' Air pollution monitoring in Canada is done by individual Provinces/Territories,
#' primarily as a part of the federal National Air Pollution Surveillance (NAPS) program.
#' The Province of Alberta hosts it's hourly air quality metadata
#' through a public API.
#'
#' [get_abgov_stations()] provides an easy way to retrieve this metadata (typically to determine station names to pass to `get_abgov_data()`)
#'
#' @seealso [get_abgov_data()]
#' @return
#' A tibble of metadata for Alberta air quality monitoring stations.
#'
#' @family Data Collection
#' @family Canadian Air Quality
#'
#' @export
#' @examples
#' \donttest{
#' # Normal usage
#' get_abgov_stations()
#' # if spatial object required
#' get_abgov_stations(use_sf = TRUE)
#' }
get_abgov_stations <- function(..., use_sf = FALSE) {
  # Constants
  api_url <- "https://data.environment.alberta.ca/Services/AirQualityV2/AQHI.svc/"
  api_endpoint <- "Stations"
  header <- c(
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
  numeric_cols <- c("lat", "lng", "elev")
  placeholders <- c("Not Available", "Unknown")

  # TODO: what about qaqc API stations?

  # Get raw station metadata from the AB gov API
  raw_stations <- api_url |>
    paste0(api_endpoint, "?") |>
    abgov_get_raw_data_request()

  # Standardize formatting
  stations <- raw_stations |>
    dplyr::select(dplyr::any_of(header)) |>
    remove_na_placeholders(na_placeholders = placeholders) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(numeric_cols),
        as.numeric
      ),
      tz_local = handyr::get_timezone(
        lng = .data$lng,
        lat = .data$lat
      )
    ) |>
    dplyr::arrange(.data$site_id)

  # Convert to spatial if desired
  if (use_sf) {
    rlang::check_installed("sf")
    stations <- stations |>
      sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") # TODO: confirm CRS, transform to WGS84 if necessary
  }

  return(stations)
}
