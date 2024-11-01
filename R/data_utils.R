# TODO: Add optional dependence on future.apply - if installed, allow for parallel data collection (responsibly)
# TODO: Add station localization (country - prov/terr/state - county? region? - nearest community?)
# TODO: update get_station_data() as additional functions added

# General -----------------------------------------------------------------

#' Gather air quality observations from multiple networks and data sources
#'
#' @param locations A character vector with at least one value that indicates a
#'   location on Open Street Map that data is desired for (ie. "Prince George, BC, Canada", or "Canada"),
#'   OR an sf object with polygon(s) indicating area of interest.
#' @param date_range A datetime vector (or a character vector with UTC dates in "YYYY-MM-DD HH" format) with either 1 or 2 values.
#'   Providing a single value will return data for that hour only,
#'   whereas two values will return data between (and including) those times.
#' @param buffer_dist (Optional) A single numeric value indicating the distance
#'   to buffer the station search location by (typically units of km).
#'   Default is 10.
#' @param networks (Optional) A character vector indicating which monitoring networks to get data for.
#'   Default is "all".
#' @param sources (Optional) A character vector indicating which data sources to get data from.
#'   Default is "all".
#' @param verbose (Optional) A single logical (TRUE or FALSE) value indicating if
#'   non-critical messages/warnings should be printed
#'
#' @description
#' This is the general use function for gathering air quality observation data in a
#' standardized format for all desired monitoring networks and data sources available in this package.
#'
#' It either uses OpenStreetMap to look up a polygon for (a) specified location(s) or user-provided polygons to
#' search for monitoring stations within a buffer distance of the polygon(s) from
#' the specified networks/sources. The data from those stations for the desired period
#' are then downloaded, standardized, combined, and returned.
#'
#' Currently the following monitoring networks and data sources are available:
#'
#' FEM (Regulatory-grade "Federal Equivalent Method") monitors:
#' \enumerate{
#'  \item AirNow (US/Global, non-validated, real-time)
#'  \item BCgov (B.C. (Canada), validated and non-validated, real-time)
#' }
#'
#' @return
#' A list with two elements, the first called "stations" is an sf POINT object with all
#' stations from the specified monitoring networks and data sources
#' within a specified location + buffer.
#' The second is a tibble of hourly observation data over the date range for those
#' stations IDs / networks / sources.
#'
#' The columns date_utc, date_local, site_id, site_named, and quality_assured will always be returned
#' Observation columns will be named "pollutant_averagingTime_unit", and
#' will be present depending on available data from station(s) and data sources.
#' Some sources may have additional columns included not found in others.
#' @export
#'
#' @examples
#' \donttest{
#' # Get data for all stations within 10 km of Fort St. John, BC
#' #  for the first hour of Feb 2019
#' get_station_data(locations = "Fort St. John, BC, Canada", date_range = "2019-02-01 01")
#'
#' # Get data for all FEM stations within 25 km of 2 BC cities from AirNow only
#' #  for the first hour of Feb 2019
#' get_station_data(c("Vanderhoof BC, Canada", "Kamloops, BC, Canada"),
#'   "2019-02-01 01",
#'   buffer_dist = 25,
#'   networks = "FEM", sources = "AirNow"
#' )
#' }
get_station_data <- function(locations, date_range, buffer_dist = 10,
                             networks = "all", sources = "all", verbose = TRUE) {
  if (any(networks == "all")) networks <- c("FEM") # , "LCM")
  if (any(sources == "all")) sources <- c("BCgov", "ABgov", "AirNow") # , "PurpleAir")
  date_range <- handle_date_range(date_range)
  # Get polygons of locations to search for stations within
  # TODO: allow for station ids/names
  if (is.character(locations)) {
    search_area <- lapply_and_bind(locations, get_location_polygons)
    if (is.null(search_area)) {
      stop(paste0("Unable to find a polygonal boundary for specified location."))
    }
  } else if ("sf" %in% class(locations)) {
    search_area <- locations
  } else {
    stop("Not sure how to handle provided `locations`")
  }

  sf::st_agr(search_area) <- "constant"
  if (buffer_dist > 0) {
    if (verbose) {
      warning(paste(
        "Adding a search buffer of", buffer_dist,
        "km to each location (see arg `buffer_dist`)"
      ))
    }
    search_area <- sf::st_buffer(search_area, buffer_dist)
  }

  # Get station metadata during period in our search area
  data_funs <- get_data_collection_funs(networks, sources)
  dates <- seq(date_range[1], date_range[2], "30 days")
  stations <- lapply_and_bind(names(data_funs), \(net)
  lapply_and_bind(names(data_funs[[net]]), \(src)
  on_error(
    return = NULL, msg = TRUE,
    data_funs[[net]][[src]]$meta(dates) |>
      dplyr::mutate(source = src, network = net)
  ))) |>
    # TODO: handle in each meta function instead
    dplyr::filter(!is.na(.data$lat), !is.na(.data$lng)) |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")
  sf::st_agr(stations) <- "constant"
  stations <- stations |>
    sf::st_intersection(search_area) |>
    dplyr::select("site_id", "site_name", "network", "source", "geometry")
  if (nrow(stations) == 0) {
    stop("No stations in location(s) and date range for selected networks/sources.")
  }

  # Get data for our stations/date_range
  data <- lapply_and_bind(unique(stations$network), \(net)
  lapply_and_bind(names(data_funs[[net]]), \(src){
    site_ids <- unique(stations |>
      dplyr::filter(.data$source == src & .data$network == net) |>
      dplyr::pull(.data$site_id))
    if (length(site_ids) == 0) {
      return(NULL)
    }

    if (verbose) {
      message(paste(
        net, "-", src, ":", length(site_ids),
        "station(s) to check for data"
      ))
    }
    data_fun <- data_funs[[net]][[src]]$data
    on_error(
      return = NULL, msg = TRUE,
      data_fun(stations = site_ids, date_range, verbose = verbose) |>
        dplyr::mutate(source = src, network = net)
    )
  }))
  list(stations = stations, data = data)
}

# TODO: Document
get_location_polygons <- function(location_name, verbose = TRUE) {
  on_error(
    return = NULL, msg = verbose,
    osmdata::getbb(location_name, format_out = "sf_polygon")
  )
}

get_data_collection_funs <- function(networks, sources) {
  list(
    FEM = list( # Federal Equivalent Method monitors
      BCgov  = list(data = get_bcgov_data, meta = get_bcgov_stations),
      ABgov  = list(data = get_abgov_data, meta = get_abgov_stations),
      AirNow = list(data = get_airnow_data, meta = get_airnow_stations)
    ),
    LCM = list( # Low-Cost Monitors
      PurpleAir = list(data = get_purpleair_data, meta = get_purpleair_stations)
    )
  )[networks] |>
    lapply(\(srcs) srcs[names(srcs) %in% sources])
}

data_citation <- function(source) {
  source_meta <- list(
    BCgov = list(
      name = "the British Columbia Ministry of Environment and Climate Change Strategy",
      url = "https://www2.gov.bc.ca/gov/content/environment/air-land-water/air"
    ),
    ABgov = list(
      name = "the Alberta Ministry of Environment and Protected Areas",
      url = "https://www.alberta.ca/access-air-data"
    ),
    AirNow = list(
      name = "the US Environmental Protection Agency",
      url = "https://www.airnow.gov"
    ),
    PurpleAir = list(
      name = "the PurpleAir API",
      url = "https://www.purpleair.com"
    )
  )
  message(paste0(
    "Data from the '", source, "'",
    " repository are collected from ", source_meta[[source]]$name,
    " and are NOT to be used commercially.",
    " Recent observations are not quality assured,",
    " and are intended for research and/or situational awareness",
    " (**NOT for regulatory decision making**).",
    " See `", source_meta[[source]]$url, "` for more information."
  ))
}
