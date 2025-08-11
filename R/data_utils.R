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
get_station_data <- function(
  locations,
  date_range,
  buffer_dist = 10,
  networks = "all",
  sources = "all",
  verbose = TRUE
) {
  # Determine what, when, and where to get data
  data_funs <- get_data_collection_funs(networks, sources)
  date_range <- date_range |> handle_date_range()
  search_area <- locations |>
    determine_search_area(buffer_dist = buffer_dist, verbose = verbose)

  # Get data for our stations/date_range
  stations <- data_funs |>
    get_stations_in_search_area(search_area, date_range)
  data <- data_funs |>
    get_data_for_stations(stations, date_range, verbose)
  list(stations = stations, data = data)
}

# Get polygons of locations to search for stations within
determine_search_area <- function(locations, buffer_dist = 10, verbose) {
  rlang::check_installed("sf")
  # TODO: allow for station ids/names
  if (is.character(locations)) {
    search_area <- locations |>
      handyr::for_each(
        .as_list = TRUE,
        .bind = TRUE,
        get_location_polygons
      )
    if (is.null(search_area)) {
      stop(paste0(
        "Unable to find a polygonal boundary for specified location."
      ))
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
        "Adding a search buffer of",
        buffer_dist,
        "km to each location (see arg `buffer_dist`)"
      ))
    }
    search_area <- sf::st_buffer(search_area, buffer_dist)
  }
  return(search_area)
}

# TODO: Document
get_location_polygons <- function(location_name, verbose = TRUE) {
  location_name |>
    osmdata::getbb(format_out = "sf_polygon") |>
    handyr::on_error(.return = NULL, .message = verbose)
}

# Get station metadata during period in our search area
get_stations_in_search_area <- function(data_funs, search_area, date_range) {
  rlang::check_installed("sf")
  dates <- seq(date_range[1], date_range[2], "30 days")
  stations <- names(data_funs) |>
    handyr::for_each(
      .as_list = TRUE,
      .bind = TRUE,
      \(net) {
        names(data_funs[[net]]) |>
          handyr::for_each(
            .as_list = TRUE,
            .bind = TRUE,
            \(src) {
              data_funs[[net]][[src]]$meta(dates, use_sf = TRUE) |>
                dplyr::mutate(source = src, network = net) |>
                handyr::on_error(.return = NULL, .message = TRUE)
            }
          )
      }
    )
  sf::st_agr(stations) <- "constant"
  stations <- stations |>
    sf::st_intersection(search_area) |>
    dplyr::select("site_id", "site_name", "network", "source", "geometry")
  if (nrow(stations) == 0) {
    stop(
      "No stations in location(s) and date range for selected networks/sources."
    )
  }
  return(stations)
}

get_data_for_stations <- function(data_funs, stations, date_range, verbose) {
  networks <- unique(stations$network)
  networks |>
    handyr::for_each(
      .as_list = TRUE,
      .bind = TRUE,
      \(net) {
        sources <- names(data_funs[[net]])
        sources |>
          handyr::for_each(
            .as_list = TRUE,
            .bind = TRUE,
            \(src) {
              site_ids <- stations |>
                dplyr::filter(.data$source == src & .data$network == net) |>
                dplyr::pull(.data$site_id) |>
                unique()
              if (length(site_ids) == 0) {
                return(NULL)
              }
              if (verbose) {
                message(paste(
                  net,
                  "-",
                  src,
                  ":",
                  length(site_ids),
                  "station(s) to check for data"
                ))
              }
              data_fun <- data_funs[[net]][[src]]$data
              data_fun(stations = site_ids, date_range, verbose = verbose) |>
                dplyr::mutate(source = src, network = net) |>
                handyr::on_error(.return = NULL, .message = TRUE)
            }
          )
      }
    )
}

get_data_collection_funs <- function(networks = "all", sources = "all") {
  data_collection_funs <- list(
    FEM = list(
      # Federal Equivalent Method monitors
      BCgov = list(data = get_bcgov_data, meta = get_bcgov_stations),
      ABgov = list(data = get_abgov_data, meta = get_abgov_stations),
      AirNow = list(data = get_airnow_data, meta = get_airnow_stations)
    ) # , Temporarily degraded until testing complete
    # LCM = list( # Low-Cost Monitors
    #   PurpleAir = list(data = get_purpleair_data, meta = get_purpleair_stations)
    # )
  )
  if (!"all" %in% networks) {
    data_collection_funs <- data_collection_funs[networks]
  }
  if (!"all" %in% sources) {
    data_collection_funs <- data_collection_funs |>
      lapply(\(srcs) srcs[names(srcs) %in% sources])
  }
  return(data_collection_funs)
}

data_citation <- function(source, verbose = TRUE) {
  if (!verbose) {
    return(invisible(NULL))
  }
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
    "Data from the '",
    source,
    "'",
    " repository are collected from ",
    source_meta[[source]]$name,
    " and are NOT to be used commercially.",
    " Recent observations are not quality assured,",
    " and are intended for research and/or situational awareness",
    " (**NOT for regulatory decision making**).",
    " See `",
    source_meta[[source]]$url,
    "` for more information."
  ))
}

# Convert date_utc to local time and insert as "date_local" column (formatted as a character)
# TODO: move to handyr
insert_date_local <- function(obs, stations_meta, by = "site_name") {
  obs |>
    dplyr::left_join(
      stations_meta |> dplyr::select(by, "tz_local"),
      by = by
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      date_local = .data$date_utc |>
        lubridate::with_tz(.data$tz_local) |>
        format("%F %H:%M %z")
    ) |>
    dplyr::ungroup() |>
    dplyr::relocate("date_local", .after = "date_utc") |>
    dplyr::select(-"tz_local")
}

# TODO: any missing? This is copied from bcmoe data
default_units <- c(
  # Particulate Matter
  pm25_1hr = "ug/m3",
  pm10_1hr = "ug/m3",
  # Ozone
  o3_1hr = "ppb",
  # Nitrogen Pollutants
  no_1hr = "ppb",
  no2_1hr = "ppb",
  nox_1hr = "ppb",
  # Sulfur Pollutants
  so2_1hr = "ppb",
  trs_1hr = "ppb",
  h2s_1hr = "ppb",
  # Carbon Monoxide
  co_1hr = "ppb",
  # Met data
  rh_1hr = "%",
  temp_1hr = "degC",
  wd_1hr = "degrees",
  wd_unitvector_1hr = "degrees",
  ws_1hr = "m/s",
  ws_vector_1hr = "m/s",
  precip_1hr = "mm",
  snow_1hr = "cm",
  pressure_1hr = "kPa", 
  vapour_pressure_1hr = "kPa"
)