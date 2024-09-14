# TODO: Add optional dependence on future.apply - if installed, allow for parallel data collection (responsibly)
# TODO: Add station localization (country - prov/terr/state - county? region? - nearest community?)
# TODO: update get_station_data() as additional functions added

# General -----------------------------------------------------------------

#' Gather air quality observations from multiple networks and data sources
#'
#' @param locations A character vector with at least one value that indicates a
#' location on Open Street Map that data is desired for (ie. "Prince George, BC, Canada", or "Canada"),
#' OR an sf object with polygon(s) indicating area of interest.
#' @param date_range A datetime vector (or a character vector with UTC dates in "YYYY-MM-DD HH" format) with either 1 or 2 values.
#' Providing a single value will return data for that hour only,
#' whereas two values will return data between (and including) those times.
#' @param buffer_dist (Optional) A single numeric value indicating the distance to buffer the station search location by (typically units of km). Default is 10.
#' @param networks (Optional) A character vector indicating which monitoring networks to get data for. Default is "all".
#' @param sources (Optional) A character vector indicating which data sources to get data from. Default is "all".
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
#' get_station_data("Fort St. John, BC, Canada", "2019-02-01 01")
#'
#' # Get data for all FEM stations within 25 km of 2 BC cities from AirNow only
#' #  for the first hour of Feb 2019
#' get_station_data(c("Vanderhoof BC, Canada", "Kamloops, BC, Canada"),
#'                    "2019-02-01 01", buffer_dist = 25,
#'                    networks = "FEM", sources = "AirNow")
#' }
get_station_data = function(locations, date_range, buffer_dist = 10,
                            networks = "all", sources = "all"){
  if(any(networks == "all")) networks = c("FEM", "LCM")
  if(any(sources == "all")) sources = c("BCgov", "ABgov", "AirNow", "PurpleAir")

  . = NULL # so build check doesn't yell at me

  ## Handle date_range inputs ---
  date_range = handle_date_range(date_range)

  if(is.character(locations)){
    if("North America" %in% locations){
      locations = c(locations[locations != "North America"],
                    "Canada", "United States", "Mexico")
    }
    # Get polygons from OSM for desired locations
    search_area = on_error(return = NULL,
      locations |>
        lapply(\(location) {
          loc = osmdata::getbb(location, format_out = "sf_polygon") 
          loc[!sapply(loc, is.null)] |>
            dplyr::bind_rows() |>
            sf::st_cast("POLYGON")}) |>
        dplyr::bind_rows())
    # Error if that fails
    if(is.null(search_area))
      stop(paste0("Unable to find a polygonal boundary for specified location."))
  }else if("sf" %in% class(locations)){
    # TODO: Warn buffer being applied unless buffer_km == 0
    search_area = locations
  }else{
    # TODO: improve messaging
    stop("Not sure how to handle provided `locations`")
  }
  # Avoid warning about assuming attributes are spatially constant
  sf::st_agr(search_area) = "constant"
  # Add buffer to search if desired
  if(buffer_dist > 0) search_area = sf::st_buffer(search_area, buffer_dist)

  # Data collection functions for each network and each source for that network
  data_funs = data_collection_funs(networks, sources)

  # Get station metadata during period
  dates = seq(date_range[1], date_range[2], "30 days")
  stations = lapply(names(data_funs), \(net){ # For each network
    network_funs = data_funs[[net]] # Get this networks functions
    lapply(names(network_funs), \(src){ # For each data source in this network
      source_funs = network_funs[[src]] # Get this sources functions
      # Get stations for desired date range
      source_funs$meta(dates) |>
        dplyr::mutate(source = src, network = net) # flag as from this source & network
    }) |> dplyr::bind_rows() # Combine data from all sources for this network
  }) |> dplyr::bind_rows() # Combine data from all networks

  # Filter to stations in our search area
  stations = sf::st_as_sf(stations, coords = c("lng", "lat"), crs = "WGS84")
  sf::st_agr(stations) = "constant" # Avoid warning about assuming attributes are spatially constant
  stations = stations |>
    sf::st_intersection(search_area) |>
    dplyr::select('site_id', 'site_name', 'network', 'source', 'geometry')

  # If no stations, warn and end the function here, returning NULL
  if(nrow(stations) == 0){
    warning("No stations in location(s) and date range for selected networks/sources.")
    return(NULL)
  }

  # Get data for our stations/date_range
  data = lapply(unique(stations$network), \(net){ # For each network
    network_funs = data_funs[[net]] # Get this networks functions
    lapply(names(network_funs), \(src){ # For each data source in this network
      source_funs = network_funs[[src]] # Get this sources functions
      # Get unique site_ids for stations in this source & network
      site_ids = unique(dplyr::filter(stations, .data$source == src & .data$network == net)$site_id)
      # Skip if no stations
      if(length(site_ids)==0) return(NULL)
      # Update user on state of data grab
      message(paste(net, "-", src, ":", length(site_ids), "station(s) to check for data"))
      # Get data for these stations and desired date range
      d = on_error(source_funs$data(stations = site_ids, date_range), return = NULL, msg = TRUE) 
      if(!is.null(d)) d = dplyr::mutate(d, source = src, network = net) # flag as from this source & network
      return(d)
    }) |> dplyr::bind_rows() # Combine data from all sources for this network
  }) |> dplyr::bind_rows() # Combine data from all networks

  # Return a list with metadata and observations
  return(list(stations = stations, data = data))
}

data_collection_funs = function(networks, sources){
  data_funs = list(
    # Federal Equivalent Method monitors
    FEM = list(
      # Canada Province of BC
      BCgov = list(data = get_bcgov_data, meta = get_bcgov_stations),
      # Canada Province of BC
      ABgov = list(data = get_abgov_data, meta = get_abgov_stations),
      # USA (and elsewhere...) AirNow
      AirNow = list(data = get_airnow_data, meta = get_airnow_stations)
    ),
    # Low-Cost Monitors
    LCM = list(
      # PurpleAir PM monitors
      PurpleAir = list(data = get_purpleair_data, meta = get_purpleair_stations)
    )
  )
  data_funs = data_funs[networks] |>
    lapply(\(srcs) srcs[names(srcs) %in% sources])

  return(data_funs)
}

data_citation = function(source){
  data_sources = list(
    BCgov = "the British Columbia Ministry of Environment and Climate Change Strategy",
    ABgov = "the Alberta Ministry of Environment and Protected Areas",
    AirNow = "the US Environmental Protection Agency"
    PurpleAir = "PurpleAir"
  )
  data_urls = list(
    BCgov = "https://www2.gov.bc.ca/gov/content/environment/air-land-water/air",
    ABgov = "https://www.alberta.ca/access-air-data",
    AirNow = "https://www.airnow.gov",
    PurpleAir = "www.purpleair.com"
  )
 message(paste0("Data from the '", source,
        "' repository are collected from ", data_sources[[source]],
        " and are NOT to be used commercially. ",
        "Recent observations are not quality assured, ",
        "and are intended research and/or situational awareness ",
        "(NOT for regulatory decision making). ",
        "See `", data_urls[[source]], "` for more information."))
}
