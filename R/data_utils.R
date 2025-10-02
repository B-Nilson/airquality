# TODO: Add optional dependence on future.apply - if installed, allow for parallel data collection (responsibly)
# TODO: Add station localization (country - prov/terr/state - county? region? - nearest community?)
# TODO: update get_station_data() as additional functions added

# General -----------------------------------------------------------------

#' Gather air quality observations from multiple networks and data sources
#'
#' @inheritParams get_airnow_data
#' @param locations A character vector with at least one value that indicates a
#'   location on Open Street Map that data is desired for (ie. "Prince George, BC, Canada", or "Canada"),
#'   OR an sf object with polygon(s) indicating area of interest.
#' @param buffer_dist (Optional) A single numeric value indicating the distance
#'   to buffer the station search location by (typically units of km).
#'   Default is 10.
#' @param networks (Optional) A character vector indicating which monitoring networks to get data for.
#'   Default is "all".
#' @param sources (Optional) A character vector indicating which data sources to get data from.
#'   Default is "all".
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
#' get_station_data(locations = "Fort St. John, BC, Canada", date_range = "2019-02-01 01:00:00")
#'
#' # Get data for all FEM stations within 25 km of 2 BC cities from AirNow only
#' #  for the first hour of Feb 2019
#' get_station_data(c("Vanderhoof BC, Canada", "Kamloops, BC, Canada"),
#'   "2019-02-01 01:00:00",
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
  quiet = FALSE
) {
  # Determine what, when, and where to get data
  data_funs <- get_data_collection_funs(networks, sources)
  date_range <- date_range |> handyr::check_date_range()
  search_area <- locations |>
    determine_search_area(buffer_dist = buffer_dist, quiet = quiet)

  # Get data for our stations/date_range
  stations <- data_funs |>
    get_stations_in_search_area(search_area, date_range)
  data <- data_funs |>
    get_data_for_stations(stations, date_range, quiet = quiet)
  list(stations = stations, data = data)
}

# Get polygons of locations to search for stations within
determine_search_area <- function(locations, buffer_dist = 10, quiet = FALSE) {
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
    if (!quiet) {
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
get_location_polygons <- function(location_name, quiet = FALSE) {
  location_name |>
    osmdata::getbb(format_out = "sf_polygon") |>
    handyr::on_error(.return = NULL, .message = !quiet)
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

get_data_for_stations <- function(
  data_funs,
  stations,
  date_range,
  quiet = FALSE
) {
  networks <- unique(stations$network)
  networks |>
    handyr::for_each(
      .as_list = TRUE,
      .bind = TRUE,
      .show_progress = !quiet,
      \(net) {
        sources <- names(data_funs[[net]])
        sources |>
          handyr::for_each(
            .as_list = TRUE,
            .bind = TRUE,
            .show_progress = !quiet,
            \(src) {
              site_ids <- stations |>
                dplyr::filter(.data$source == src & .data$network == net) |>
                dplyr::pull(.data$site_id) |>
                unique()
              if (length(site_ids) == 0) {
                return(NULL)
              }
              if (!quiet) {
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
              data_fun(stations = site_ids, date_range, quiet = quiet) |>
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

data_citation <- function(source, quiet = FALSE) {
  if (quiet) {
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
      stations_meta |> dplyr::select(dplyr::all_of(by), "tz_local"),
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
  snow_1hr = "mm",
  snowdepth_1hr = "cm",
  pressure_1hr = "kPa",
  vapour_pressure_1hr = "kPa"
)

standardize_data_format <- function(
  obs_data,
  date_range,
  known_stations = NULL,
  id_col = "site_id",
  fast = FALSE,
  raw = FALSE
) {
  if (raw) {
    return(obs_data)
  }
  if (nrow(obs_data) == 0) {
    stop("No data available before reformatting.")
  }
  formatted <- obs_data |>
    dplyr::arrange(
      dplyr::pick(dplyr::any_of(c("site_name", "site_id"))),
      .data$date_utc,
      !.data$quality_assured
    ) |>
    dplyr::distinct(
      dplyr::pick(dplyr::any_of(c("site_name", "site_id"))),
      .data$date_utc,
      .keep_all = TRUE
    ) |>
    dplyr::filter(
      .data$date_utc |> dplyr::between(date_range[1], date_range[2])
    ) |>
    drop_missing_obs_rows(where_fn = is.numeric)

  if (nrow(formatted) == 0) {
    stop("No data available after reformatting.")
  }

  # Insert local time (slow-ish for many stations)
  if (!fast & !is.null(known_stations)) {
    formatted <- formatted |>
      insert_date_local(stations_meta = known_stations, by = id_col)
  }
  return(formatted)
}

widen_with_units <- function(obs, unit_col, value_col, name_col, desired_cols) {
  obs |>
    dplyr::group_by(.unit = get(unit_col)) |>
    dplyr::select(-dplyr::all_of(unit_col)) |>
    dplyr::group_split() |>
    handyr::for_each(
      .join = TRUE,
      .show_progress = FALSE,
      \(dat) {
        dat |>
          dplyr::mutate(
            dplyr::across(
              dplyr::any_of(value_col),
              \(val) {
                unit <- fix_units(.data$.unit[1])
                val |>
                  as.numeric() |>
                  units::set_units(unit, mode = "standard")
              }
            )
          ) |>
          dplyr::select(-".unit") |>
          tidyr::pivot_wider(
            names_from = name_col,
            values_from = value_col
          ) |>
          dplyr::select(dplyr::any_of(desired_cols)) |>
          dplyr::distinct()
      }
    ) |>
    dplyr::select(dplyr::any_of(names(desired_cols)))
}

drop_missing_obs_rows <- function(obs, where_fn = is.numeric) {
  obs |>
    dplyr::filter(
      rowSums(!is.na(dplyr::across(dplyr::where(!!where_fn)))) > 0
    )
}

standardize_obs_units <- function(obs, default_units, input_units = NULL) {
  cols_to_convert <- is.null(input_units) |>
    ifelse(
      yes = names(default_units),
      no = names(input_units)
    )
  obs |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(cols_to_convert),
        \(x) {
          in_unit <- is.null(input_units) |>
            ifelse(
              yes = units(x) |> as.character(),
              no = input_units[names(input_units) == dplyr::cur_column()]
            )
          x |>
            handyr::convert_units(
              from = in_unit,
              to = default_units[
                names(default_units) == dplyr::cur_column()
              ],
              keep_units = TRUE
            )
        }
      )
    )
}

extract_options <- function(session, html_id) {
  options <- session |>
    rvest::html_nodes(paste0("select[name='", html_id, "'] option"))
  # Extract operator names and keys from options
  option_names <- options |>
    rvest::html_text()
  options <- options |>
    rvest::html_attr("value") |>
    as.numeric() |>
    suppressWarnings() |>
    stats::setNames(option_names)
  is_place_holder <- option_names == "--- SELECT ---"
  options[!is_place_holder]
}

# TODO: move to handyr
get_session_token <- function(session) {
  session |>
    rvest::html_node("input[name='__RequestVerificationToken']") |>
    rvest::html_attr("value")
}

simulate_session <- function(site, endpoint) {
  site |>
    paste0(endpoint) |>
    httr::GET() |>
    httr::content(as = "text", encoding = "UTF-8") |>
    rvest::read_html()
}

standardize_input_vars <- function(variables, all_variables = NULL) {
  variables <- tolower(variables)

  if ("all" %in% variables & !is.null(all_variables)) {
    return(all_variables)
  }

  variables[variables == "pm2.5"] <- "pm25"
  variables[variables == "humidity"] <- "rh"
  variables[variables == "temperature"] <- "temp"
  variables[variables == "wdir"] <- "wd"
  variables[variables == "wspd"] <- "ws"

  if (!is.null(all_variables)) {
    variables <- variables[variables %in% all_variables]
  }

  if (length(variables) == 0) {
    stop("No valid variables specified.")
  }

  return(variables)
}

fix_units <- function(units) {
  dplyr::case_when(
    units %in% c("% RH", "percent") ~ "%",
    units %in% c("\xb0C", "deg c", "c") ~ "degC",
    units %in% c("Deg.", "deg", "Deg") ~ "degrees",
    TRUE ~ units
  )
}
