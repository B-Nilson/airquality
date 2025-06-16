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
  placeholders <- c("Not Available", "Unknown")
  numeric_cols <- c("lat", "lng", "elev")

  # Get station metadata from the AB gov API
  api_endpoint <- "Stations"
  stations <- ab_api_site |>
    paste0(api_endpoint, "?") |>
    parse_abgov_api_request()

  # Standardize formatting
  stations <- stations |>
    tibble::tibble() |>
    dplyr::select(dplyr::any_of(header)) |>
    remove_na_placeholders(na_placeholders = placeholders) |>
    dplyr::filter(!is.na(.data$lat), !is.na(.data$lng)) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(numeric_cols), as.numeric),
      tz_local = .data$lng |> handyr::get_timezone(lat = .data$lat)
    )

  # Convert to spatial if desired
  if (use_sf) {
    rlang::check_installed("sf")
    stations <- stations |>
      sf::st_as_sf(coords = c("lng", "lat"), crs = abgov_crs) |>
      sf::st_transform(crs = "WGS84")
  }

  return(stations)
}

#' Download air quality station observations from the Alberta (Canada) Government
#'
#' @param stations A character vector of one or more station names to try and get data desired for (see [get_abgov_stations()]).
#' @param date_range A datetime vector (or a character vector with UTC dates in "YYYY-MM-DD HH" format) with either 1 or 2 values.
#' Providing a single value will return data for that hour only,
#' whereas two values will return data between (and including) those times.
#' Dates are "backward-looking", so a value of "2019-01-01 01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00".
#' @param raw (Optional) A single logical (TRUE or FALSE) value indicating
#' if raw data files desired (i.e. without a standardized format). Default is FALSE.
#' @param fast (Optional) A single logical (TRUE or FALSE) value indicating if time-intensive code should be skipped where possible.
#' Default is FALSE.
#' @param verbose (Optional) A single logical (TRUE or FALSE) value indicating if
#' non-critical messages/warnings should be printed
#' @param stations_per_call (Optional) A single numeric value indicating the maximum number of stations to request per API call.
#' The API header requires station names to be passed as a comma-separated list, too manyu stations may cause an eror depending on station name length.
#' Default is 1.
#' @param days_per_call (Optional) A single numeric value indicating the maximum number of days (per station) to request per API call.
#' This is a safety measure to prevent the API from timing out by requesting too many days at once.
#' Default is 90.
#'
#' @description
#' Air pollution monitoring in Canada is done by individual Provinces/Territories,
#' primarily as a part of the federal National Air Pollution Surveillance (NAPS) program.
#' The Province of Alberta hosts it's hourly air quality observations
#' through a public API, providing both historic and real-time raw data.
#'
#' [get_abgov_data()] provides an easy way to retrieve these observations using
#' station name(s) (see [get_abgov_stations()]) and a specified date or date range.
#'
#' @seealso [get_abgov_stations()]
#' @return
#' A tibble of hourly observation data for desired station(s) and date range where available.
#' Columns returned will vary depending on available data from station(s).
#'
#' Dates UTC time and are "backward-looking", so a value of "2019-01-01 01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00".
#' @family Data Collection
#' @family Canadian Air Quality
#'
#' @export
#' @examples
#' \donttest{
#' get_abgov_data(
#'   stations = "Calgary Southeast",
#'   date_range = c("2024-01-05 00", "2025-01-05 23")
#' )
#' }
get_abgov_data <- function(stations, date_range, raw = FALSE, fast = FALSE, verbose = TRUE, stations_per_call = 1, days_per_call = 90) {
  # Output citation message to user
  if (verbose) data_citation("ABgov")

  # Handle date_range inputs
  min_date <- "1970-01-01 00" |> # TODO: determine actual min date
    lubridate::ymd_h(tz = abgov_tzone)
  max_date <- Sys.time() |>
    lubridate::floor_date("hours")
  date_range <- date_range |>
    handle_date_range(min_date, max_date) |>
    lubridate::with_tz(abgov_tzone)

  # Only get data for stations that exist on the API
  if (!fast) {
    known_stations <- get_abgov_stations()
    stations <- stations |>
      check_stations_exist(
        known_stations$site_name,
        source = "the AB Gov. site"
      )
    if (length(stations) == 0) {
      stop("No data available for provided station(s)")
    }
  }

  # Make request(s) as needed to load all desired data
  api_endpoint <- "StationMeasurements"
  args <- stations |>
    build_abgov_data_args(
      date_range,
      stations_per_call = stations_per_call,
      days_per_call = days_per_call
    )
  stations_data <- ab_api_site |>
    paste0(api_endpoint, "?", args) |>
    handyr::for_each(
      parse_abgov_api_request,
      .as_list = TRUE, .bind = TRUE
    )

  # Handle no data or raw return
  no_values <- nrow(stations_data) == 0 |
    !"Value" %in% names(stations_data)
  if (no_values) {
    stop("No data available for provided stations and date_range")
  } else if (raw) {
    return(stations_data)
  }

  # Standardise data formatting
  id_cols <- c("site_name", "date_utc", "quality_assured")
  pivot_cols <- c("ParameterName", "Value")
  stations_data <- stations_data |>
    tibble::as_tibble() |>
    # Convert dates, add QA/QC placeholder, and filter to desired range
    dplyr::mutate(
      date_utc = .data$ReadingDate |>
        lubridate::ymd_hms(tz = abgov_tzone) |>
        lubridate::with_tz("UTC"),
      quality_assured = NA # TODO: determine if/what QA/QC'ed
    ) |>
    dplyr::select(-dplyr::any_of(
      c("DeterminantParameterName", "ReadingDate", "Id")
    )) |>
    dplyr::filter(
      .data$date_utc |>
        dplyr::between(date_range[1], date_range[2])
    ) |>
    # Long -> wide, fix column names
    tidyr::pivot_wider(
      names_from = pivot_cols[1], values_from = pivot_cols[2]
    ) |>
    dplyr::select(dplyr::any_of(abgov_col_names)) |>
    # Convert non-id columns to numeric
    dplyr::mutate(
      dplyr::across(-dplyr::any_of(id_cols), as.numeric)
    ) |>
    # Order by site and date
    dplyr::arrange(.data$site_name, .data$date_utc) |>
    # Remove duplicated entries
    dplyr::distinct()
  # Insert local time (slow-ish for many stations)
  if (!fast) {
    stations_data <- stations_data |>
      insert_date_local(stations_meta = known_stations)
  }
  return(stations_data)
}

## AB MoE Helpers ---------------------------------------------------------

ab_api_site <- "https://data.environment.alberta.ca/Services/AirQualityV2/AQHI.svc/"

abgov_tzone <- "America/Edmonton" # TODO: confirm this
abgov_crs <- "WGS84" # TODO: confirm this

# TODO: check if there are more values available
abgov_col_names <- c(
  # Meta
  date_utc = "date_utc", # Added by get_abgov_data()
  site_name = "StationName",
  quality_assured = "quality_assured", # Added by get_abgov_data()
  # Particulate Matter
  pm25_1hr_ugm3 = "Fine Particulate Matter",
  # pm10_1hr_ugm3 = "PM10",
  # # Ozone
  o3_1hr_ppb = "Ozone",
  # # Nitrogen Pollutants
  no_1hr_ppb = "Nitric Oxide",
  no2_1hr_ppb = "Nitrogen Dioxide",
  nox_1hr_ppb = "Total Oxides of Nitrogen",
  nh3_1hr_ppb = "Ammonia",
  # # Sulfur Pollutants
  so2_1hr_ppb = "Sulphur Dioxide",
  trs_1hr_ppb = "Total Reduced Sulphur",
  h2s_1hr_ppb = "Hydrogen Sulphide",
  # # Carbon Monoxide
  co_1hr_ppb = "Carbon Monoxide",
  # Methane
  ch4_1hr_ppb = "Methane",
  # Hydrocarbons
  hc_1hr_ppb = "Total Hydrocarbons",
  hcnm_1hr_ppb = "Non-methane Hydrocarbons",
  # # Met data
  rh_1hr_percent = "Relative Humidity",
  t_1hr_celcius = "Outdoor Temperature",
  wd_1hr_degrees = "Wind Direction",
  ws_1hr_ms = "Wind Speed",
  solar_1hr_wm2 = "Solar Radiation"
)

build_abgov_data_args <- function(stations, date_range, stations_per_call = 3, days_per_call = 3) {
  # Build station filter(s)
  station_filters <- seq(1, length(stations), stations_per_call) |> 
    sapply(\(s){
      is_past_n <- (s + stations_per_call) > length(stations)
      end <- !is_past_n |> ifelse(
        s + stations_per_call,
        length(stations)
      )
      prefix <- "(indexof('"
      seperator <- "', StationName) ge 0 or indexof('"
      suffix <- "', StationName) ge 0)"
      s_query <- stations[s:end] |> paste0(collapse = seperator)
      paste0(prefix, s_query, suffix)
    })
  # Build date filter(s)
  steps <- paste(days_per_call, "days")
  starts <- (date_range[1] - lubridate::hours(1)) |>
    seq(date_range[2], steps)
  ends <- starts + lubridate::days(days_per_call)
  ends[ends > date_range[2]] <- date_range[2]
  date_filters <- 1:length(starts) |> sapply(\(i){
    s <- starts[i] |> format("%FT%T")
    e <- ends[i] |> format("%FT%T")
    prefix <- "(ReadingDate ge datetime'"
    seperator <- "' and ReadingDate le datetime'"
    suffix <- "')"
    paste0(prefix, s, seperator, e, suffix)
  })
  # Combine arguments
  station_filters |>
    sapply(\(station_filter) date_filters |> sapply(\(date_filter) {
      column_filter <- "select=" |> paste0(
        c("Value", "StationName", "ParameterName", "ReadingDate") |>
          paste0(collapse = ",")
      )
      # Parameter filter is required, so do a dummy one that allows for any param
      param_filter <- "indexof('Fine Particulate Matter', ParameterName) ge -1"
      filter_query <- station_filter |>
        paste(date_filter, param_filter, sep = " and ")
      column_filter |>
        paste0("&", "$filter=" |> paste0(filter_query)) |>
        paste0("&Connection Timeout=3600")
    })) |>
    unlist() |>
    utils::URLencode(reserved = TRUE) |>
    stringr::str_replace_all("%3D", "=") |>
    stringr::str_replace_all("%2C", ",") |>
    stringr::str_replace_all("%26", "&") |>
    unname()
}

parse_abgov_api_request <- function(api_request) {
  print(api_request)
  api_request <- api_request |>
    xml2::read_xml() |>
    xml2::as_list() |>
    # server sends 400 error when no data in query
    handyr::on_error(.return = NULL)
  
  if (is.null(api_request)) return(NULL)

  api_request$feed[-(1:4)] |>
    lapply(\(entry){
      e <- unlist(entry$content$properties)
      if (!is.null(e)) data.frame(t(e)) else e
    }) |>
    dplyr::bind_rows()
}

abgov_data_mgmt_api <- function(api_request) {
  operator_keys <- c(6) |>
    paste(collapse = "%3B")
  station_keys <- c("19501") |>
    paste(collapse = "%3B")
  parameter_keys <- c(5, 10, 4, 13, 14, 12, 115, 17, 21, 83, 113, 24, 27, 15, 132, 139) |>
    paste(collapse = "%3B")
  date_range <- c("2021-01-01", "2024-12-31") |>
    lubridate::as_date() |>
    format("%F")
  verification_token <- uuid::UUIDgenerate()
  # Original: SelectedAreaOperatorKeys=6%3B&SelectedStationKeys=19501%3B&SelectedParameterKeys=5%3B10%3B4%3B13%3B14%3B12%3B115%3B17%3B21%3B83%3B113%3B24%3B27%3B15%3B132%3B139%3B&IsNoRecordFound=False&AreaOperatorKeys=6&StationKeys=19501&StartDate=2021-01-01&__Invariant=StartDate&EndDate=2024-12-31&__Invariant=EndDate&CollectionType=Continuous&ParameterKeys=5&ParameterKeys=10&ParameterKeys=4&ParameterKeys=13&ParameterKeys=14&ParameterKeys=12&ParameterKeys=115&ParameterKeys=17&ParameterKeys=21&ParameterKeys=83&ParameterKeys=113&ParameterKeys=24&ParameterKeys=27&ParameterKeys=15&ParameterKeys=132&ParameterKeys=139&IsGoodQuality=0&IncludeQaQcSamples=0&AdditionalMetadata=0&__RequestVerificationToken=CfDJ8FMRL2JQLblMkdnzpJfcax68WbDprSKN4N4wUTLI7WuO79AmwkRNONb8CLRK1wbf8pC_y4ZPXgPgEs6C-1fK-9XPQb0poo20JWfNV3zJN1SnbLixtcSZ4hOAbsLMtA5bDvu7NBHBjObUimFPzo4bOkk
  # POST with requested data args
  # GET regularly to check if request completed
  # Once, "OK" returned, GET data
  abgov_data_mgmt_api_url <- "https://datamanagementplatform.alberta.ca/Ambient/AreaOperatorAmbientMultipleParameters"
  api_request <- paste0(
    abgov_data_mgmt_api_url, "?",
    "SelectedAreaOperatorKeys=", operator_keys,
    "%3B&AreaOperatorKeys=", operator_keys, 
    "&SelectedStationKeys=", station_keys, 
    "&StationKeys=", station_keys,
    "&SelectedParameterKeys=", parameter_keys,
    "&ParameterKeys=", parameter_keys, 
    "&IsNoRecordFound=False",
    "&StartDate=", date_range[1], "&__Invariant=StartDate",
    "&EndDate=", date_range[2], "&__Invariant=EndDate",
    "&CollectionType=Continuous",
    "&IsGoodQuality=0&IncludeQaQcSamples=0&AdditionalMetadata=0",
    "&__RequestVerificationToken=", verification_token
  )
}
