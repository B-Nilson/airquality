#' Download air quality station metadata from the Alberta (Canada) Government
#'
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
#' A tibble of station metadata for Alberta FEMs.
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
get_abgov_stations = function(use_sf = FALSE){
  # Define endpoint
  api_endpoint = "Stations?"

  # Make request
  stations = paste0(ab_api_site, api_endpoint) |>
    parse_abgov_api_request()

  # Standardize column names
  stations = dplyr::select(stations,
    site_id = "Abbreviation",
    site_name = "Name",
    type = "Type",
    description = "Description",
    address = "Address",
    airshed = "AirshedName",
    lat = "Latitude",
    lng = "Longitude",
    elev = "Elevation")

  # Fix data types
  stations = stations |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(col)
      ifelse(col %in% c("Not Available", "Unknown"), NA, col))) |>
    dplyr::mutate(dplyr::across(c("lat", "lng", "elev"), as.numeric)) |>
    # Lookup local timezones
    dplyr::mutate(tz_local = get_station_timezone(.data$lng, .data$lat))

  # Convert to spatial if desired
  if(use_sf) stations = sf::st_as_sf(stations, coords = c("lng", "lat"))

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
#' @param verbose (Optional) A single logical (TRUE or FALSE) value indicating if
#' non-critical messages/warnings should be printed
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
#' get_abgov_data("Calgary Southeast", c("2024-01-05 00", "2024-01-05 23"))
#' }
get_abgov_data = function(stations, date_range, raw = FALSE, verbose = TRUE){
  # Output citation message to user
  if(verbose) data_citation("ABgov")

  date_range = lubridate::with_tz(date_range, abgov_tzone) # Correct? Or is it UTC time? DST?

  # Handle date_range inputs
  min_date = lubridate::ymd_h("1970-01-01 00", tz = abgov_tzone) # TODO: determine actual min date
  max_date = lubridate::floor_date(lubridate::with_tz(Sys.time(), "UTC"), "hours")
  date_range = handle_date_range(date_range, min_date, max_date)

  # Get all stations during period
  known_stations = get_abgov_stations()

  # Handle if any/all don't exist in meta data
  check_stations_exist(stations, known_stations$site_name, source = "the AB Gov. site")

  # Define endpoint
  api_endpoint = "StationMeasurements?"

  # Columns to retrieve
  data_cols = c("Value", "StationName",
                "ParameterName", "ReadingDate")

  # Build station filters (max 10 stations at a time)
  station_filters = sapply(seq(1, length(stations), 10), \(s){
    end = ifelse(s + 10 > length(stations), length(stations), s + 10)
    paste0(
    "(indexof('", stations[s:end] |>
      paste0(collapse = "', StationName) ge 0 or indexof('"),
    "', StationName) ge 0)")
  })

  # Build date filter
  starts = seq(date_range[1] -lubridate::hours(1), date_range[2], "3 days")
  ends = starts + lubridate::days(3)
  ends[ends > date_range[2]] = date_range[2]
  date_filters = sapply(1:length(starts), \(i) paste0(
    "(ReadingDate ge datetime'", format(starts[i], "%FT%T"),
    "' and ReadingDate le datetime'", format(ends[i], "%FT%T"),
    "')"
  ))

  # Combine arguments
  args = sapply(station_filters, \(station_filter) {
    sapply(date_filters, \(date_filter) {
      c(
        paste0("select=", paste0(data_cols, collapse = ",")),
        paste0("$filter=", paste(station_filter, date_filter, sep = " and ")) |>
          paste(" and indexof('Fine Particulate Matter', ParameterName) ge -1")
      ) %>% paste0(collapse = "&") |> utils::URLencode()
    })
  }) %>% as.character() |> unname()

  # Make request
  stations_data = paste0(ab_api_site, api_endpoint, args) %>%
    lapply(parse_abgov_api_request) |> dplyr::bind_rows()

  # Error if no data retrieved
  if(nrow(stations_data) == 0){
    stop("No data available for provided stations and date_range")
  }

  stations_data = stations_data |>
    # Convert dates, add quality assured column (unknown at the moment)
    # TODO: determine if/what QA/QC'ed
    dplyr::mutate(date_utc = lubridate::ymd_hms(.data$ReadingDate, tz = abgov_tzone) %>%
                    lubridate::with_tz("UTC"),
                  quality_assured = NA) |>
    # Drop erroneous columns
    dplyr::select(-"DeterminantParameterName", -"ReadingDate", -"Id") |>
    # Filter data to desired date range
    dplyr::filter(.data$date_utc |> dplyr::between(date_range[1], date_range[2])) |>
    # Long to wide, sort
    tidyr::pivot_wider(names_from = "ParameterName", values_from = "Value") |>
    dplyr::arrange("StationName", "date_utc") |>
    # Drop duplicated dates for a particular station
    dplyr::filter(!duplicated(.data$date_utc), .by = "StationName") |>
    # Convert to numeric
    dplyr::mutate(dplyr::across(-(1:3), as.numeric)) %>%
    # Rename and select desired columns
    standardize_colnames(abgov_col_names, raw = raw) %>%
    # Convert date_local to local time
    dplyr::left_join(known_stations |> dplyr::select("site_name", "tz_local"),
                     by = "site_name") |>
    dplyr::rowwise() |>
    dplyr::mutate(date_local = lubridate::with_tz(.data$date_utc, .data$tz_local) |>
                    format("%F %H:%M %z")) |>
    dplyr::ungroup() |>
    dplyr::relocate("date_local", .after = "date_utc") %>%
    dplyr::select(-"tz_local") |>
    tibble::as_tibble()

  return(stations_data)
}

parse_abgov_api_request = function(api_request){
  api_request = api_request |>
    xml2::read_xml() |>
    xml2::as_list()
  api_request$feed[-(1:4)] |>
    lapply(\(entry){
      e = unlist(entry$content$properties)
      # TODO: handle no data causing error here `get_abgov_data("Calgary Southeast", c("2021-01-05 00", "2021-01-05 23"))`
      data.frame(t(e))
    }) %>%
    dplyr::bind_rows()
}

## AB MoE Helpers ---------------------------------------------------------

ab_api_site = "https://data.environment.alberta.ca/Services/AirQualityV2/AQHI.svc/"

## Endpoints we care about
# Data: StationMeasurements?
# Stations: Stations?

abgov_tzone = "America/Edmonton"

abgov_col_names = c(
  # Meta
  date_utc = "date_utc", # Added by get_abgov_data()
  # date_local = "DATE_PST",
  site_id = "EMS_ID",
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
  # precip_1hr_mm = "PRECIP",
  # snowDepth_1hr_cm = "SNOW",
  # pressure_1hr_kpa = "PRESSURE", # TODO: Ensure pressure proper units ....
  # vapourPressure_1hr_kpa = "VAPOUR",
  solar_1hr_wm2 = "Solar Radiation"
)