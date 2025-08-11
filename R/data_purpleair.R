get_purpleair_data <- function(
  stations,
  date_range,
  api_key,
  raw = FALSE,
  verbose = TRUE
) {}

get_purpleair_stations <- function(
  date_range,
  api_key,
  bounds,
  use_sf = FALSE
) {}
#' Interface with the PurpleAir API
#'
#' @param read_key (Optional) A single character value of your PurpleAir API read key (see develop.purpleair.com). If NULL, write_key must be provided.
#' @param write_key (Optional) A single character value of your PurpleAir API write key (see develop.purpleair.com). If NULL, read_key must be provided.
#' @param channel A single character value indicating what API channel to use (Currently supported: keys, organization, sensors, groups)
#' @param parameters (Optional) A named list containing paramaters to be supplied to the API request (see api.purpleair.com).
#' @param verbose (Optional) A single logical (TRUE or FALSE) value indicating if
#' non-critical messages/warnings should be printed.
#'
#' @md
#' @description
#' PurpleAir provides access to the observations from their network of low-cost PM2.5 monitors through a points-based API system.
#' New accounts start with 1M points, and each request has a cost from 1-5 points plus the number of rows times the total column cost
#' (which depends on the column, typically 1-2 points per column).
#'  Point can be purchased on develop.purpleair.com and cost less if buying in bulk.
#'
#' For more information, see: api.purpleair.com
#'
#' In order to retrieve these data you must:
#' * make an account on develop.purpleair.com (a google account is required, which can be made using any email)
#' * create a new project, click on it after it is created
#' * add two API keys, 1 for reading (getting data), 1 for writing (making groups of sensors)
#' * (Optional) open your .Renviron for editing (you can use `usethis::edit_r_environ("project")` after installing the usethis package)
#' * (Optional) add your keys on 2 seperate lines in this format: `purpleair_api_read = "YOUR-API-KEY-HERE"`
#' * (Optional) refer to keys using `read_key = Sys.getenv("purpleair_api_read")` to keep your API key secure
#'
#' `purpleair_api()` requires either a read or write key (depending on what you are trying to do), a channel name to make a request to,
#' and (depending on the request type you want to make) a named list of parameters.
#' See api.purpleair.com for which parameters are avalailable for each request type.
#' Based on the provided channel/parameters, the request and estimated points cost will be displayed,
#' and the request will be made if desired. Set `verbose` to `FALSE` to silence this and always run the request
#' (caution, some requests may cost a lot of points)
#'
#' @return
#' If channel either "keys" or "organization", or invalid/missing parameters, then a list with the request results
#' If sensors or groups channel, then a tibble with the data returned from the request
#'
#' @family Data Collection
#' @family Canadian Air Quality
#' @family USA Air Quality
#' #'
#' @export
#' @examples
#' \dontrun{
#' read_key <- Sys.getenv("purpleair_api_read")
#' write_key <- Sys.getenv("purpleair_api_write")
#'
#' parameters <- list( # see api.purpleair.com for more
#'   nwlat = 53.93, nwlng = -122.77,
#'   selat = 53.9, selng = -122.73,
#'   fields = "temperature",
#'   sensor_index = 198385,
#'   start_timestamp = Sys.time() - lubridate::hours(1)
#' )
#'
#'
#' # Get Sensors Data
#' # test = purpleair_api(read_key = read_key, channel = "sensors",
#' #   parameters = parameters[1:5])
#'
#' # Get Sensor Data
#' # test = purpleair_api(read_key = read_key, channel = "sensors",
#' #  parameters = parameters[5:6])
#'
#' # Get Sensor History
#' # test = purpleair_api(read_key = read_key, channel = "sensors",
#' # parameters = parameters[5:7])
#' }
purpleair_api <- function(
  read_key = NULL,
  write_key = NULL,
  channel,
  parameters = NULL,
  verbose = TRUE,
  estimate_cost = TRUE
) {
  api_url <- "https://api.purpleair.com/v1/"
  # Handle inputs
  stopifnot(is.null(read_key) | is.character(read_key))
  stopifnot(is.null(write_key) | is.character(write_key))
  stopifnot(is.character(channel))
  stopifnot(
    "Either read_key or write_key must be provided." = !is.null(read_key) |
      !is.null(write_key)
  )
  stopifnot(
    "Either read_key or write_key must be provided, not both" = is.null(
      read_key
    ) |
      is.null(write_key)
  )
  if (!channel %in% purpleair_api_channels) {
    stop(paste(
      "channel must be one of:",
      paste(purpleair_api_channels, collapse = ", ")
    ))
  }
  if (channel %in% c("organization", "sensors")) {
    if (is.null(read_key)) {
      stop(paste(channel, "channel is read only"))
    }
  }

  # Handle parameters
  if ("fields" %in% names(parameters)) {
    parameters$fields <- parameters$fields |>
      paste0(collapse = ",")
  }
  if ("start_timestamp" %in% names(parameters)) {
    parameters$start_timestamp <- parameters$start_timestamp |>
      lubridate::as_datetime() |>
      as.numeric()
  }
  if ("end_timestamp" %in% names(parameters)) {
    parameters$end_timestamp <- parameters$end_timestamp |>
      lubridate::as_datetime() |>
      as.numeric()
  }

  # Determine request that will be made
  api_key <- ifelse(!is.null(read_key), read_key, write_key)
  request_name <- api_key |>
    determine_purpleair_request(channel, parameters)
  if (verbose) {
    message(purpleair_request_msg(channel, request_name))
  }
  endpoint <- purpleair_api_endpoints[[channel]][[request_name]]

  # Insert values into endpoint if needed
  if (!is.null(parameters$sensor_index)) {
    endpoint <- endpoint |>
      stringr::str_replace(
        ":sensor_index",
        as.character(parameters$sensor_index[1])
      )
  }
  if (!is.null(parameters$group_id)) {
    endpoint <- endpoint |>
      stringr::str_replace(":group_id", parameters$group_id)
  }
  if (!is.null(parameters$member_id)) {
    endpoint <- endpoint |>
      stringr::str_replace(":member_id", parameters$member_id)
  }

  # Estimate expected points cost (BETA)
  if (estimate_cost) {
    expected_cost <- request_name |>
      purpleair_points_estimator(parameters, verbose) |>
      handyr::on_error(.return = -1)

    if (!is.na(expected_cost)) {
      if (expected_cost > 0) {
        if (expected_cost == -1) {
          query <- "Unable to estimate points cost. Would you like to continue? (y/n): "
        } else {
          query <- "Would you like to continue? (y/n): "
        }
        keep_going <- readline(query) |>
          stringr::str_to_lower() %in%
          c("y", "yes")
        if (!keep_going) stop("User requested to exit.")
      }
    }
  }

  # Make the request
  api_response <- httr::GET(
    paste0(api_url, endpoint),
    httr::add_headers("X-API-Key" = api_key),
    query = parameters
  ) |>
    httr::content()

  response_timestamp <- api_response$data_time_stamp |>
    lubridate::as_datetime(tz = "UTC") |>
    handyr::on_error(.return = NA)
  if ("data" %in% names(api_response)) {
    if (length(api_response$data) == 0) {
      stop("No data available for the desired sensors/period")
    }
    response_fields <- unlist(api_response$fields)

    pa_data <- api_response$data |>
      handyr::for_each(
        .bind = TRUE,
        \(entry) {
          entry |>
            stats::setNames(response_fields)
        }
      ) |>
      dplyr::select(dplyr::all_of(response_fields))

    if (!"sensor_index" %in% names(pa_data)) {
      pa_data$sensor_index <- api_response$sensor_index
    }
    if (!"time_stamp" %in% names(pa_data)) {
      pa_data$time_stamp <- response_timestamp
    }
  } else if ("sensor" %in% names(api_response)) {
    pa_data <- api_response$sensor |>
      dplyr::as_tibble() |>
      dplyr::mutate(time_stamp = response_timestamp)
  }else {
    return(api_response)
  }

  pa_data |>
    dplyr::relocate(c("sensor_index", "time_stamp"), .before = 1) |>
    dplyr::arrange(.data$sensor_index, .data$time_stamp)
}

purpleair_api_channels <- c("sensors", "groups", "organization", "keys")

purpleair_api_endpoints <- list(
  keys = list(
    "Check API Key" = "keys"
  ),
  organization = list(
    "Get Organization Data" = "organization"
  ),
  sensors = list(
    "Get Sensor Data" = "sensors/:sensor_index",
    "Get Sensor History" = "sensors/:sensor_index/history",
    "Get Sensors Data" = "sensors"
  ),
  groups = list(
    "Create Group" = "groups",
    "Create Member" = "groups/:group_id/members",
    "Delete Group" = "groups/:group_id",
    "Delete Member" = "groups/:group_id/members/:member_id",
    "Get Group Detail" = "groups/:group_id",
    "Get Groups List" = "groups",
    "Get Member Data" = "groups/:group_id/members/:member_id",
    "Get Member History" = "groups/:group_id/members/:member_id/history/csv",
    "Get Members Data" = "groups/:group_id/members"
  )
)

determine_purpleair_request <- function(write_key, channel, parameters) {
  p_names <- names(parameters)

  # Organization-specific requests
  if (channel %in% c("keys", "organization")) {
    request_name <- names(purpleair_api_endpoints[[channel]])[1]
  }

  # Sensor-specific requests
  if (channel == "sensors") {
    request_name <- dplyr::case_when(
      # sensor_index not provided
      !"sensor_index" %in% p_names ~ "Get Sensors Data",
      # sensor_index and a start/end date provided
      "start_timestamp" %in% p_names | "end_timestamp" %in% p_names ~
        "Get Sensor History",
      # sensor_index provided but not dates
      "sensor_index" %in% p_names ~ "Get Sensor Data"
    )
  }

  # Group-specific requests
  if (channel == "groups") {
    if (!is.null(write_key)) {
      # If write key provided
      request_name <- dplyr::case_when(
        # group_id not provided
        !"group_id" %in% p_names ~ "Create Group",
        # group id provided but not other ids
        !any(c("sensor_index", "sensor_id", "member_id") %in% p_names) ~
          "Delete Group",
        # group_id and member_id provided
        "member_id" %in% p_names ~ "Delete Member",
        c("sensor_index", "sensor_id") %in% p_names ~ "Create Member"
      )
    } else {
      # If read key provided
      request_name <- dplyr::case_when(
        # group_id not provided
        !"group_id" %in% p_names ~ "Get Groups List",
        # group_id and fields provided
        "fields" %in% p_names ~ "Get Members Data",
        # group_id provided but not fields nor member_id
        !"member_id" %in% p_names ~ "Get Group Detail",
        # group_id, member_id, and start/end date provided
        any(c("start_timestamp", "end_timestamp") %in% p_names) ~
          "Get Member History",
        # group_id and member_id provided but no dates
        "member_id" %in% p_names ~ "Get Member Data"
      )
    }
  }
  return(request_name)
}

purpleair_request_msg <- function(channel, request) {
  sanitized_request <- request |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(" ", "_") |>
    paste(collapse = "-")
  url <- "https://api.purpleair.com/#api-" |>
    paste0(channel, "-", sanitized_request)
  "Make a request to `%s` (See: %s)" |>
    sprintf(request, url)
}

purpleair_points_costs <- list(
  # If row cost is NA, cost is sum of requested field costs
  endpoints = list(
    get_organization_data = c(request = 0, row = 0),
    check_api_key = c(request = 0, row = 0),
    get_sensor_data = c(request = 1, row = NA),
    get_sensors_data = c(request = 5, row = NA),
    get_sensor_history = c(request = 2, row = NA),
    create_member = c(request = 1, row = 1),
    delete_member = c(request = 1, row = 0),
    get_member_data = c(request = 1, row = NA),
    get_member_history = c(request = 2, row = NA),
    get_members_data = c(request = 3, row = NA),
    create_group = c(request = 1, row = 0),
    delete_group = c(request = 1, row = 0),
    get_group_detail = c(request = 1, row = 3),
    get_groups_list = c(request = 4, row = 3)
  ),
  # (any other fields cost 1 point)
  fields = list(
    sensor_index = 0,
    humidity = 2,
    temperature = 2,
    pressure = 2,
    voc = 2,
    scattering_coefficient = 2,
    deciviews = 2,
    visual_range = 2,
    "0.3_um_count" = 2,
    "0.5_um_count" = 2,
    "1.0_um_count" = 2,
    "2.5_um_count" = 2,
    "5.0_um_count" = 2,
    "10.0_um_count" = 2,
    pm1.0 = 2,
    pm1.0_cf_1 = 2,
    pm1.0_atm = 2,
    pm2.5_10minute = 2,
    pm2.5_30minute = 2,
    pm2.5_60minute = 2,
    pm2.5_6hour = 2,
    pm2.5_24hour = 2,
    pm2.5_1week = 2,
    pm2.5 = 2,
    pm2.5_atm = 2,
    pm2.5_cf_1 = 2,
    pm2.5_alt = 2,
    pm10.0 = 2,
    pm10.0_atm = 2,
    pm10.0_cf_1 = 2
  )
)

# See https://community.purpleair.com/t/loop-api-calls-for-historical-data/4623
purpleair_request_limits <- list(
  avg_0 = 30 * 24 * 30,
  avg_10 = 6 * 24 * 60,
  avg_30 = 2 * 24 * 90,
  avg_60 = 1 * 24 * 180,
  avg_360 = 1 / 6 * 24 * 365,
  avg_1440 = 1 / 24 * 24 * 730,
  avg_10080 = 1 / 24 / 7 * 24 * 1825,
  avg_43200 = 1 / 24 / 30 * 24 * 7300,
  avg_525600 = 1 / 24 / 365 * 24 * 36500
)

purpleair_points_estimator <- function(request, parameters, verbose = FALSE) {
  request_costs <- purpleair_points_costs$endpoints[[
    stringr::str_to_lower(request) |> stringr::str_replace_all(" ", "_")
  ]]

  if (!is.na(request_costs[2])) {
    row_costs <- request_costs[2]
  } else {
    if (!"fields" %in% names(parameters)) {
      stop("Parameter `fields` must be provided.")
    }
    fields <- stringr::str_split(parameters$fields, ",")[[1]]
    row_costs <- (fields %in% names(purpleair_points_costs$fields)) |>
      ifelse(purpleair_points_costs$fields[fields], 1) |>
      unlist() |>
      sum()
  }

  if (request %in% c("Get Sensors Data", "Get Members Data")) {
    if (verbose) {
      message(paste0(
        "It is difficult to estimate the points cost of `",
        request,
        "` as it depends on the number of sensors in the request. ",
        "Points used will be equal to ",
        request_costs[1],
        " + ",
        row_costs,
        " * n_sensors"
      ))
    }
    if (
      request == "Get Sensors Data" & !"nwlat" %in% names(parameters) & verbose
    ) {
      message(
        "This request may cost a significant amount of points (100k+) given that a bounding box is not supplied"
      )
      return(Inf)
    }
    return(NA)
  }

  if (!"average" %in% names(parameters)) {
    parameters$average <- 10
  }
  avg_name <- paste0("avg_", parameters$average)
  if (!avg_name %in% names(purpleair_request_limits)) {
    stop(paste(
      "`average` must be one of",
      paste(
        names(purpleair_request_limits) |> stringr::str_remove("avg_"),
        collapse = ", "
      )
    ))
  }
  max_rows <- purpleair_request_limits[[avg_name]]

  if (request %in% c("Get Sensor Data", "Get Member Data")) {
    n_rows <- 1
  } else if (
    all(c("start_timestamp", "end_timestamp") %in% names(parameters))
  ) {
    start <- lubridate::as_datetime(parameters$start_timestamp)
    end <- lubridate::as_datetime(parameters$end_timestamp)
    n_rows <- start |>
      seq(to = end, by = paste(parameters$average, "mins")) |>
      length()
  } else if ("start_timestamp" %in% names(parameters)) {
    start <- parameters$start_timestamp |>
      lubridate::as_datetime() |>
      lubridate::with_tz("UTC")
    end <- Sys.time() |>
      lubridate::with_tz("UTC")
    n_rows <- start |>
      seq(to = end, by = paste(parameters$average, "mins")) |>
      length()
  } else if ("end_timestamp" %in% names(parameters)) {
    n_rows <- max_rows
  } else {
    n_rows <- 1
  }

  if (n_rows > max_rows) {
    n_rows <- max_rows
  }
  total_cost <- request_costs[1] + row_costs * n_rows
  if (verbose & total_cost > 0) {
    message(paste("This is estimated to cost up to", total_cost, "points."))
  }
  return(total_cost)
}
