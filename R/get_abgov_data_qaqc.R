get_abgov_data_qaqc <- function(
  stations = "all",
  variables = "all",
  date_range = "now",
  max_tries = 100,
  raw = FALSE,
  fast = FALSE,
  quiet = FALSE
) {
  # Decide which api endpoint to use
  mode <- dplyr::case_when(
    any(stations == "all") ~ "parameters",
    any(variables == "all") & length(stations) < 8 ~ "stations",
    length(stations) > length(variables) ~ "parameters",
    TRUE ~ "stations"
  )

  # Initiate data request(s) and download data as it becomes available
  obs <- date_range |>
    abgov_submit_qaqc_requests(
      stations = stations,
      variables = variables,
      mode = mode,
      quiet = quiet
    ) |>
    abgov_read_qaqc_data(
      max_tries = max_tries,
      quiet = quiet
    )

  if (nrow(obs) == 0) {
    stop("No data available for provided request")
  }

  if (!fast & !raw) {
    obs <- obs |>
      dplyr::left_join(
        abgov_qaqc_flags[, c("flag", "flag_name")],
        by = c("Flags" = "flag")
      )
  }

  return(obs)
}

abgov_format_qaqc_data <- function(qaqc_data, date_range, desired_cols) {
  stopifnot(is.data.frame(qaqc_data), nrow(qaqc_data) > 0)
  stopifnot(lubridate::is.POSIXct(date_range), length(date_range) == 2)
  stopifnot(is.character(desired_cols), length(desired_cols) > 0)

  qaqc_data |>
    # Remove duplicates and any missing or flagged data
    dplyr::filter(!is.na(.data$`Measurement Value`), is.na(.data$Flags)) |>
    dplyr::distinct(
      .data$StationName,
      .data$`Interval End`,
      .data$Parameter,
      .data$Unit,
      .keep_all = TRUE
    ) |>
    # Fix units, set obs date, and mark quality assured
    dplyr::mutate(
      Unit = fix_units(.data$Unit),
      ReadingDate = (.data$`Interval Start` + lubridate::hours(1)) |>
        lubridate::with_tz("UTC"),
      quality_assured = is.na(.data$Flags)
    ) |>
    dplyr::filter(
      .data$ReadingDate |> dplyr::between(date_range[1], date_range[2])
    ) |>
    # Cleanup
    widen_with_units(
      unit_col = "Unit",
      value_col = "Measurement Value",
      name_col = "Parameter",
      desired_cols = desired_cols
    ) |>
    standardize_obs_units(default_units = default_units) # TODO: move to standardize_data_format() ?
}

abgov_submit_qaqc_requests <- function(
  date_range,
  stations,
  variables,
  mode,
  quiet = FALSE
) {
  tzone <- "MST" # TODO: confirm this?
  allowed_date_range <- c("1970-01-01 00:00:00", "now") # TODO: confirm this
  now_time_step <- "1 months" # floor "now" to current month
  value_cols <- .abgov_columns$values[
    .abgov_columns$values != "Fine Particulate Matter" # Drop raw API column
  ]

  # Handle variables input
  variables <- variables |>
    standardize_input_vars(
      all_variables = names(value_cols) |>
        stringr::str_remove("_1hr")
    )

  # Get API keys for our stations/parameters
  desired_var_cols <- value_cols[
    stringr::str_remove(names(value_cols), "_1hr") %in% variables
  ]
  keys <- stations |>
    abgov_get_qaqc_keys(parameters = desired_var_cols, mode = mode)

  # Handle date_range inputs
  date_range <- date_range |>
    handyr::check_date_range(
      within = allowed_date_range,
      tz = tzone,
      now_time_step = now_time_step
    )

  # Break up date range into smaller chunks depending on duration
  date_chunks <- date_range |> pretty(n = 5)
  max_duration <- date_chunks |>
    difftime(dplyr::lag(date_chunks), units = "days") |>
    stats::median(na.rm = TRUE)
  if (max_duration > 3650) {
    max_duration <- "3650 days"
  } else if (max_duration < 30) {
    max_duration <- "30 days"
  }

  date_range |>
    handyr::split_date_range(max_duration = max_duration, as_list = TRUE) |>
    handyr::for_each(
      .show_progress = !quiet,
      \(d_range) {
        d_range <- d_range |> sapply(format, format = "%F") |> unname()
        mode_name <- ifelse(mode == "stations", "station", "parameter")
        mode_keys <- unique(keys[[mode_name]])
        mode_keys |>
          sapply(\(mode_key) {
            this <- keys[keys[[mode_name]] == mode_key, ]
            out_name <- this[[paste0(mode_name, "_name")]][1]
            station_keys <- if (mode == "stations") mode_key else this$station
            parameter_keys <- if (mode == "stations") {
              this$parameter
            } else {
              mode_key
            }
            unique(this$operator) |>
              abgov_init_data_request(
                station_keys = station_keys,
                parameter_keys = parameter_keys,
                mode = mode,
                date_range = d_range
              ) |>
              stats::setNames(out_name) |>
              handyr::on_error(.return = NULL)
          })
      }
    ) |>
    lapply(\(x) x[!sapply(x, is.null)])
}

abgov_read_qaqc_data <- function(request_tokens, max_tries, quiet) {
  request_tokens |>
    handyr::for_each(
      .bind = TRUE,
      .enumerate = TRUE,
      .show_progress = !quiet,
      \(tokens, i) {
        tokens |>
          handyr::for_each(
            .bind = TRUE,
            .enumerate = TRUE,
            .show_progress = FALSE,
            \(token, j) {
              if (is.null(token)) {
                return(NULL)
              }
              token |>
                abgov_get_qaqc_data_request(
                  max_tries = max_tries,
                  quiet = quiet
                ) |>
                abgov_parse_qaqc_data() |>
                handyr::on_error(.return = NULL)
            }
          )
      }
    )
}

abgov_init_data_request <- function(
  operator_keys,
  station_keys,
  parameter_keys,
  date_range,
  mode = "stations",
  continuous = TRUE
) {
  # Constants
  api_url <- "https://datamanagementplatform.alberta.ca/Ambient/"
  endpoint <- list(
    stations = "AreaOperatorAmbientMultipleParameters",
    parameters = "AreaOperatorAmbientMultipleStations"
  )[[mode]]

  # Handle inputs
  is_continuous <- continuous |>
    ifelse("Continuous", "Non Continuous")
  operator_keys <- unique(operator_keys)
  operator_keys <- operator_keys[!is.na(operator_keys)]
  if (length(operator_keys) == 0) {
    operator_keys <- 1:20
  }
  station_keys <- unique(station_keys)
  station_keys <- station_keys[!is.na(station_keys)]
  parameter_keys <- unique(parameter_keys)
  parameter_keys <- parameter_keys[!is.na(parameter_keys)]

  # Get session token
  session_token <- api_url |>
    simulate_session(endpoint = endpoint) |>
    get_session_token()

  # Build request
  request_body <- list(
    IsGoodQuality = 1,
    IncludeQaQcSamples = 0,
    AdditionalMetadata = 0,
    IsNoRecordFound = FALSE,
    `__RequestVerificationToken` = session_token,
    SelectedAreaOperatorKeys = operator_keys |>
      paste(collapse = ";") |>
      paste0(";"),
    SelectedAreaOperatorKey = operator_keys |> # TODO: why needed?
      paste(collapse = ";") |>
      paste0(";"),
    SelectedStationKeys = station_keys |>
      paste(collapse = ";") |>
      paste0(";"),
    SelectedParameterKeys = parameter_keys |>
      paste(collapse = ";") |>
      paste0(";"),
    ParameterKey = parameter_keys[1], # for parameters mode
    CollectionType = is_continuous,
    StartDate = date_range[1],
    `__Invariant` = "StartDate",
    EndDate = date_range[2],
    `__Invariant` = "EndDate"
  ) |>
    c(
      operator_keys |> abgov_make_key_args("AreaOperatorKeys"),
      station_keys |> abgov_make_key_args("StationKeys"),
      parameter_keys |> abgov_make_key_args("ParameterKeys")
    )
  result <- api_url |>
    paste0(endpoint) |>
    httr::POST(body = request_body, encode = "form") |>
    httr::content(as = "parsed", encoding = "UTF-8")

  if (!is.null(result$error)) {
    if (result$error != FALSE) {
      stop(result$message)
    }
  }
  result$token
}

abgov_get_qaqc_operators <- function(
  select_operators = "all",
  select_parameter = NULL,
  continuous = TRUE
) {
  api_url <- "https://datamanagementplatform.alberta.ca/Ambient/"
  endpoint_stations <- "AreaOperatorAmbientMultipleParameters"
  endpoint_parameters <- "GetAOAreaOperatorsForParameter"

  if (is.null(select_parameter)) {
    # Simulate session and extract operator options
    operators <- api_url |>
      simulate_session(endpoint = endpoint_stations) |>
      extract_options(html_id = "AreaOperatorKeys")
  } else {
    request_body <- list(
      ParameterKey = unname(select_parameter),
      CollectionType = continuous |>
        ifelse("Continuous", "Non Continuous")
    )
    operators <- api_url |>
      abgov_post_request(
        endpoint = endpoint_parameters,
        request_body = request_body
      ) |>
      unlist()
  }
  # Filter to desired operators
  if (!"all" %in% select_operators) {
    is_selected <-
      operators %in% select_operators | names(operators) %in% select_operators
    operators <- operators[is_selected]
  }
  return(operators)
}

abgov_get_qaqc_parameters <- function() {
  api_url <- "https://datamanagementplatform.alberta.ca/Ambient/"
  endpoint <- "AreaOperatorAmbientMultipleStations"

  # Simulate session and extract parameter options
  parameters <- api_url |>
    simulate_session(endpoint = endpoint) |>
    extract_options(html_id = "ParameterKey")

  # Extract parameter names
  names(parameters) <- names(parameters) |>
    stringr::str_split(pattern = " / ", simplify = TRUE) |>
    as.data.frame() |>
    dplyr::pull(1)

  return(parameters)
}

abgov_make_key_args <- function(keys, key_name) {
  keys <- unique(keys)
  names <- rep(key_name, length(keys))
  keys |> 
    as.list() |> 
    stats::setNames(names)
}

abgov_get_keys <- function(
  input_keys,
  select_keys,
  type = "stations",
  continuous = TRUE
) {
  api_url <- "https://datamanagementplatform.alberta.ca/"
  endpoint <- type |>
    switch(
      "stations" = "Ambient/GetAOStationsForAreaOperator",
      "parameters" = "Ambient/GetAOParametersForStations"
    )
  is_continuous <- continuous |>
    ifelse("Continuous", "Non Continuous")

  # Build request
  key_args <- c(
    if (type %in% c("stations", "parameters")) {
      input_keys$operators |> abgov_make_key_args("AreaOperatorKeys")
    },
    if (type == "parameters") {
      input_keys$stations |> abgov_make_key_args("StationKeys")
    }
  )
  request_body <- list(CollectionType = is_continuous) |>
    c(key_args)

  # Get key names and values
  keys <- api_url |>
    abgov_post_request(endpoint = endpoint, request_body = request_body) |>
    unlist()
  names(keys) <- stringr::str_split(
    names(keys),
    " / ",
    simplify = TRUE
  )[, 1]

  # Filter to desired keys
  if (!"all" %in% select_keys) {
    is_selected <- (names(keys) %in% select_keys) |
      keys %in% select_keys
    if (type == "stations") {
      stations_ids <- names(keys) |>
        stringr::str_split("\\|", simplify = TRUE)
      is_selected <- is_selected |
        stations_ids[, 1] %in% select_keys |
        stations_ids[, 2] %in% select_keys
    }
    keys <- keys[is_selected]
  }
  return(keys)
}

abgov_post_request <- function(api_url, endpoint, request_body) {
  api_url |>
    paste0(endpoint) |>
    httr::POST(body = request_body, encode = "form") |>
    httr::content(as = "parsed", encoding = "UTF-8") |>
    dplyr::bind_rows() |>
    dplyr::select(name = "text", "value") |>
    dplyr::mutate(value = as.numeric(.data$value)) |>
    tidyr::pivot_wider()
}

abgov_get_qaqc_station_params <- function(
  operator_keys,
  station_keys,
  select_params = "all",
  continuous = TRUE
) {
  api_url <- "https://datamanagementplatform.alberta.ca/"
  endpoint <- "Ambient/GetAOParametersForStations"

  # Build request
  key_args <- c(
    operator_keys |> abgov_make_key_args("AreaOperatorKeys"),
    station_keys |> abgov_make_key_args("StationKeys")
  )
  request_body <- list(
    CollectionType = continuous |>
      ifelse("Continuous", "Non Continuous")
  ) |>
    c(key_args)

  # Get parameter names and keys
  params <- api_url |>
    abgov_post_request(endpoint = endpoint, request_body = request_body)

  # Filter to desired parameters
  if (!"all" %in% select_params) {
    is_selected <- names(params) %in% select_params | params %in% select_params
    params <- params[is_selected]
  }
  return(params)
}

abgov_get_qaqc_stations <- function(
  operator_keys = NULL,
  parameter = NULL,
  continuous = TRUE
) {
  api_url <- "https://datamanagementplatform.alberta.ca/Ambient/"
  endpoint <- "GetAOStationsListForAreaOperators"

  # Get keys for all operators if needed
  if (is.null(operator_keys)) {
    operator_keys <- abgov_get_qaqc_operators()
  }

  # Get stations for all/select parameters
  if (!is.null(parameter)) {
    parameters <- abgov_get_qaqc_parameters()
    parameter_key <- parameters[parameter]
    request_body <- list(
      ParameterKey = unname(parameter_key),
      AreaOperatorKeys = paste(operator_keys, collapse = ";"),
      CollectionType = continuous |>
        ifelse("Continuous", "Non Continuous")
    )
    stations <- api_url |>
      abgov_post_request(endpoint = endpoint, request_body = request_body) |>
      unlist()
    stations <- list(all = stations) # match expected output (operator = stations)
  } else {
    stations <- operator_keys |>
      lapply(\(key) {
        list(operators = key) |>
          abgov_get_keys(select_keys = "all", type = "stations")
      })
  }
  return(stations)
}

abgov_get_qaqc_keys <- function(
  stations,
  parameters,
  mode = "stations",
  continuous = TRUE
) {
  # Get keys for all operators
  operator_keys <- abgov_get_qaqc_operators()

  # Get keys for selected stations by operator
  station_keys <- abgov_get_qaqc_stations(
    operator_keys = operator_keys
  )

  # Drop keys for non-selected operators
  if (mode == "stations") {
    station_keys <- station_keys[sapply(station_keys, length) > 0]
    operator_keys <- operator_keys[
      names(operator_keys) %in% names(station_keys)
    ]
  }

  # Get keys for selected parameters if needed
  if (mode == "stations") {
    parameter_keys <- list(
      operators = operator_keys,
      stations = station_keys |> unname() |> unlist()
    ) |>
      abgov_get_keys(
        select_keys = parameters,
        type = "parameters",
        continuous = TRUE
      )
  } else {
    parameter_keys <- abgov_get_qaqc_parameters()
    parameter_keys <- parameter_keys[names(parameter_keys) %in% parameters]
  }

  # Combine keys for making requests
  operator_keys <- data.frame(
    operator_name = names(operator_keys),
    operator = operator_keys
  )
  parameter_keys <- data.frame(
    parameter_name = names(parameter_keys),
    parameter = parameter_keys
  )
  station_keys |>
    handyr::for_each(
      \(keys) {
        data.frame(
          station_name = names(keys),
          station = unname(keys)
        )
      },
      .bind = TRUE,
      .bind_id = "operator_name",
      .show_progress = FALSE
    ) |>
    dplyr::tibble() |>
    dplyr::left_join(operator_keys, by = "operator_name") |>
    dplyr::cross_join(parameter_keys)
}

abgov_get_qaqc_data_request <- function(
  request_token,
  max_tries = 25,
  quiet = FALSE
) {
  stopifnot(length(request_token) == 1, is.character(request_token))
  stopifnot(length(max_tries) == 1, is.numeric(max_tries))
  stopifnot(length(quiet) == 1, is.logical(quiet))

  # Wait for request to be ready
  request_token |>
    abgov_wait_for_request(max_tries = max_tries)

  # Download data
  "https://datamanagementplatform.alberta.ca/" |>
    paste0("Home/ProcessDownload", "?token=", request_token) |>
    httr::GET() |>
    httr::content(as = "text", encoding = "UTF-8") |>
    data.table::fread(
      fill = TRUE,
      encoding = "UTF-8",
      verbose = FALSE,
      showProgress = !quiet
    )
}

abgov_wait_for_request <- function(request_token, max_tries = 25) {
  stopifnot(length(request_token) == 1, is.character(request_token))
  stopifnot(length(max_tries) == 1, is.numeric(max_tries))

  api_url <- "https://datamanagementplatform.alberta.ca/"
  endpoint_status <- "Home/GetRequestStatus"

  # Initialize
  request_not_ready <- TRUE
  total_tries <- 0
  recent_tries <- 0

  # Check request status until ready or exceed maximum attempts
  while (request_not_ready) {
    # Check request status
    request_status <- api_url |>
      paste0(endpoint_status, "?token=", request_token) |>
      httr::GET() |>
      httr::content(as = "parsed")
    request_not_ready <- as.character(request_status) != "OK"
    if (request_not_ready) {
      # Handle too many attempts
      if (total_tries > max_tries) {
        stop(
          "Failed to download data within the specified number of attempts. ",
          "Try increasing `max_tries` or requesting less data."
        )
      }

      # Wait before trying again
      wait_time <- c(1, 1, 1, 5, 5, 10)[pmin(recent_tries + 1, 6)]
      recent_tries <- recent_tries %% 6
      Sys.sleep(wait_time)

      # Increment
      recent_tries <- recent_tries + 1
      total_tries <- total_tries + 1
    }
  }

  invisible(request_token)
}

abgov_parse_qaqc_data <- function(
  data_stream
) {
  # Remove metadata header
  header_row <- which(data_stream[, 1] == "Data Origin: ")
  if (length(header_row) == 0) {
    stop("No data found")
  }
  meta_rows <- data_stream |>
    utils::head(header_row - 1)
  data_rows <- data_stream |>
    utils::tail(-(header_row - 1))

  # Parse from groups of columns for each param to a single data frame
  param_starts <- which(data_rows[1, ] == "Data Origin: ")
  param_ends <- dplyr::lead(param_starts) - 1
  param_ends[length(param_ends)] <- ncol(data_rows)
  qaqc_data <- param_starts |>
    handyr::for_each(
      .as_list = TRUE,
      .enumerate = TRUE,
      .bind = TRUE,
      .show_progress = FALSE,
      \(start, i) {
        data_rows[, start:param_ends[i]] |>
          abgov_parse_qaqc_station_data()
      }
    ) |>
    dplyr::tibble()
  return(qaqc_data)
}

abgov_parse_qaqc_station_data <- function(station_data) {
  tzone <- "MST" # TODO: confirm this?
  # Split metadata and observations
  header_row_idx <- which(station_data[, 1] == "Interval Start")
  meta <- station_data[, 1:2] |>
    utils::head(header_row_idx - 1) |>
    stats::setNames(c("name", "value")) |>
    dplyr::mutate(
      name = gsub(": ", "", .data$name),
      value = .data$value |> handyr::swap("", with = NA)
    ) |>
    tidyr::pivot_wider()
  obs <- station_data |>
    utils::tail(-(header_row_idx - 1))

  # Parse observations
  header <- obs[1, ] |> as.vector() |> unlist()
  obs[-1, ] |>
    stats::setNames(header) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("Interval"),
        \(x) lubridate::ymd_hms(x, tz = tzone)
      ),
      `Measurement Value` = as.numeric(.data$`Measurement Value`),
      Flags = .data$Flags |> handyr::swap("", with = NA),
      StationName = meta$`Station Name`,
      StationId = meta$`Station Id`,
      Parameter = meta$Parameter,
      Unit = meta$Unit
    )
}
