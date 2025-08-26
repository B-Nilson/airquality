get_abgov_data_qaqc <- function(
  stations = "all",
  variables = "all",
  date_range = "now",
  max_tries = 100,
  raw = FALSE,
  fast = FALSE,
  quiet = FALSE
) {
  # Constants
  tzone <- "MST" # TODO: confirm this?
  allowed_date_range <- c("1970-01-01 00") # TODO: confirm this
  allowed_date_range[2] <- lubridate::now(tz = tzone) |>
    lubridate::floor_date("months") |>
    lubridate::with_tz("UTC") |>
    format("%Y-%m-%d %H")

  # Decide which api endpoint to use
  mode <- dplyr::case_when(
    any(stations == "all") ~ "parameters",
    any(parameters == "all") & length(stations) < 8 ~ "stations",
    length(stations) > length(parameters) ~ "parameters",
    TRUE ~ "stations"
  )

  # Handle date_range inputs
  date_range <- date_range |>
    handle_date_range(within = allowed_date_range, tz = tzone)

  # Handle input variables
  value_cols <- .abgov_columns$values[
    .abgov_columns$values != "Fine Particulate Matter" # raw API column
  ]
  all_variables <- names(value_cols) |>
    stringr::str_remove("_1hr")
  variables <- variables |>
    standardize_input_vars(all_variables)

  # Break up date range into smaller chunks depending on duration
  date_chunks <- date_range |> pretty(n = 5)
  max_duration <- date_chunks |>
    difftime(dplyr::lag(date_chunks), units = "days") |>
    median(na.rm = TRUE)
  if (max_duration > 3650) {
    max_duration <- "3650 days"
  } else if (max_duration < 30) {
    max_duration <- "30 days"
  }

  # Get API keys for our stations/parameters
  desired_var_cols <- value_cols[
    stringr::str_remove(names(value_cols), "_1hr") %in% variables
  ]
  keys <- stations |>
    abgov_get_qaqc_keys(parameters = desired_var_cols, mode = mode)

  # Initiate data request(s) and get token(s) for tracking progress
  request_tokens <- date_range |>
    handyr::split_date_range(max_duration = max_duration, as_list = TRUE) |>
    handyr::for_each(
      \(d_range) {
        d_range <- d_range |> sapply(format, format = "%F") |> unname()
        if (!quiet) {
          "Requesting data for:" |>
            handyr::log_step(d_range[1], "–", d_range[2])
        }
        mode_name <- ifelse(mode == "stations", "station", "parameter")
        unique(keys[[mode_name]]) |>
          sapply(\(key) {
            this <- keys[keys[[mode_name]] == key, ]
            out_name <- this[[paste0(mode_name, "_name")]][1]
            station_keys <- if (mode == "stations") key else this$station
            parameter_keys <- if (mode == "stations") this$parameter else key
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
  # Download data as it becomes available
  obs <- request_tokens |>
    handyr::for_each(
      .bind = TRUE,
      .enumerate = TRUE,
      \(tokens, i) {
        if (!quiet) {
          "Downloading data for request:" |>
            handyr::log_step(i, "/", length(request_tokens))
        }
        tokens |>
          handyr::for_each(
            .bind = TRUE,
            .enumerate = TRUE,
            \(token, j) {
              if (is.null(token)) {
                return(NULL)
              }
              token |>
                abgov_get_qaqc_data_request(
                  max_tries = max_tries,
                  quiet = quiet
                ) |>
                abgov_parse_qaqc_data(mode = mode) |>
                handyr::on_error(.return = NULL)
            }
          )
      }
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
  qaqc_data |>
    # Remove duplicates and any missing or flagged data
    dplyr::filter(!is.na(`Measurement Value`), is.na(Flags)) |>
    dplyr::distinct(
      StationName,
      `Interval End`,
      Parameter,
      Unit,
      .keep_all = TRUE
    ) |>
    # Fix units, set obs date, and mark quality assured
    dplyr::mutate(
      Unit = standardize_units(Unit),
      ReadingDate = (`Interval Start` + lubridate::hours(1)) |>
        lubridate::with_tz("UTC"),
      quality_assured = is.na(Flags)
    ) |>
    dplyr::filter(
      ReadingDate |> dplyr::between(date_range[1], date_range[2])
    ) |>
    # Cleanup
    widen_with_units(
      unit_col = "Unit",
      value_col = "Measurement Value",
      name_col = "Parameter",
      desired_cols = desired_cols
    ) |>
    drop_missing_obs_rows() |>
    standardize_obs_units(default_units = default_units)
}

widen_with_units <- function(obs, unit_col, value_col, name_col, desired_cols) {
  obs |> 
    dplyr::group_by(.unit = get(unit_col)) |>
    dplyr::select(-dplyr::all_of(unit_col)) |>
    dplyr::group_split() |>
    handyr::for_each(
      \(dat) {
        dat |>
          dplyr::mutate(
            dplyr::across(
              dplyr::any_of(value_col),
              \(val) val |> 
                as.numeric() |>
                units::set_units(.unit[1], mode = "standard")
            )
          ) |>
          dplyr::select(-.unit) |>
          tidyr::pivot_wider(
            names_from = name_col,
            values_from = value_col
          ) |>
          dplyr::select(dplyr::any_of(desired_cols)) |> 
          dplyr::distinct()
      }
    ) |>
    handyr::join_list() |>
    dplyr::select(dplyr::any_of(desired_cols))
}

drop_missing_obs_rows <- function(obs, where = is.numeric) {
  obs |>
    dplyr::filter(
      rowSums(!is.na(dplyr::across(dplyr::where(where)))) > 0
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
              yes = units(x)|> as.character(), 
              no = input_units[names(input_units) == dplyr::cur_column()]
            )
          x |>
            convert_units(
              in_unit = in_unit,
              out_unit = default_units[
                names(default_units) == dplyr::cur_column()
              ],
              keep_units = TRUE
            )
        }
      )
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
  api_url <- "https://datamanagementplatform.alberta.ca/Ambient/"
  endpoints <- list(
    stations = "AreaOperatorAmbientMultipleParameters",
    parameters = "AreaOperatorAmbientMultipleStations"
  )
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

  session_token <- api_url |>
    simulate_session(endpoint = endpoints[[mode]]) |>
    get_session_token()
  # Build request
  key_args <- c(
    operator_keys |> abgov_make_key_args("AreaOperatorKeys"),
    station_keys |> abgov_make_key_args("StationKeys"),
    parameter_keys |> abgov_make_key_args("ParameterKeys")
  )
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
    c(key_args)
  result <- api_url |>
    paste0(endpoints[[mode]]) |>
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
    operator <- api_url |>
      simulate_session(endpoint = endpoint_stations) |>
      extract_options(html_id = "AreaOperatorKeys")
  } else {
    request_body <- list(
      ParameterKey = select_parameter, # TODO: convert from variable -> key,
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
  parameters <- api_url |>
    simulate_session(endpoint = endpoint) |>
    extract_options(html_id = "ParameterKey")

  names(parameters) <- stringr::str_split(
    names(parameters),
    " / ",
    simplify = TRUE
  )[, 1]
  return(parameters)
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
    setNames(option_names)
  is_place_holder <- option_names == "--- SELECT ---"
  options[!is_place_holder]
}

abgov_make_key_args <- function(keys, key_name) {
  as.list(keys) |> setNames(rep(key_name, length(keys)))
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

# TODO: use in other sources?
abgov_post_request <- function(api_url, endpoint, request_body) {
  api_url |>
    paste0(endpoint) |>
    httr::POST(body = request_body, encode = "form") |>
    httr::content(as = "parsed", encoding = "UTF-8") |>
    dplyr::bind_rows() |>
    dplyr::select(name = text, value) |>
    dplyr::mutate(value = as.numeric(value)) |>
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

abgov_get_qaqc_stations <- function(operator_keys = NULL, parameter = NULL) {
  api_url <- "https://datamanagementplatform.alberta.ca/Ambient/"
  endpoint <- "GetAOStationsListForAreaOperators"

  # Get keys for all operators if needed
  if (is.null(parameter) & is.null(operator_keys)) {
    operator_keys <- abgov_get_qaqc_operators()
  } else if (is.null(operator_keys)) {
    all_parameters <- abgov_get_qaqc_parameters()
    parameter_key <- all_parameters[names(all_parameters) == parameter]
    operator_keys <- abgov_get_qaqc_operators(
      select_parameter = parameter_key,
      continuous = continuous
    )
  }
  # Get stations for all/select parameters
  if (!is.null(parameter)) {
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
  if (mode == "stations") {
    operator_keys <- abgov_get_qaqc_operators()
  } else if (mode == "parameters") {
    all_parameters <- abgov_get_qaqc_parameters()
    parameter_keys <- all_parameters[names(all_parameters) %in% parameters]
    operator_keys <- abgov_get_qaqc_operators(
      select_parameter = parameter_keys,
      continuous = continuous
    )
  } else {
    stop("mode must be one of 'stations' or 'parameters'")
  }

  # Get keys for selected stations by operator
  station_keys <- abgov_get_qaqc_stations(
    operator_keys = operator_keys,
    parameter = if (mode == "parameters") parameters[1] else NULL
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
      .bind_id = "operator_name"
    ) |>
    dplyr::tibble() |>
    dplyr::left_join(operator_keys, by = "operator_name") |>
    dplyr::cross_join(parameter_keys)
}

abgov_get_qaqc_data_request <- function(
  data_request_token,
  max_tries = 25,
  quiet = FALSE
) {
  api_url <- "https://datamanagementplatform.alberta.ca/"
  endpoint_status <- "Home/GetRequestStatus"
  endpoint_download <- "Home/ProcessDownload"

  # Initialize
  request_not_ready <- TRUE
  total_tries <- 0
  recent_tries <- 0
  while (request_not_ready) {
    # Check request status
    request_status <- api_url |>
      paste0(endpoint_status, "?token=", data_request_token) |>
      httr::GET() |>
      httr::content(as = "parsed")
    request_not_ready <- as.character(request_status) != "OK"
    if (request_not_ready) {
      # Handle too many attempts
      if (total_tries > max_tries) {
        stop(
          "Failed to download data within the specified number of attempts. Try increasing `max_tries` or requesting less data."
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

  # Download data
  api_url |>
    paste0(endpoint_download, "?token=", data_request_token) |>
    httr::GET() |>
    httr::content(as = "text", encoding = "UTF-8") |>
    data.table::fread(
      fill = TRUE,
      encoding = "UTF-8",
      verbose = FALSE,
      showProgress = !quiet
    )
}

abgov_parse_qaqc_data <- function(
  data_stream,
  mode = "stations"
) {
  # Remove metadata header
  header_texts <- list(
    stations = "Data Origin: ",
    parameters = "Data Origin: "
  )
  header_row <- which(data_stream[, 1] == header_texts[[mode]])
  if (length(header_row) == 0) {
    stop("No data found")
  }
  meta_rows <- data_stream |>
    head(header_row - 1)
  data_rows <- data_stream |>
    tail(-(header_row - 1))

  # Parse from groups of columns for each param to a single data frame
  param_starts <- which(data_rows[1, ] == header_texts[[mode]])
  param_ends <- dplyr::lead(param_starts) - 1
  param_ends[length(param_ends)] <- ncol(data_rows)
  qaqc_data <- param_starts |>
    handyr::for_each(
      .as_list = TRUE,
      .enumerate = TRUE,
      .bind = TRUE,
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
    head(header_row_idx - 1) |>
    setNames(c("name", "value")) |>
    dplyr::mutate(
      name = gsub(": ", "", .data$name),
      value = .data$value |> handyr::swap("", with = NA)
    ) |>
    tidyr::pivot_wider()
  obs <- station_data |>
    tail(-(header_row_idx - 1))

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
