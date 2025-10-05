bcgov_get_raw_data <- function(
  stations = "all",
  variables = "all",
  mode = "binary",
  quiet = FALSE
) {
  stopifnot(is.character(stations), length(stations) > 0)
  stopifnot(is.character(variables), length(variables) > 0)
  stopifnot(
    is.character(mode),
    mode %in% c("binary", "stations", "variables", "realtime"),
    length(mode) == 1
  )
  stopifnot(is.logical(quiet), length(quiet) == 1)

  # Get raw data file paths
  data_paths <- stations |>
    bcgov_make_raw_paths(
      variables = variables,
      mode = mode,
      quiet = quiet
    )

  is_parquet <- tools::file_ext(data_paths[1]) == "parquet"
  if (mode %in% c("variables", "binary") | is_parquet) {
    # Download/format each variables file and join together
    data_paths |>
      handyr::for_each(
        read_bcgov_qaqc_file,
        use_rounded_value = TRUE,
        .join = TRUE,
        .as_list = TRUE,
        .show_progress = !quiet
      )
  } else {
    # Download each stations file and bind together, then format
    data_paths |>
      handyr::for_each(
        read_raw_bcgov_data,
        .bind = TRUE,
        .show_progress = !quiet
      ) |>
      format_bcgov_raw_data(
        variables = variables,
        mode = mode
      )
  }
}

bcgov_make_raw_paths <- function(
  stations = NULL,
  variables = NULL,
  mode = "binary",
  quiet = FALSE
) {
  bcgov_ftp_site <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/"
  raw_directories <- list(
    stations = bcgov_ftp_site |>
      paste0("/Hourly_Raw_Air_Data/Year_to_Date/STATION_DATA/"),
    variables = bcgov_ftp_site |>
      paste0("/Hourly_Raw_Air_Data/Year_to_Date/"),
    binary = bcgov_ftp_site |>
      paste0("/Hourly_Raw_Air_Data/Year_to_Date/binary/"),
    realtime = bcgov_ftp_site |>
      paste0("/Hourly_Raw_Air_Data/Station/")
  )

  # Standardize common var names
  is_all_variables <- any(variables == "all")
  variables <- variables |>
    standardize_input_vars(
      all_variables = names(.bcgov_columns$values) |>
        stringr::str_remove("_1hr")
    )

  # Handle "all" in stations
  all_stations <- bcgov_get_raw_stations(realtime = mode == "realtime")
  if (any(stations == "all")) {
    stations <- all_stations
    if (mode != "realtime") {
      mode <- "binary" # force binary mode if all stations desired
    }
  }

  # Handle invalid stations
  if (all(!stations %in% all_stations)) {
    stop("All provided stations not available for raw data")
  }
  if (any(!stations %in% all_stations)) {
    if (!quiet) {
      warning(
        "Some stations not available for raw data: ",
        paste0(stations[!stations %in% all_stations], collapse = ", ")
      )
    }
  }
  stations <- stations[stations %in% all_stations]

  # Determine which columns to drop if variables not "all"
  non_id_cols <- unname(.bcgov_columns[c("values", "instruments")]) |>
    unlist()
  if (is_all_variables) {
    cols_to_drop <- character(0)
  } else {
    is_col_in_variables <- names(non_id_cols) |>
      stringr::str_starts(variables |> paste0(collapse = "|"))
    cols_to_drop <- non_id_cols[!is_col_in_variables] |>
      unname()
  }

  # Force binary mode if many stations relative to variables
  if (
    ((length(variables) == 1 & length(stations) > 3) |
      (length(variables) == 2 & length(stations) > 10)) &
      mode != "realtime"
  ) {
    mode <- "binary"
  }

  # Determine files to download
  if (mode %in% c("variables", "binary")) {
    is_instrument_col <- non_id_cols %in% .bcgov_columns$instruments
    file_variables <- non_id_cols[
      !non_id_cols %in% cols_to_drop & !is_instrument_col
    ] |>
      unname()
    all_paths <- raw_directories[[mode]] |>
      get_file_list(full_path = TRUE)
    if (mode == "binary") {
      data_paths <- raw_directories$binary |>
        paste0(file_variables, ".parquet")
    } else {
      data_paths <- raw_directories$variables |>
        paste0(file_variables, ".csv")
    }
    # Ensure realtime files exist before attempting to read
    data_paths <- all_paths[all_paths %in% data_paths]
  } else {
    data_paths <- raw_directories[[mode]] |>
      paste0(stations, ".csv")
    # Ensure realtime files exist and are recent before attempting to read
    if (mode == "realtime") {
      all_paths <- raw_directories$realtime |>
        get_file_list(full_path = TRUE)
      all_ages <- raw_directories$realtime |>
        get_file_ages(units = "days")
      data_paths <- all_paths[all_paths %in% data_paths & all_ages < 30]
    }
  }

  return(data_paths)
}

bcgov_get_raw_stations <- function(realtime = FALSE) {
  # Make path to realtime/year_to_date directory
  bcgov_ftp_site <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR"
  mode_directory <- ifelse(realtime, "Station/", "Year_to_Date/STATION_DATA/")
  raw_directory <- bcgov_ftp_site |>
    file.path("Hourly_Raw_Air_Data", mode_directory)

  # Pull stations from directory listing
  raw_directory |>
    get_file_list(full_path = FALSE, pattern = "\\.csv") |>
    stringr::str_subset("AQHI|Copy", negate = TRUE) |>
    stringr::str_remove("\\.csv")
}

# TODO: move to handyr and implement
get_file_list <- function(url_directory, full_path = TRUE, pattern = NULL) {
  files <- url_directory |>
    paste0("/") |>
    readLines() |>
    stringr::str_extract("[\\w\\-]+\\..*$")
  if (full_path) {
    files <- url_directory |>
      paste0(files)
  }

  if (!is.null(pattern)) {
    files <- files |>
      stringr::str_subset(pattern)
  }
  return(files)
}

get_file_ages <- function(url_directory, since = Sys.time(), units = "auto") {
  date_modified <- url_directory |>
    readLines() |>
    stringr::str_extract("^\\d{2}-\\d{2}-\\d{2}  \\d{2}:\\d{2}[A,P]M") |>
    lubridate::mdy_hm()
  difftime(since, date_modified, units = units)
}


read_raw_bcgov_data <- function(bcgov_path) {
  stopifnot(
    is.character(bcgov_path),
    length(bcgov_path) == 1,
    tools::file_ext(bcgov_path) %in% c("csv", "parquet")
  )

  is_binary <- tools::file_ext(bcgov_path) == "parquet"

  # Fetch and load file
  if (is_binary) {
    obs <- bcgov_path |> get_parquet_file()
  } else {
    obs <- withr::with_options(
      list(timeout = 3600),
      data.table::fread(
        file = bcgov_path,
        colClasses = "character",
        showProgress = FALSE
      ) |>
        suppressWarnings() |>
        handyr::on_error(
          .return = NULL,
          .warn = paste0("Could not read file:", bcgov_path)
        )
    )
  }
  # Drop "IGNORE_THIS_ROW" entries
  obs <- obs |>
    dplyr::filter(!.data$DATE_PST %in% "IGNORE THIS ROW")

  # Fix bugged csv files where header not attached properly
  if (any(names(obs) %in% c("V1", "0.001", "0.0001"))) {
    header <- readLines(url(bcgov_path), n = 1)
    names(obs) <- strsplit(header, ",")[[1]]
  }

  # Attach ems id from file name if not in file already
  if (!"EMS_ID" %in% names(obs) & nrow(obs) > 0) {
    obs$EMS_ID <- basename(bcgov_path) |>
      gsub(pattern = "\\.csv", replacement = "")
  }

  return(obs)
}

format_bcgov_raw_data <- function(
  raw_data,
  variables = "all",
  mode = "binary"
) {
  stopifnot(is.data.frame(raw_data), nrow(raw_data) > 0)
  stopifnot(
    is.character(mode),
    mode %in% c("binary", "stations", "variables", "realtime"),
    length(mode) == 1
  )

  bcgov_tzone <- "Etc/GMT+8" # PST (confirmed: raw/qaqc data files have col "DATE_PST")

  # Standardize common var names
  is_all_variables <- any(variables == "all")
  variables <- variables |>
    standardize_input_vars(
      all_variables = names(.bcgov_columns$values) |>
        stringr::str_remove("_1hr")
    )
  value_cols <- .bcgov_columns$values[
    names(.bcgov_columns$values) %in% paste0(variables, "_1hr")
  ]

  # binary/variables modes have same format as QAQC data - so use that function
  if (mode %in% c("variables", "binary")) {
    return(
      raw_data |> bcgov_format_qaqc_data(use_rounded_value = TRUE)
    )
  }

  # Drop unnecessary variables
  raw_data <- raw_data |>
    dplyr::select(
      dplyr::any_of(unname(.bcgov_columns$meta)),
      "DATE_PST",
      dplyr::any_of(unname(value_cols)),
      dplyr::any_of(unname(value_cols) |> paste0("_UNIT")),
      dplyr::any_of(unname(value_cols) |> paste0("_INSTRUMENT"))
    )

  # Get value/unit column names and insert unit columns if needed
  if (mode != "stations") {
    # Insert default units as unit columns as no units provided within file
    unit_cols <- paste0(value_cols, "_UNIT")
    for (i in 1:length(unit_cols)) {
      is_value_col <- .bcgov_columns$values == value_cols[i]
      if (any(is_value_col)) {
        raw_data[[unit_cols[i]]] <- default_units[
          names(.bcgov_columns$values)[is_value_col]
        ]
      }
    }
  } else {
    # "stations" mode has "_UNIT" columns already
    unit_cols <- names(raw_data)[names(raw_data) %in% .bcgov_columns$units]
  }

  # Assign units to value columns
  for (i in 1:length(unit_cols)) {
    # Get default unit for this value
    default_unit <- default_units[
      names(.bcgov_columns$values[.bcgov_columns$values == value_cols[i]])
    ]
    # Set units and standardize to default unit
    raw_data <- raw_data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(unname(value_cols[i])),
          \(x) {
            as.numeric(x) |>
              suppressWarnings() |> # NAs introduced by coercion
              handyr::convert_units(
                from = .data[[unit_cols[i]]][1],
                to = default_unit,
                keep_units = TRUE
              )
          }
        )
      )
  }

  # Standardize formatting
  raw_data |>
    dplyr::mutate(
      # Convert dates
      date_utc = .data$DATE_PST |>
        lubridate::ymd_hm(tz = bcgov_tzone) |>
        suppressWarnings(),
      ## Some files use HMS instead of HM for some reason..
      date_utc2 = .data$DATE_PST |>
        lubridate::ymd_hms(tz = bcgov_tzone) |>
        suppressWarnings(),
      date_utc = is.na(.data$date_utc) |>
        ifelse(yes = .data$date_utc2, no = .data$date_utc) |>
        lubridate::with_tz("UTC")
    ) |>
    remove_na_placeholders(na_placeholders = c("", "UNSPECIFIED")) |>
    drop_missing_obs_rows()
}
