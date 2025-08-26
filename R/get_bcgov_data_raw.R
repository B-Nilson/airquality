bcgov_get_raw_data <- function(
  stations,
  variables = "all",
  mode = "stations",
  quiet = FALSE
) {
  stopifnot(mode %in% c("stations", "variables", "realtime"))

  non_id_cols <- unname(.bcgov_columns[c("values", "instruments")]) |>
    unlist()

  raw_directories <- list(
    stations = bcgov_ftp_site |>
      paste0("/Hourly_Raw_Air_Data/Year_to_Date/STATION_DATA/"),
    variables = bcgov_ftp_site |>
      paste0("/Hourly_Raw_Air_Data/Year_to_Date/"),
    realtime = bcgov_ftp_site |>
      paste0("/Hourly_Raw_Air_Data/Station/")
  )

  # Standardize common var names
  all_variables <- names(.bcgov_columns$values) |>
    stringr::str_remove("_1hr")
  variables <- variables |>
    standardize_input_vars(all_variables)

  # Handle "all" in stations
  all_stations <- bcgov_get_raw_stations(realtime = mode == "realtime")
  if (any(stations == "all")) {
    stations <- all_stations
    if (mode != "realtime") {
      mode <- "variables" # force variable mode if all stations desired
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
  if (any(variables == "all")) {
    cols_to_drop <- character(0)
  } else {
    is_col_in_variables <- names(non_id_cols) |>
      stringr::str_starts(variables |> paste0(collapse = "|"))
    cols_to_drop <- non_id_cols[!is_col_in_variables] |>
      unname()
  }

  # Force variable mode if only one variable and more than a few stations
  if (length(variables) == 1 & length(stations) > 3 & mode != "realtime") {
    mode <- "variables"
  } else if (
    length(variables) == 2 & length(stations) > 10 & mode != "realtime"
  ) {
    mode <- "variables"
  }

  # Determine files to download
  if (mode == "variables") {
    is_instrument_col <- non_id_cols %in% .bcgov_columns$instruments
    file_variables <- non_id_cols[
      !non_id_cols %in% cols_to_drop & !is_instrument_col
    ] |>
      unname()
    data_paths <- raw_directories$variables |>
      paste0(file_variables, ".csv")
  } else {
    data_paths <- raw_directories[[mode]] |>
      paste0(stations, ".csv")
  }

  # Download each stations file and bind together
  data_paths |>
    handyr::for_each(
      .bind = TRUE,
      \(path) {
        withr::with_options(
          list(timeout = 3600),
          data.table::fread(
            file = path,
            colClasses = "character",
            showProgress = !quiet
          ) |>
            handyr::on_error(.return = NULL)
        )
      }
    ) |>
    bcgov_format_raw_data(mode = mode) |>
    dplyr::select(-dplyr::any_of(cols_to_drop))
}

bcgov_format_raw_data <- function(raw_data, mode = "stations") {
  if (nrow(raw_data) == 0) {
    return(raw_data)
  }
  if (mode == "variables") {
    return(
      bcgov_format_qaqc_data(raw_data, use_rounded_value = TRUE)
    )
  }

  # Get column names and insert unit columns if needed
  meta_cols <- c("EMS_ID", "DATE_PST")
  value_cols <- names(raw_data)[
    names(raw_data) %in% .bcgov_columns$values
  ]
  unit_cols <- names(raw_data)[
    names(raw_data) %in% .bcgov_columns$units
  ]
  if (mode == "stations") {
    instrument_cols <- names(raw_data)[
      names(raw_data) %in% .bcgov_columns$instruments
    ]
  } else {
    instrument_cols <- character(0)
    # Insert default units as unit columns as no units provided
    # TODO: confirm units are correct here
    for (i in 1:length(unit_cols)) {
      is_value_col <- .bcgov_columns$values %in% value_cols[i]
      raw_data[[unit_cols[i]]] <- default_units[
        names(.bcgov_columns$values)[is_value_col]
      ]
    }
  }
  # Assign units to value columns
  for (i in 1:length(unit_cols)) {
    default_unit <- default_units[
      names(.bcgov_columns$values[
        .bcgov_columns$values %in% c(value_cols[i], names(value_cols[i])) # TODO: is this needed?
      ])
    ]
    raw_data <- raw_data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(value_cols[i]),
          \(x) {
            unit <- fix_units(.data[[unit_cols[i]]][1])
            as.numeric(x) |>
              suppressWarnings() |> # NAs introduced by coercion
              convert_units(in_unit = unit, out_unit = default_unit)
          }
        )
      )
  }
  # Drop unnecessary columns for memory-saving
  # TODO: see if they will do in source? files are quite bloated...
  raw_data |>
    dplyr::select(dplyr::all_of(c(meta_cols, value_cols, instrument_cols))) |>
    dplyr::mutate(quality_assured = FALSE)
}
