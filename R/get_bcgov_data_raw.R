bcgov_get_raw_data <- function(
  stations,
  variables = "all",
  mode = "stations",
  quiet = FALSE
) {
  stopifnot(mode %in% c("stations", "variables", "realtime"))

  raw_directories <- list(
    stations = bcgov_ftp_site |>
      paste0("/Hourly_Raw_Air_Data/Year_to_Date/STATION_DATA/"),
    variables = bcgov_ftp_site |>
      paste0("/Hourly_Raw_Air_Data/Year_to_Date/"),
    realtime = bcgov_ftp_site |>
      paste0("/Hourly_Raw_Air_Data/Station/")
  )

  # Standardize common var names
  variables <- standardize_input_vars(variables)

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
  is_id_col <- names(bcgov_col_names) %in%
    c('date_utc', 'site_id', 'quality_assured')
  if (any(variables == "all")) {
    cols_to_drop <- character(0)
  } else {
    is_col_in_variables <- names(bcgov_col_names) |>
      stringr::str_starts(variables |> paste0(collapse = "|"))
    cols_to_drop <- bcgov_col_names[
      !is_col_in_variables & !is_id_col
    ] |>
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
    is_instrument_col <- bcgov_col_names |> endsWith("_INSTRUMENT")
    file_variables <- bcgov_col_names[
      !bcgov_col_names %in% cols_to_drop & !is_id_col & !is_instrument_col
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
  if (mode == "stations") {
    instrument_cols <- names(raw_data) |> stringr::str_subset("_INSTRUMENT$")
    unit_cols <- names(raw_data) |> stringr::str_subset("_UNITS$")
    value_cols <- unit_cols |> stringr::str_remove("_UNITS$")
  } else {
    instrument_cols <- character(0)
    value_cols <- bcgov_col_names[bcgov_col_names %in% names(raw_data)] |>
      unname() |>
      stringr::str_subset(paste(meta_cols, collapse = "|"), negate = TRUE)
    unit_cols <- value_cols |> paste0("_UNITS")
    # Insert default units as unit columns as no units provided
    for (i in 1:length(unit_cols)) {
      # TODO: confirm units are correct here
      is_value_col <- bcgov_col_names %in% value_cols[i]
      raw_data[[unit_cols[i]]] <- default_units[
        names(bcgov_col_names[is_value_col])
      ]
    }
  }
  # Assign units to value columns
  for (i in 1:length(unit_cols)) {
    default_unit <- default_units[
      names(bcgov_col_names[
        bcgov_col_names %in% c(value_cols[i], names(value_cols[i]))
      ])
    ]
    raw_data <- raw_data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(value_cols[i]),
          \(x) {
            x |>
              as.numeric() |>
              suppressWarnings() |> # NAs introduced by coercion
              units::set_units(
                bcgov_fix_units(.data[[unit_cols[i]]][1]),
                mode = "standard"
              ) |>
              units::set_units(default_unit, mode = "standard")
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

# TODO: implement for all sources
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
