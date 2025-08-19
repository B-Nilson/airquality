
bcgov_get_raw_data <- function(stations, variables = "all", quiet = FALSE) {
  collection_mode <- "stations" # default collection mode
  raw_directory_variables <- bcgov_ftp_site |>
    paste0("/Hourly_Raw_Air_Data/Year_to_Date/")
  raw_directory_stations <- raw_directory_variables |>
    paste0("STATION_DATA/")

  force_col_class <- c(
    DATE_PST = "character",
    EMS_ID = "character",
    STATION_NAME = "character"
  )

  all_stations <- bcgov_get_raw_stations()
  if (any(stations == "all")) {
    stations <- all_stations
    collection_mode <- "variables"
  }

  if(all(!stations %in% all_stations)) {
    stop("All provided stations not available for raw data")
  }
  if(any(!stations %in% all_stations)) {
    if (!quiet) warning(
      "Some stations not available for raw data: ", 
      paste0(stations[!stations %in% all_stations], collapse = ", ")
    )
  }
  stations <- stations[stations %in% all_stations]

  variables[variables == "pm2.5"] <- "pm25"
  
  if (any(variables == "all")) {
    variables_to_drop = character(0)
    collection_mode <- "stations"
  } else {
    variables_to_drop <- bcgov_col_names[
      !(names(bcgov_col_names) |>
        stringr::str_starts(variables |> paste0(collapse = "|"))) &
        !(names(bcgov_col_names) %in%
          c('date_utc', 'site_id', 'quality_assured'))
    ] |>
      unname()
  }

  if(length(variables) == 1 & length(stations) > 3) {
    collection_mode <- "variables"
  }

  # Determine files to download
  if (collection_mode == "variables") {
    variables <- bcgov_col_names[
      ! bcgov_col_names %in% variables_to_drop & 
      ! names(bcgov_col_names) %in% c('date_utc', 'site_id', 'quality_assured') &
      ! stringr::str_detect(bcgov_col_names, "_INSTRUMENT")
    ] |> 
      unname()
    data_paths <- raw_directory_variables |>
      paste0(variables, ".csv")
  }else {
    data_paths <- raw_directory_stations |>
      paste0(stations, ".csv")
  }

  # Download each stations file and bind together
  data_paths |>
    handyr::for_each(
      .as_list = TRUE, # TODO: remove once handyr updated (should be default when .bind = TRUE)
      .bind = TRUE,
      \(path) {
        withr::with_options(
          list(timeout = 3600),
          data.table::fread(
            file = path,
            colClasses = force_col_class,
            showProgress = !quiet
          ) |>
            bcgov_format_raw_data(mode = collection_mode) |>
            dplyr::select(-dplyr::any_of(variables_to_drop)) |>
            handyr::on_error(.return = NULL)
        )
      }
    )
}

bcgov_format_raw_data <- function(raw_data, mode = "stations") {
  if (nrow(raw_data) == 0) {
    return(raw_data)
  }
  if(mode == "variables") {
    return(
      bcgov_format_qaqc_data(raw_data, use_rounded_value = TRUE)
    )
  }
  if(mode == "stations") {
    meta_cols <- c("EMS_ID", "DATE_PST")
    instrument_cols <- stringr::str_subset(names(raw_data), "_INSTRUMENT$")
    unit_cols <- stringr::str_subset(names(raw_data), "_UNITS$")
    value_cols <- unit_cols |>
      stringr::str_remove("_UNITS$")
  }else {
    meta_cols <- c("EMS_ID", "DATE_PST")
    instrument_cols <- character(0)
    value_cols <- bcgov_col_names[bcgov_col_names %in% names(raw_data)] |> 
      unname()
    value_cols <- value_cols[!value_cols %in% meta_cols]
    unit_cols <- paste0(value_cols, "_UNITS")
    # Insert default units as unit columns as no units provided
    for(i in 1:length(unit_cols)) { # TODO: confirm units are correct here
      raw_data[[unit_cols[i]]] <- default_units[
        names(bcgov_col_names[bcgov_col_names %in% value_cols[i]])
      ]
    }
  }
  
  # Assign units to value columns
  for (i in 1:length(unit_cols)) {
    value_col <- value_cols[i]
    unit_col <- unit_cols[i]
    default_unit <- default_units[
      names(bcgov_col_names[bcgov_col_names %in% c(value_col, names(value_col))])
    ]
    raw_data <- raw_data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(value_col),
          \(x) {
            x |>
              units::set_units(
                bcgov_fix_units(.data[[unit_col]][1]),
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
    dplyr::select(dplyr::all_of(c(meta_cols, value_cols, instrument_cols)))
}
