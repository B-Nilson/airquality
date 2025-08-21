bcgov_get_qaqc_data <- function(
  stations = "all",
  years,
  variables = "all",
  use_rounded_value = TRUE,
  quiet = FALSE
) {
  qaqc_directory <- bcgov_ftp_site |>
    paste0("/AnnualSummary/")

  # Handle input variables
  is_instrument_col <- bcgov_col_names |> endsWith("_INSTRUMENT")
  all_variables <- names(bcgov_col_names)[is_instrument_col] |>
    stringr::str_remove("_1hr_instrument")
  variables <- variables |>
    standardize_input_vars(all_variables)

  # Make paths to files to get
  qaqc_paths <- years |>
    sapply(\(year) {
      year |>
        bcgov_make_qaqc_paths(params = variables) |>
        handyr::on_error(.return = character(0))
    }) |>
    unlist() |>
    as.vector()
  if (length(qaqc_paths) == 0) {
    stop("No data available for provided date_range / parameters.")
  }

  # Download, format, and join data
  qaqc_data <- qaqc_paths |>
    handyr::for_each(
      .as_list = TRUE,
      .enumerate = TRUE,
      # .parallel = fast, # TODO: test if works
      \(path, i) {
        if (!quiet) {
          "Downloading file" |>
            handyr::log_step(i, "/", length(qaqc_paths))
        }
        withr::with_options(
          list(timeout = 3600),
          path |>
            data.table::fread(
              showProgress = !quiet,
              colClasses = "character"
            ) |>
            bcgov_format_qaqc_data(use_rounded_value = use_rounded_value) |>
            handyr::on_error(.return = NULL)
        )
      }
    ) |>
    join_list() # TODO: use .join when implemented in for_each

  if (is.null(qaqc_data)) {
    stop("No data available for provided stations / date_range / parameters.")
  }

  if (!"all" %in% stations) {
    qaqc_data <- qaqc_data |>
      dplyr::filter(.data$EMS_ID %in% stations)
  }
  return(qaqc_data)
}

bcgov_format_qaqc_data <- function(qaqc_data, use_rounded_value = TRUE) {
  if (nrow(qaqc_data) == 0) {
    return(qaqc_data)
  }

  value_cols <- c("RAW_VALUE", "ROUNDED_VALUE")
  value_col <- value_cols[use_rounded_value + 1]
  erroneous_cols <- c(
    "NAPS_ID",
    "STATION_NAME",
    "STATION_NAME_FULL",
    "OWNER",
    "REGION",
    "DATE",
    "TIME",
    value_cols[(!use_rounded_value) + 1]
  )

  parameter <- qaqc_data$PARAMETER[1]
  default_unit <- default_units[
    names(bcgov_col_names[bcgov_col_names %in% parameter])
  ]
  qaqc_data |>
    # drop unnecessary rows/columns for memory-saving
    dplyr::filter(!is.na(.data[[value_col]])) |>
    dplyr::select(-dplyr::any_of(erroneous_cols)) |>
    # Set units of value column
    dplyr::mutate(
      UNIT = bcgov_fix_units(UNIT),
      dplyr::across(dplyr::all_of(value_col), \(x) {
        x |>
          units::set_units(.data$UNIT[1], mode = "standard") |>
          units::set_units(default_unit, mode = "standard")
      })
    ) |>
    dplyr::select(-UNIT) |>
    # PARAMETER, INSTRUMENT, VALUE -> `PARAMETER`, `PARAMETER`_INSTRUMENT
    tidyr::pivot_wider(
      names_from = "PARAMETER",
      values_from = value_col
    ) |>
    dplyr::rename_with(
      .cols = "INSTRUMENT",
      \(col_name) paste0(parameter, "_", col_name)
    )
}

bcgov_get_qaqc_years <- function() {
  qaqc_directory <- bcgov_ftp_site |>
    paste0("/AnnualSummary/")
  # Get directories in QAQC directory
  qaqc_dirs <- qaqc_directory |>
    readLines() |>
    stringr::str_subset("<DIR>")
  # Extract years from dir names
  qaqc_dirs |>
    stringr::str_extract("\\d{4}$") |>
    as.numeric()
}

bcgov_get_qaqc_year_params <- function(year) {
  qaqc_directory <- bcgov_ftp_site |>
    paste0("/AnnualSummary/", year, "/")
  # Get directories in QAQC directory
  param_files <- qaqc_directory |>
    readLines() |>
    stringr::str_subset("csv$") |>
    stringr::str_subset("station", negate = TRUE)
  # Extract parameters from file names
  param_files |>
    stringr::str_extract("(\\w*)\\.csv$", group = 1)
}

bcgov_make_qaqc_paths <- function(year, params) {
  qaqc_directory <- bcgov_ftp_site |>
    paste0("/AnnualSummary/")
  available_params <- bcgov_get_qaqc_year_params(year)
  params <- params[params %in% available_params]
  if (length(params) == 0) {
    stop("No valid parameters provided for this year.")
  }
  qaqc_directory |>
    paste0(year) |>
    paste0("/", params, ".csv")
}
