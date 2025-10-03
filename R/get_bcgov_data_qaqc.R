bcgov_get_qaqc_data <- function(
  stations = "all",
  variables = "all",
  years,
  use_rounded_value = TRUE, # TODO: is FALSE better?
  quiet = FALSE
) {
  stopifnot(is.character(stations), length(stations) > 0)
  stopifnot(is.character(variables), length(variables) > 0)
  stopifnot(is.numeric(years), length(years) > 0)
  stopifnot(is.logical(use_rounded_value), length(use_rounded_value) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)

  # Download, format, and join data
  qaqc_data <- years |>
    bcgov_make_qaqc_paths(variables = variables) |>
    handyr::for_each(
      read_bcgov_qaqc_file,
      use_rounded_value = use_rounded_value,
      .join = TRUE,
      .show_progress = !quiet
      # .parallel = fast, # TODO: test if works
    ) |>
    # Drop any all NA columns
    dplyr::select_if(\(x) !all(is.na(x)))

  if (nrow(qaqc_data) == 0) {
    stop("No data available.")
  }

  if (!"all" %in% stations) {
    qaqc_data <- qaqc_data |>
      dplyr::filter(.data$EMS_ID %in% stations)
  }
  return(qaqc_data)
}

read_bcgov_qaqc_file <- function(
  path,
  use_rounded_value = TRUE,
  timeout = 3600
) {
  stopifnot(is.character(path), length(path) == 1)
  stopifnot(is.logical(use_rounded_value), length(use_rounded_value) == 1)
  stopifnot(is.numeric(timeout), length(timeout) == 1, !is.na(timeout))

  list(timeout = timeout) |>
    withr::with_options(
      path |>
        data.table::fread(
          showProgress = FALSE,
          colClasses = "character"
        )
    ) |>
    handyr::on_error(
      .return = NULL,
      .warn = paste0("Could not read file:", path)
    ) |>
    bcgov_format_qaqc_data(use_rounded_value = use_rounded_value) |>
    handyr::on_error(
      .return = NULL,
      .warn = paste0("Could not format file:", path)
    )
}

bcgov_format_qaqc_data <- function(qaqc_data, use_rounded_value = TRUE) {
  stopifnot(is.data.frame(qaqc_data), nrow(qaqc_data) > 0)
  stopifnot(is.logical(use_rounded_value), length(use_rounded_value) == 1)

  # Constants
  value_cols <- c("RAW_VALUE", "ROUNDED_VALUE")
  erroneous_cols <- c(
    "NAPS_ID",
    "STATION_NAME",
    "STATION_NAME_FULL",
    "OWNER",
    "REGION",
    "DATE",
    "TIME",
    "UNIT",
    value_cols[(!use_rounded_value) + 1]
  )

  # Get parameter name, default unit, and value column name
  parameter <- qaqc_data$PARAMETER[1]
  unit <- qaqc_data$UNIT[1] |> fix_units()
  default_unit <- default_units[
    names(.bcgov_columns$values)[.bcgov_columns$values %in% parameter]
  ]
  value_col <- value_cols[use_rounded_value + 1]

  # Reformat and return
  qaqc_data |>
    # Set units of value column and convert to default unit
    dplyr::mutate(
      dplyr::across(dplyr::all_of(value_col), \(x) {
        x |>
          as.numeric() |>
          handyr::convert_units(
            from = unit,
            to = default_unit,
            keep_units = TRUE
          )
      })
    ) |>
    # drop unnecessary rows/columns for memory-saving
    dplyr::filter(!is.na(.data[[value_col]])) |>
    dplyr::select(-dplyr::any_of(erroneous_cols)) |>
    # some sites in some files have duplicated rows (i.e. TEMP_MEAN for 0450307 in 1989)
    dplyr::distinct() |>
    # PARAMETER, INSTRUMENT, VALUE -> `PARAMETER`, `PARAMETER`_INSTRUMENT
    tidyr::pivot_wider(
      names_from = "PARAMETER",
      values_from = value_col
    ) |>
    dplyr::rename_with(
      .cols = dplyr::all_of("INSTRUMENT"),
      \(col_name) paste0(parameter, "_", col_name)
    ) |> 
    remove_na_placeholders(na_placeholders = c("UNSPECIFIED", ""))
}

# Returns the available parameters for a given qaqc year
bcgov_get_qaqc_year_params <- function(year) {
  stopifnot(is.numeric(year), length(year) == 1)

  qaqc_directory <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/" |>
    paste0("AnnualSummary/", year, "/")

  # Get paramater CSV file names located in QAQC directory
  param_files <- qaqc_directory |>
    readLines() |>
    stringr::str_subset("csv$") |>
    stringr::str_subset("station", negate = TRUE)

  # Extract parameters from file names
  param_files |>
    stringr::str_extract("(\\w*)\\.csv$", group = 1)
}

# Returns paths to qaqc data for given years and parameters
bcgov_make_qaqc_paths <- function(years, variables) {
  stopifnot(is.numeric(years), length(years) > 0, !any(duplicated(years)))
  stopifnot(is.character(variables), length(variables) > 0)

  qaqc_directory <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/" |>
    file.path("AnnualSummary")

  # Handle input variables
  variables <- variables |>
    standardize_input_vars(
      all_variables = names(.bcgov_columns$values) |>
        stringr::str_remove("_1hr")
    )

  # Build paths to each vailable parameter file for each year
  all_params <- .bcgov_columns$values[paste0(variables, "_1hr")]
  paths <- years |>
    sapply(
      \(year) {
        # Filter for valid parameters
        available_params <- bcgov_get_qaqc_year_params(year) |>
          handyr::on_error(.return = character(0)) # in case year not available
        params <- all_params[all_params %in% available_params]
        if (length(params) == 0) {
          warning("No valid parameters provided:", year)
        }
        # Build paths to each parameter file
        param_files <- paste0(params, ".csv")
        qaqc_directory |> file.path(year, param_files)
      }
    ) |>
    unlist()

  if (length(paths) == 0) {
    stop("No files available for provided years / variables.")
  }

  return(paths)
}
