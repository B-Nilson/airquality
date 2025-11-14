bcgov_get_qaqc_data <- function(
  stations = "all",
  variables = "all",
  years,
  mode = "binary",
  use_rounded_value = TRUE,
  quiet = FALSE
) {
  stopifnot(is.character(stations), length(stations) > 0)
  stopifnot(is.character(variables), length(variables) > 0)
  stopifnot(is.numeric(years), length(years) > 0)
  stopifnot(is.character(mode), mode %in% c("binary", "csv"), length(mode) == 1)
  stopifnot(is.logical(use_rounded_value), length(use_rounded_value) == 1)
  stopifnot(is.logical(quiet), length(quiet) == 1)

  # Download, format, and join data
  qaqc_data <- years |>
    bcgov_make_qaqc_paths(variables = variables, mode = mode) |>
    handyr::for_each(
      read_bcgov_qaqc_file,
      use_rounded_value = use_rounded_value,
      .join = TRUE,
      .show_progress = !quiet
      # .parallel = fast, # TODO: test if works
    )

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
  stopifnot(
    is.character(path),
    length(path) == 1,
    tools::file_ext(path) %in% c("csv", "parquet")
  )
  stopifnot(is.logical(use_rounded_value), length(use_rounded_value) == 1)
  stopifnot(is.numeric(timeout), length(timeout) == 1, !is.na(timeout))

  if (tools::file_ext(path) == "parquet") {
    obs <- path |> get_parquet_file()
  } else {
    obs <- list(timeout = timeout) |>
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
      )
  }

  obs |>
    bcgov_format_qaqc_data(use_rounded_value = use_rounded_value) |>
    handyr::on_error(
      .return = NULL,
      .warn = paste0("Could not format file:", path)
    )
}

get_parquet_file <- function(url) {
  rlang::check_installed("arrow")
  rlang::check_installed("tzdb")
  rlang::check_installed("RCurl")
  url |>
    RCurl::getBinaryURL() |>
    arrow::read_parquet() |>
    handyr::on_error(
      .return = NULL,
      .warn = paste0("Could not read file:", url)
    )
}

bcgov_format_qaqc_data <- function(qaqc_data, use_rounded_value = TRUE) {
  stopifnot(is.data.frame(qaqc_data), nrow(qaqc_data) > 0)
  stopifnot(is.logical(use_rounded_value), length(use_rounded_value) == 1)

  # Constants
  value_cols <- c("RAW_VALUE", "ROUNDED_VALUE")
  value_col <- value_cols[use_rounded_value + 1]
  desired_cols <- c(
    "date_utc",
    "EMS_ID",
    "PARAMETER",
    "VALUE" = value_col,
    "INSTRUMENT"
  )

  # Get parameter name, unit, and default unit
  parameter <- qaqc_data$PARAMETER[1]
  unit <- qaqc_data$UNIT[1] |> fix_units()
  default_unit <- default_units[
    names(.bcgov_columns$values)[.bcgov_columns$values %in% parameter]
  ]

  # Reformat and return
  qaqc_data |>
    remove_na_placeholders(na_placeholders = c("UNSPECIFIED", "")) |>
    dplyr::mutate(
      # Pad left side of id with 0s if needed
      EMS_ID = .data$EMS_ID |>
        stringr::str_pad(pad = "0", width = 7, side = "left"),
      # Convert date to UTC backward looking
      date_utc = (as.numeric(.data$DATE) *
        (60 * 60 * 24) +
        as.numeric(.data$TIME) + (8 * 3600)) |>
        lubridate::as_datetime()
    ) |>
    # drop unnecessary rows/columns for memory-saving
    dplyr::select(dplyr::all_of(desired_cols)) |>
    dplyr::filter(!is.na(.data$VALUE)) |>
    # some sites in some files have duplicated rows (i.e. TEMP_MEAN for 0450307 in 1989, PM25 for E246240 in 2008)
    dplyr::distinct(
      .data$date_utc,
      .data$EMS_ID,
      .data$PARAMETER,
      .data$INSTRUMENT,
      .keep_all = TRUE
    ) |>
    # Set units of value column and convert to default unit
    dplyr::mutate(
      VALUE = as.numeric(.data$VALUE) |>
        handyr::convert_units(
          from = unit,
          to = default_unit,
          keep_units = TRUE
        )
    ) |>
    # PARAMETER, INSTRUMENT, VALUE -> `PARAMETER`, `PARAMETER`_INSTRUMENT
    tidyr::pivot_wider(names_from = "PARAMETER", values_from = "VALUE") |>
    dplyr::rename_with(
      .cols = dplyr::all_of("INSTRUMENT"),
      \(col_name) paste0(parameter, "_", col_name)
    )
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
bcgov_make_qaqc_paths <- function(years, variables, mode = "binary") {
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
        param_files <- params |>
          paste0(ifelse(mode == "binary", ".parquet", ".csv"))
        qaqc_directory |>
          file.path(year, ifelse(mode == "binary", "binary", ""), param_files)
      }
    ) |>
    unlist()

  if (length(paths) == 0) {
    stop("No files available for provided years / variables.")
  }

  return(paths)
}
