
# make backward looking rolling means (NAs not ignored)
# TODO: ignore NAs, make forward looking equivelant
get_lag_n_mean <- function(x, n = 3) {
  out <- x
  if (n <= 1) stop("`n` must be greater than one")
  for (i in 1:(n - 1)) {
    out <- out + dplyr::lag(x, i)
  }
  return(out / n)
}

# General function for loading in data quickly and quietly
# TODO: document
read_data <- function(..., showProgress = FALSE, verbose = FALSE, data.table = FALSE) {
  if (verbose) {
    data.table::fread(...,
      showProgress = showProgress,
      verbose = verbose, data.table = data.table
    )
  } else {
    invisible(data.table::fread(...,
      showProgress = showProgress,
      verbose = verbose, data.table = data.table
    ))
  }
}

lapply_and_name <- function(x, ...) {
  lapply(x, ...) |> stats::setNames(x)
}

# Truncate to desired digits
trunc_n <- function(x, n = 0) {
  trunc(x * 10^n) / 10^n
}

is_leap_year <- function(year) {
  year %% 4 == 0
}

# remove NA by default
mean_no_na <- function(x, ...) mean(x, na.rm = T, ...)
min_no_na <- function(x, ...) suppressWarnings(min(x, na.rm = T, ...)) |> handyr::swap(Inf, with = NA)
max_no_na <- function(x, ...) suppressWarnings(max(x, na.rm = T, ...)) |> handyr::swap(-Inf, with = NA)

standardize_colnames <- function(df, col_names, raw = FALSE) {
  if (raw) {
    return(df)
  }
  df |> dplyr::select(dplyr::any_of(col_names))
}

handle_date_range <- function(date_range, min_date_allowed = NA, max_date_allowed = NA) {
  # Handle date_range inputs with length != 2
  if (length(date_range) == 1) date_range <- c(date_range, date_range)
  if (length(date_range) != 2) {
    stop("`date_range` must have a length of either 1 or 2.")
  }
  # Handle character inputs
  if (is.character(date_range)) {
    date_range <- suppressWarnings(date_range |> lubridate::ymd_h(tz = "UTC"))
    if (any(is.na(date_range))) {
      stop("Ensure `date_range` is either a datetime or a character (UTC only) with this format: YYYY-MM-DD HH")
    }
  }
  # Handle dates before min date allowed
  if (!is.na(min_date_allowed)) {
    if (any(date_range < min_date_allowed)) {
      if (all(date_range < min_date_allowed)) {
        stop(paste(
          "At least one date_range value must be on or after",
          format(min_date_allowed, "%F %Z")
        ))
      }
      warning(paste0(
        "No data available for this source prior to",
        format(min_date_allowed, "%F %H:%M %Z"), ".\n",
        "Set the `date_range` to a period from this date onwards to stop this warning."
      ))
      date_range[date_range < min_date_allowed] <- min_date_allowed
    }
  }
  # Handle dates after max date allowed
  if (!is.na(max_date_allowed)) {
    if (any(date_range > max_date_allowed)) {
      if (all(date_range > max_date_allowed)) {
        stop(paste(
          "At least one date_range value must be on or before",
          format(max_date_allowed, "%F %Z")
        ))
      }
      warning(paste0(
        "No hourly data available from this source beyond the current hour (UTC).\n",
        "Set the `date_range` to a period from ", format(max_date_allowed, "%F %H:%M %Z"),
        " and earlier to stop this warning."
      ))
      date_range[date_range > max_date_allowed] <- max_date_allowed
    }
  }
  return(date_range)
}

# Handle if any/all requested stations for a specific data source don't exist in its meta data
# Returns stations with any unknown stations filtered out
check_stations_exist <- function(stations, known_stations, source) {
  unknown_stations <- stations[!stations %in% known_stations]
  if (length(unknown_stations) == length(stations)) {
    stop(paste(
      "All station IDs provided not found on", source, "for provided date_range:",
      paste0(unknown_stations, collapse = ", ")
    ))
  } else if (length(unknown_stations) > 0) {
    warning(paste(
      "Some station IDs provided not found on", source, "for provided date_range:",
      paste0(unknown_stations, collapse = ", ")
    ))
  }
  stations[stations %in% known_stations]
}

# TODO: generalize
convert_date_utc_to_local <- function(obs) {
  obs |> dplyr::mutate(
    tz_offset = .data$date_local |>
      stringr::str_extract("[+,-]\\d\\d*$") |>
      as.numeric() / 100,
    tz_hours = .data$tz_offset |> trunc(),
    tz_minutes = floor((.data$tz_offset - trunc(.data$tz_offset)) * 100),
    # Convert local date string to a datetime
    date_local = .data$date_local |>
      stringr::str_remove(" [+,-]\\d\\d*$") |>
      lubridate::ymd_hm(tz = "UTC"), # Set to UTC preemtively (still local time)
    # Convert from local to UTC by subtracting timezone offset
    date_utc_from_local = .data$date_local -
      lubridate::hours(.data$tz_hours) -
      lubridate::minutes(.data$tz_minutes)
  )
}

remove_na_placeholders <- function(obs, na_placeholders) {
  obs |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      \(x) x |> handyr::swap(na_placeholders, with = NA)
    ))
}
