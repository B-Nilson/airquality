# make backward looking rolling means (NAs not ignored)
# TODO: ignore NAs, make forward looking equivelant
# TODO: move this to handyr
get_lag_n_mean <- function(x, n = 3) {
  out <- x
  if (n <= 1) {
    stop("`n` must be greater than one")
  }
  for (i in 1:(n - 1)) {
    out <- out + dplyr::lag(x, i)
  }
  return(out / n)
}

# General function for loading in data quickly and quietly
# TODO: document
# TODO: move this to handyr
read_data <- function(
  ...,
  showProgress = FALSE,
  verbose = FALSE,
  data.table = FALSE
) {
  if (verbose) {
    data.table::fread(
      ...,
      showProgress = showProgress,
      verbose = verbose,
      data.table = data.table
    )
  } else {
    invisible(data.table::fread(
      ...,
      showProgress = showProgress,
      verbose = verbose,
      data.table = data.table
    ))
  }
}

is_leap_year <- function(year) {
  year %% 4 == 0
}

# remove NA by default
mean_no_na <- function(x, ...) mean(x, na.rm = TRUE, ...)

handle_date_range <- function(date_range, within = c(NA, NA), tz = "UTC") {
  # Handle date_range inputs with length != 2
  if (length(date_range) == 1) {
    date_range <- c(date_range, date_range)
  }
  if (length(date_range) != 2) {
    stop("`date_range` must have a length of either 1 or 2.")
  }
  # Handle character inputs
  if (is.character(date_range)) {
    date_range <- date_range |>
      lubridate::ymd_h(tz = "UTC") |>
      handyr::silence(output = FALSE)
    if (any(is.na(date_range))) {
      stop(
        "Ensure `date_range` is either a datetime or a character (UTC only) with this format: YYYY-MM-DD HH"
      )
    }
  }
  # Handle NA's in within (no bound)
  if (is.na(within[1])) {
    within[1] <- "1970-01-01 00" |> lubridate::ymd_h(tz = "UTC")
  }
  if (is.na(within[2])) {
    within[2] <- lubridate::now(tz = "UTC")
  }
  if (is.numeric(within)) {
    within <- within |>
      lubridate::as_datetime(tz = tz)
  }
  
  # Convert within to min/max dates
  if (lubridate::is.POSIXct(within)) {
    within <- within |>
      lubridate::with_tz(tz) |>
      format("%F %H")
  }
  within <- within |>
    handyr::swap("now", with = Sys.time() |>
          lubridate::floor_date("hours") |>
          lubridate::with_tz(tz) |> 
          format("%F %H")) |> 
    lubridate::ymd_h(tz = tz)
  # Handle dates before min date allowed
  if (!is.null(within[1])) {
    if (any(date_range < within[1])) {
      if (all(date_range < within[1])) {
        stop(paste(
          "At least one date_range value must be on or after",
          format(within[1], "%F %Z")
        ))
      }
      warning(paste0(
        "No data available for this source prior to",
        format(within[1], "%F %H:%M %Z"),
        ".\n",
        "Set the `date_range` to a period from this date onwards to stop this warning."
      ))
      date_range[date_range < within[1]] <- within[1]
    }
  }
  # Handle dates after max date allowed
  if (!is.na(within[2])) {
    if (any(date_range > within[2])) {
      if (all(date_range > within[2])) {
        stop(paste(
          "At least one date_range value must be on or before",
          format(within[2], "%F %Z")
        ))
      }
      warning(paste0(
        "No hourly data available from this source beyond the current hour (UTC).\n",
        "Set the `date_range` to a period from ",
        format(within[2], "%F %H:%M %Z"),
        " and earlier to stop this warning."
      ))
      date_range[date_range > within[2]] <- within[2]
    }
  }
  date_range |> lubridate::with_tz(tz)
}

# Handle if any/all requested stations for a specific data source don't exist in its meta data
# Returns stations with any unknown stations filtered out
check_stations_exist <- function(stations, known_stations, source) {
  unknown_stations <- stations[!stations %in% known_stations]
  if (length(unknown_stations) == length(stations)) {
    stop(paste(
      "All station IDs provided not found on",
      source,
      "for provided date_range:",
      paste0(unknown_stations, collapse = ", ")
    ))
  } else if (length(unknown_stations) > 0) {
    warning(paste(
      "Some station IDs provided not found on",
      source,
      "for provided date_range:",
      paste0(unknown_stations, collapse = ", ")
    ))
  }
  stations[stations %in% known_stations]
}

# TODO: generalize
convert_date_utc_to_local <- function(obs) {
  obs |>
    dplyr::mutate(
      tz_offset = .data$date_local |>
        stringr::str_extract("[+,-]\\d\\d*$") |>
        as.numeric() /
        100,
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
