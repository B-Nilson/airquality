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
  quiet = FALSE,
  data.table = FALSE
) {
  if (!quiet) {
    data.table::fread(
      ...,
      showProgress = !quiet,
      data.table = data.table
    )
  } else {
    invisible(data.table::fread(
      ...,
      showProgress = !quiet,
      data.table = data.table
    ))
  }
}

is_leap_year <- function(year) {
  year %% 4 == 0
}

handle_date_range <- function(
  date_range,
  within = c(NA, NA),
  tz = "UTC",
  time_step = "1 hours"
) {
  stopifnot(length(date_range) %in% 1:2)
  stopifnot(
    length(within) %in% 1:2,
    all(is.na(within)) |
      "Date" %in% class(within) |
      "POSIXct" %in% class(within) |
      is.character(within)
  )
  stopifnot(!is.na(lubridate::as.duration(time_step)))

  # Pass within to this function to allow for flexible date range there
  if (!all(is.na(within))) {
    original <- within
    within <- handle_date_range(within, tz = tz)
    within[is.na(original)] <- NA
    if (length(original) == 2) {
      if (is.na(original[2])) within[2] <- NA
    }
  }

  # Handle single value date_range
  if (length(date_range) == 1) {
    date_range <- c(date_range, date_range)
  }

  # Handle character inputs
  if (is.character(date_range)) {
    # Handle "now"
    if ("now" %in% date_range) {
      date_range <- date_range |>
        handyr::swap(
          "now",
          with = lubridate::now(tz = tz) |>
            lubridate::floor_date(time_step) |>
            lubridate::with_tz("UTC") |>
            format("%F %H")
        )
    }

    # Convert to POSIXct
    date_range <- date_range |>
      lubridate::ymd_h(tz = "UTC") |>
      handyr::silence(output = FALSE)
    if (all(is.na(date_range))) {
      stop(
        "If `date_range` is a character it must be either 'now' ",
        "or date/time strings (UTC only) with this format: YYYY-MM-DD HH"
      )
    }
  }

  # Handle NA's in within (no bound)
  if (is.na(within[1])) {
    within[1] <- lubridate::as_datetime(0)
  }
  if (is.na(within[2])) {
    within[2] <- lubridate::now(tz = tz) |>
      lubridate::floor_date(time_step)
  }

  # Handle dates before min date allowed
  if (any(date_range < within[1])) {
    if (all(date_range < within[1])) {
      stop(
        "At least one date_range value must be on or after ",
        format(within[1], "%F %Z")
      )
    }
    warning(
      "No data available for this source prior to ",
      format(within[1], "%F %H:%M %Z"),
      ".\n ",
      "Set the `date_range` to a period from this date onwards to stop this warning."
    )
    date_range[date_range < within[1]] <- within[1]
  }

  # Handle dates after max date allowed
  if (!is.na(within[2])) {
    if (any(date_range > within[2])) {
      if (all(date_range > within[2])) {
        stop(
          "At least one date_range value must be on or before ",
          format(within[2], "%F %Z")
        )
      }
      warning(
        "No hourly data available from this source beyond ",
        format(within[2], "%F %H:%M %Z"),
        "Set the max `date_range` to a period from ",
        "this date or earlier to stop this warning."
      )
      date_range[date_range > within[2]] <- within[2]
    }
  }

  # Ensure right time zones
  date_range |>
    lubridate::with_tz(tz)
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
