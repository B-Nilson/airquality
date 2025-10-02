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
