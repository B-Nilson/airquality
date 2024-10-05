# Specify return value if code fails
# TODO: document
on_error = function(..., return = NULL, msg = FALSE, warn = FALSE){
  tryCatch(..., error = \(e){
    if(msg) message(as.character(e))
    return(return)
  })
}

# General function for loading in data quickly and quietly
read_data = function(..., showProgress = FALSE, verbose = FALSE, data.table = FALSE){
  suppressWarnings(data.table::fread(..., showProgress = showProgress,
                                     verbose = verbose, data.table = data.table))
}

# Calculates the mean if enough values are provided
mean_if_enough = function(x, min_n = 0, ...){
  ifelse(sum(!is.na(x)) >= min_n, mean(x, na.rm = T, ...), NA)
}

# Calculates rolling mean if enough non-na provided
# TODO: code without zoo (use dplyr::lag/lead)
# TODO: document, test, and export
roll_mean = function(x, width, direction = "backward", fill = NA, min_n = 0){
  align = ifelse(direction == "backward", "right",
                 ifelse(direction == "forward", "left", "center"))
  zoo::rollapply(
    x, width = width, align = align, fill = fill,
    FUN = mean_if_enough, min_n = min_n) |>
    round(1)
}


# TODO: do not use these - roll_mean is short enough now to use as is
# Calculate rolling 3 hour mean if at least 2 hours available
roll_mean_3hr_min_2 = function(x) {
  round(roll_mean(x, 3, min_n = 2), 1)
}

# TODO: do not use these - roll_mean is short enough now to use as is
# Calculate rolling 8 hour mean if at least 5 hours available
roll_mean_8hr_min_5 = function(x) {
  roll_mean(x, 8, min_n = 5)
}

# TODO: do not use these - roll_mean is short enough now to use as is
# Calculate rolling 24 hour mean if at least 15 hours available
roll_mean_24hr_min_15 = function(x) {
  roll_mean(x, 24, min_n = 15)
}

# Truncate to desired digits
trunc_n = function(x, n = 0){
  trunc(x*10^n)/10^n
}

# replace NA/inf with val
swap_na = function(x, val = -99) ifelse(is.na(x), val, x)
swap_inf = function(x, val = NA) ifelse(is.infinite(x), val, x)

# remove NA by default
mean_no_na = function(x, ...) mean(x, na.rm = T, ...)
min_no_na = function(x, ...) swap_inf(suppressWarnings(min(x, na.rm = T, ...)), NA)
max_no_na = function(x, ...) swap_inf(suppressWarnings(max(x, na.rm = T, ...)), NA)

# make backward looking rolling means (NAs not ignored)
# TODO: ignore NAs, make forward looking equivelant, incorp with roll_mean
get_lag_n_mean = function(x, n = 3){
  out = x
  if(n <= 1) stop("`n` must be greater than one")
  for(i in 1:(n-1)){
    out = out + dplyr::lag(x, i)
  }
  return(out/n)
}

standardize_colnames = function(df, all_col_names, raw = FALSE){
  if(raw){
    return(df)
  }else{
    col_names = all_col_names[all_col_names %in% names(df)]
    dplyr::rename_with(df, .cols = unname(col_names), 
        \(x) names(col_names)[col_names == x]) |>
      dplyr::select(dplyr::any_of(names(col_names)))
  }
}


handle_date_range = function(date_range, min_date_allowed = NA, max_date_allowed = NA){
  # If only a single value provided, repeat it
  if(length(date_range) == 1){
    date_range = c(date_range, date_range)
  }
  # If not 1/2 values provided, stop and say why
  if(length(date_range) != 2){
    stop("`date_range` must have a length of either 1 or 2.")
  }
  # If characters provided for date range, try to convert and stop if that fails
  if(is.character(date_range)){
    date_range = suppressWarnings(lubridate::ymd_h(date_range, tz = "UTC"))
    if(any(is.na(date_range)))
      stop("Ensure `date_range` is either a datetime or a character (UTC only) with this format: YYYY-MM-DD HH")
  }

  if(!is.na(min_date_allowed)){
    # If any dates less than the min allowed date
    if(any(date_range < min_date_allowed)){
      # End the function here and throw error if all requested data before min_date_allowed
      if(all(date_range < min_date_allowed))
        stop(paste("At least one date_range value must be on or after",
                   format(min_date_allowed, "%F"),"(PST)."))
      # Otherwise, warn the user
      warning(paste0(
        "No data available for this source prior to",
        format(min_date_allowed, "%F %H:%M %Z"),".\n",
        "Set the `date_range` to a period from this date onwards to stop this warning."))
      # And set the date that is before min date to the min date
      # (i.e. still try to get data from min_date_allowed onwards if the provided period straddles it)
      date_range[date_range < min_date_allowed] = min_date_allowed
    }
  }

  if(!is.na(max_date_allowed)){
    # hourly data only available for the current hour and prior - warn user if date_range in the future
    if(any(date_range > max_date_allowed)){
      # End the function here and throw error if all requested data after max date
      if(all(date_range > max_date_allowed)) stop("At least one date_range value must not be in the future.")
      warning(paste0(
        "No hourly data available from this source beyond the current hour (UTC).\n",
        "Set the `date_range` to a period from ", format(max_date_allowed, "%F %H:%M %Z"),
        " and earlier to stop this warning."))
      # And set the date that is after max_date_allowed to the max_date_allowed
      # (i.e. still try to get data from min_date_allowed onwards if the provided period straddles it)
      date_range[date_range > max_date_allowed] = max_date_allowed
    }
  }
  return(date_range)
}

# Handle if any/all requested stations for a specific data source don't exist in its meta data
check_stations_exist = function(stations, known_stations, source){
  # Determine if any desired stations not known
  unknown_stations = stations[! stations %in% known_stations]
  if(length(unknown_stations) == length(stations)){
    # Error if all stations unknown
    stop(paste("All station IDs provided not found on", source, "for provided date_range:",
               paste0(unknown_stations, collapse = ", ")))
  }else if(length(unknown_stations) > 0){
    # Warn if some stations unknown
    warning(paste("Some station IDs provided not found on", source, "for provided date_range:",
                  paste0(unknown_stations, collapse = ", ")))
  }
  return(invisible(NULL))
}

# Wrapper for looking up timezone of locations from lat/lng coords
get_station_timezone = function(lng, lat, method = "accurate"){
  lutz::tz_lookup_coords(lat, lng, method = method)
}

extract_tz_offset = function(date_str){
  offset = stringr::str_extract(date_str, "[+,-]\\d\\d*$")

  hours = stringr::str_sub(offset, end = 3)
  minutes = stringr::str_sub(offset, start = 4)
  paste0(hours, minutes)
}

lapply_and_bind = function(...){
  lapply(...) |> dplyr::bind_rows()
}

convert_date_utc_to_local = function(obs) {
  obs |> dplyr::mutate(
    tz_offset = as.numeric(extract_tz_offset(.data$date_local)) / 100,
    tz_hours = trunc(.data$tz_offset),
    tz_minutes = floor((.data$tz_offset - trunc(.data$tz_offset)) * 100),
    # Convert local date string to a datetime
    date_local = stringr::str_remove(.data$date_local, " [+,-]\\d\\d*$") |>
      lubridate::ymd_hm(tz = "UTC"), # Set to UTC preemtively (still local time)
    # Convert from local to UTC by subtracting timezone offset
    date_utc_from_local = .data$date_local - lubridate::hours(.data$tz_hours) -
      (lubridate::minutes(.data$tz_minutes)))
}
