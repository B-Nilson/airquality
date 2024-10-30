#' Easy error handler
#'
#' @param ... A code block (typically wrapped in `{}`) to run and capture errors (if any).
#' @param return (Optional) What is to be returned if an error occurs (instead of throwing an error).
#'   Default is `NULL`
#' @param msg (Optional) A single logical (TRUE/FALSE) value indicating if the error message should be displayed as a message instead.
#'   Default is `FALSE`
#' @param warn (Optional) A single logical (TRUE/FALSE) value indicating if the error message should be displayed as a warning instead.
#'   Default is `FALSE`
#' 
#' @description
#' `on_error` provides a simple way to handle errors by specifying a value to be returned on error and/or if a message/warning should be displayed instead.
#'  This is especially useful when attempting to load in multiple data files where it is possible for files not to exist. 
#'
#' @family Utilities
#'
#' @return the output of `...` unless an error occurs, then `return` instead. 
#' @export
#'
#' @examples
#' on_error(stop("test"), return = NULL, msg = TRUE)
#' on_error(read.csv("not_A_fil3.123"), return = NULL)
on_error = function(..., return = NULL, msg = FALSE, warn = FALSE){
  tryCatch(..., error = \(e){
    if(msg) message(as.character(e))
    if(warn) warning(as.character(e))
    return(return)
  })
}

#' Swap out values in a vector
#'
#' @param x Vector of values to be have certain values swapped out.
#' @param what One or more values to be replaced with `with` throughout `x`.
#' @param with A single value to replace `what` for throughout `x`.
#' 
#' @description
#' `swap` provides a simple way to switch out certain values in a vector. It is useful for replacing NA's, Infinites, and erroneous values. 
#'
#' @family Utilities
#'
#' @return a vector of `x` where all instances of `what` are replaced with `with`
#' @export
#'
#' @examples
#' swap(c(-20:20, NA), what = NA, with = -1)
#' swap(c(-20:20, Inf), what = Inf, with = NA)
#' swap(c(-20:20), what = Inf, with = NA)
swap = function(x, what, with) {
  if (any(is.na(what))) 
    x[is.na(x)] = with
  if (any(is.infinite(what))) 
    x[is.infinite(x)] = with
  x[x %in% what] = with
  return(x)
}
# Wrappers for swap()
swap_na  = function(x, with = -99) swap(x, NA , with)
swap_inf = function(x, with = NA)  swap(x, Inf, with)

#' Wrapper for looking up timezone of locations from lat/lng coords
#'
#' @param lng Vector of numeric values indicating location longitudes (decimal degrees) to lookup.
#' @param lat Vector of numeric values indicating location latitudes (decimal degrees) to lookup.
#' @param method A single character value indicating the lookup method to use (see `?lutz::tz_lookup_coords`)
#' @param ... (Optional) Additional paramaters to pass to `lutz::tz_lookup_coords`
#' 
#' @description
#' `get_timezone` is a wrapper for the `lutz::tz_lookup_coords` function. See `?lutz::tz_lookup_coords` for more details.
#'
#' @family Utilities
#'
#' @return a character vector with the same length as `lat` and `lng` indicating the locations likely timezone.
#' @export
#'
#' @examples
#' get_timezone(-105.053144, 69.116178)
get_timezone = function(lng, lat, method = "accurate", ...){
  lutz::tz_lookup_coords(lat, lng, method = method, ...)
}

# Calculates rolling mean if enough non-na provided
# TODO: code without zoo (use dplyr::lag/lead)
# TODO: document, test, and export
roll_mean = function(x, width, direction = "backward", fill = NA, min_n = 0, digits = 0){
  # Calculates the mean if enough values are provided
  mean_if_enough = function(x, min_n = 0, ...) {
    ifelse(sum(!is.na(x)) >= min_n, mean(x, na.rm = T, ...), NA)
  }
  align = ifelse(direction == "backward", "right",
                 ifelse(direction == "forward", "left", "center"))
  zoo::rollapply(
    x, width = width, align = align, fill = fill,
    FUN = mean_if_enough, min_n = min_n) |>
    round(digits)
}

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

# General function for loading in data quickly and quietly
# TODO: document
read_data = function(..., showProgress = FALSE, verbose = FALSE, data.table = FALSE){
  if(verbose){
    data.table::fread(..., showProgress = showProgress, 
      verbose = verbose, data.table = data.table)
  }else {
    invisible(data.table::fread(..., showProgress = showProgress,
      verbose = verbose, data.table = data.table))
  }
}

lapply_and_bind = function(...){
  lapply(...) |> dplyr::bind_rows()
}

lapply_and_name = function(x, ...){
  lapply(x, ...) |> stats::setNames(x)
}

# Truncate to desired digits
trunc_n = function(x, n = 0) {
  trunc(x * 10^n) / 10^n
}

is_leap_year = function(year) {
  year %% 4 == 0
}

# remove NA by default
mean_no_na = function(x, ...) mean(x, na.rm = T, ...)
min_no_na = function(x, ...) swap_inf(suppressWarnings(min(x, na.rm = T, ...)), NA)
max_no_na = function(x, ...) swap_inf(suppressWarnings(max(x, na.rm = T, ...)), NA)

standardize_colnames = function(df, col_names, raw = FALSE){
  if (raw) return(df)
  dplyr::select(df, dplyr::any_of(col_names))
}

handle_date_range = function(date_range, min_date_allowed = NA, max_date_allowed = NA){
  if(length(date_range) == 1) date_range = c(date_range, date_range)
  
  if(length(date_range) != 2)
    stop("`date_range` must have a length of either 1 or 2.")

  if(is.character(date_range)){
    date_range = suppressWarnings(lubridate::ymd_h(date_range, tz = "UTC"))
    if(any(is.na(date_range)))
      stop("Ensure `date_range` is either a datetime or a character (UTC only) with this format: YYYY-MM-DD HH")
  }

  if(!is.na(min_date_allowed))
    if(any(date_range < min_date_allowed)){
      if(all(date_range < min_date_allowed))
        stop(paste("At least one date_range value must be on or after",
                   format(min_date_allowed, "%F %Z")))
      warning(paste0(
        "No data available for this source prior to",
        format(min_date_allowed, "%F %H:%M %Z"),".\n",
        "Set the `date_range` to a period from this date onwards to stop this warning."))
      date_range[date_range < min_date_allowed] = min_date_allowed
    }

  if(!is.na(max_date_allowed))
    if(any(date_range > max_date_allowed)){
      if(all(date_range > max_date_allowed)) 
        stop(paste("At least one date_range value must be on or before",
                   format(max_date_allowed, "%F %Z")))
      warning(paste0(
        "No hourly data available from this source beyond the current hour (UTC).\n",
        "Set the `date_range` to a period from ", format(max_date_allowed, "%F %H:%M %Z"),
        " and earlier to stop this warning."))
      date_range[date_range > max_date_allowed] = max_date_allowed
    }
  return(date_range)
}

# Handle if any/all requested stations for a specific data source don't exist in its meta data
check_stations_exist = function(stations, known_stations, source){
  unknown_stations = stations[! stations %in% known_stations]
  if(length(unknown_stations) == length(stations)){
    stop(paste("All station IDs provided not found on", source, "for provided date_range:",
               paste0(unknown_stations, collapse = ", ")))
  }else if(length(unknown_stations) > 0){
    warning(paste("Some station IDs provided not found on", source, "for provided date_range:",
                  paste0(unknown_stations, collapse = ", ")))
  }
  return(invisible(TRUE))
}

# TODO: generalize
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

extract_tz_offset = function(date_str){
  offset = stringr::str_extract(date_str, "[+,-]\\d\\d*$")
  hours = stringr::str_sub(offset, end = 3) # TODO: Does this just split it and recombine it??
  minutes = stringr::str_sub(offset, start = 4)
  paste0(hours, minutes)
}