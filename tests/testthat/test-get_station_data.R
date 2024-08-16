
# Inputs: locations -------------------------------------------------------

# Inputs: date_range ------------------------------------------------------

test_that("invalid date_range causes error", {
  location = "Vanderhoof, BC, Canada"
  # Case: invalid input value
  expect_error(get_station_data(location, "bananas"))
  # Case: too many dates
  expect_error(get_station_data(location, c("1919-01-01 00", "1919-01-01 01", "1919-01-01 02")))
})

# Inputs: buffer_dist -----------------------------------------------------


# Inputs: networks --------------------------------------------------------


# Inputs: sources ---------------------------------------------------------

# Outputs: dates ---------------------------------------------------------

test_that("all dates non-na and within requested period", {
  location = "Vanderhoof, BC, Canada"
  date_range = lubridate::ymd_h(
    c("2019-01-01 00", "2019-01-01 01"), tz = "America/Toronto")
  obs = get_station_data(location, date_range)$data
  # Case: All date_utc non-NA
  expect_true(all(!is.na(obs$date_utc)))
  # Case: All date_local non-NA
  expect_true(all(!is.na(obs$date_local)))
  # Case: All date_utc within requested date range
  expect_true(all(obs$date_utc %>% dplyr::between(date_range[1], date_range[2])))
})

test_that("date_local converts to date_utc correctly", {
  location = "Vanderhoof, BC, Canada"
  date_range = lubridate::ymd_h(c("2019-02-01 00", "2019-02-02 00"), tz = "America/Vancouver")
  obs = get_station_data(location, date_range)$data
  obs = obs %>% dplyr::mutate(
    # Extract tz offset from end of local date string
    tz_offset = as.numeric(stringr::str_extract(.data$date_local, "[+,-]\\d{4}$")) %>%
      {trunc(. / 100) + (. - trunc(./100)*100)/60},
    # Convert local date string to a datetime
    date_local = stringr::str_remove(.data$date_local, " [+,-]\\d{4}$") %>%
      lubridate::ymd_hm(tz = "UTC"), # Set to UTC preemtively (still local time)
    # Convert from local to UTC by subtracting timezone offset
    date_utc_from_local = .data$date_local - lubridate::minutes(tz_offset*60))
  # Case: date_utc the same as converting date_local to UTC
  expect_equal(obs$date_utc, obs$date_utc_from_local)
})

# Outputs: data -----------------------------------------------------------

test_that("expected data returned", {
  date_range = lubridate::ymd_h(c("2019-02-01 00", "2019-02-02 00"))
  obs = get_station_data("Vanderhoof, BC, Canada", date_range)$data
  # Case: tibble is returned
  expect_true("tbl_df" %in% class(obs))
  # Case: data.frame has rows
  expect_true(nrow(obs) > 0)
  # Case: data.frame has cols
  expect_true(ncol(obs) > 0)
})
