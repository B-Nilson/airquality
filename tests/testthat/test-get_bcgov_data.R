test_that("an error/warning is not thrown in normal usaage", {
  date_range = "2019-02-01 00"
  obs = expect_no_warning(expect_no_error(
    get_bcgov_data("0450307", date_range, verbose = FALSE)))
})

# Input: stations ---------------------------------------------------------

test_that("returns requested stations only", {
  stations = c("0450307", "E206898")
  date_range = "2019-02-01 00"
  # Case: a single station
  obs = get_bcgov_data(stations[1], date_range, verbose = FALSE)
  expect_true(all(unique(obs$site_id) %in% stations[1]))
  # Case: 2+ stations
  obs = get_bcgov_data(stations, date_range, verbose = FALSE)
  expect_true(all(unique(obs$site_id) %in% stations))
})

test_that("unknown stations cause warning", {
  stations = c("bananas", "0450307")
  date_range = "2019-02-02 00"
  # Case: All stations invalid
  expect_error(get_bcgov_data(stations[1], date_range, verbose = FALSE))
  # Case: Some stations invalid
  expect_warning(get_bcgov_data(stations, date_range, verbose = FALSE))
})

# Inputs: date_range ------------------------------------------------------

test_that("invalid date_range causes error", {
  station = "0450307"
  # Case: invalid input value
  expect_error(get_bcgov_data(station, "bananas", verbose = FALSE))
  # Case: too many dates
  expect_error(get_bcgov_data(station, c("1919-01-01 00", "1919-01-01 01", "1919-01-01 02"), verbose = FALSE))
})

test_that("too early date_range causes warning/error", {
  station = "M110514"
  earliest_time = lubridate::ymd_h("1990-01-01 01", tz = bcmoe_tzone)
  # Case: All in the past
  expect_error(get_bcgov_data(station, earliest_time - hours(1), verbose = FALSE))
  # Case: Partly in the past
  date_range = c(earliest_time - lubridate::hours(1), earliest_time)
  expect_warning(get_bcgov_data(station, date_range, verbose = FALSE))
})

test_that("too late date_range causes warning/error", {
  station = "0450307"
  current_time = lubridate::floor_date(Sys.time(), "hours")
  future_time = current_time + lubridate::hours(24)
  # Case: All in the future
  expect_error(get_bcgov_data(station, future_time, verbose = FALSE))
  # Case: Partly in the future
  date_range = c(current_time - lubridate::hours(1), future_time)
  expect_warning(get_bcgov_data(station, date_range, verbose = FALSE))
})

# Outputs: dates ---------------------------------------------------------

test_that("all dates non-na and within requested period", {
  date_range = lubridate::ymd_h(
    c("2019-01-01 00", "2019-01-01 01"), tz = "America/Toronto")
  obs = get_bcgov_data("0450307", date_range, verbose = FALSE)
  # Case: All date_utc non-NA
  expect_true(all(!is.na(obs$date_utc)))
  # Case: All date_local non-NA
  expect_true(all(!is.na(obs$date_local)))
  # Case: All date_utc within requested date range
  expect_true(all(obs$date_utc |> dplyr::between(date_range[1], date_range[2])))
})

test_that("date_local converts to date_utc correctly", {
  date_range = lubridate::ymd_h(c("2019-02-01 00", "2019-02-02 00"), tz = "America/Vancouver")
  obs = get_bcgov_data("0450307", date_range, verbose = FALSE)
  obs = obs |> convert_date_utc_to_local()
  # Case: date_utc the same as converting date_local to UTC
  expect_equal(obs$date_utc, obs$date_utc_from_local)
})

# Outputs: data -----------------------------------------------------------

# TODO: handle sporadic warning here: Detected an unexpected many-to-many relationship between `x` and `y`
test_that("expected data returned", {
  station = "0450307"
  date_range = lubridate::ymd_h(c("2019-02-01 00", "2019-02-02 00"))

  obs = get_bcgov_data(station, date_range, verbose = FALSE)
  expect_snapshot(obs)
  
  obs_raw = get_bcgov_data(station, date_range, raw = TRUE, verbose = FALSE)
  expect_snapshot(obs_raw)
})
