test_that("an error/warning is not thrown in normal usage", {
  date_range <- format(Sys.time(), "%Y-%m-%d %H")
  obs <- expect_no_warning(expect_no_error(
    get_bcgov_data(
      stations = "0450307",
      date_range = date_range,
      quiet = TRUE
    )
  ))
})

# Input: stations ---------------------------------------------------------

test_that("returns requested stations only", {
  stations <- c("0450307", "E206898")
  date_range <- "2019-02-01 00"
  # Case: a single station
  obs <- get_bcgov_data(stations[1], date_range, quiet = TRUE)
  expect_true(all(unique(obs$site_id) %in% stations[1]))
  # Case: 2+ stations
  obs <- get_bcgov_data(stations, date_range, quiet = TRUE)
  expect_true(all(unique(obs$site_id) %in% stations))
})

test_that("unknown stations cause warning", {
  stations <- c("bananas", "0450307")
  date_range <- "2019-02-02 00"
  # Case: All stations invalid
  expect_error(get_bcgov_data(stations[1], date_range, quiet = TRUE))
  # Case: Some stations invalid
  expect_warning(get_bcgov_data(stations, date_range, quiet = TRUE))
})

# Inputs: date_range ------------------------------------------------------

test_that("invalid date_range causes error", {
  station <- "0450307"
  # Case: invalid input value
  expect_error(get_bcgov_data(station, "bananas", quiet = TRUE))
  # Case: too many dates
  expect_error(get_bcgov_data(
    station,
    c("1919-01-01 00", "1919-01-01 01", "1919-01-01 02"),
    quiet = TRUE
  ))
})

test_that("too early date_range causes warning/error", {
  station <- "M110514"
  earliest_time <- lubridate::ymd_h("1990-01-01 01", tz = bcgov_tzone)
  # Case: All in the past
  expect_error(get_bcgov_data(
    station,
    earliest_time - hours(1),
    quiet = TRUE
  ))
  # Case: Partly in the past
  date_range <- c(earliest_time - lubridate::hours(1), earliest_time)
  expect_warning(get_bcgov_data(station, date_range, quiet = TRUE))
})

test_that("too late date_range causes warning/error", {
  station <- "0450307"
  current_time <- lubridate::floor_date(Sys.time(), "hours")
  future_time <- current_time + lubridate::hours(24)
  # Case: All in the future
  expect_error(get_bcgov_data(station, future_time, quiet = TRUE))
  # Case: Partly in the future
  date_range <- c(current_time - lubridate::hours(1), future_time)
  expect_warning(get_bcgov_data(station, date_range, quiet = TRUE))
})

# Outputs: dates ---------------------------------------------------------

test_that("all dates non-na and within requested period", {
  date_range <- lubridate::ymd_h(
    c("2019-01-01 00", "2019-01-01 01"),
    tz = "America/Toronto"
  )
  obs <- get_bcgov_data("0450307", date_range, quiet = TRUE)
  # Case: All date_utc non-NA
  expect_true(all(!is.na(obs$date_utc)))
  # Case: All date_local non-NA
  expect_true(all(!is.na(obs$date_local)))
  # Case: All date_utc within requested date range
  expect_true(all(obs$date_utc |> dplyr::between(date_range[1], date_range[2])))
})

test_that("date_local converts to date_utc correctly", {
  date_range <- lubridate::ymd_h(
    c("2019-02-01 00", "2019-02-02 00"),
    tz = "America/Vancouver"
  )
  obs <- get_bcgov_data("0450307", date_range, quiet = TRUE)
  obs <- obs |> convert_date_utc_to_local()
  # Case: date_utc the same as converting date_local to UTC
  expect_equal(obs$date_utc, obs$date_utc_from_local)
})

# Outputs: data -----------------------------------------------------------

# TODO: handle sporadic warning here: Detected an unexpected many-to-many relationship between `x` and `y`
test_that("expected data returned", {
  station <- "0450307"
  date_range <- lubridate::ymd_h(c("2019-02-01 00", "2019-02-02 00"))

  obs <- get_bcgov_data(station, date_range, quiet = TRUE)
  expect_snapshot(obs)

  obs_raw <- get_bcgov_data(station, date_range, raw = TRUE, quiet = TRUE)
  expect_snapshot(obs_raw)
})

# Helpers -----------------------------------------------------------------

test_that("able to get raw stations", {
  raw_stations <- bcgov_get_raw_stations() |>
    expect_no_error() |>
    expect_no_warning()
  expect_true(length(raw_stations) > 0)
  expect_true(is.character(raw_stations))
})

test_that("able to get qaqc years", {
  qaqc_years <- bcgov_get_qaqc_years() |>
    expect_no_error() |>
    expect_no_warning()
  expect_true(length(qaqc_years) > 0)
  expect_true(is.numeric(qaqc_years))
  expect_true(min(qaqc_years) == 1980)
  expect_true(max(qaqc_years) >= 2023) # As of 2025-06-19
})

test_that("able to differentiate qaqc/raw years", {
  years <- 1980:(Sys.Date() |> lubridate::year())
  qaqc_years <- bcgov_get_qaqc_years()
  years_to_get <- years |>
    bcgov_determine_years_to_get(qaqc_years = qaqc_years) |>
    expect_no_error() |>
    expect_no_warning()
  expect_true(length(years_to_get) %in% (length(qaqc_years) + c(0, 1)))
  expect_true(is.numeric(years_to_get))
  expect_true(min(years_to_get) == 1980)
  expect_true(max(years_to_get) %in% years)
  expect_true(max(years_to_get) >= max(qaqc_years))
})

test_that("able to get annual metadata", {
  bcgov_get_annual_stations(quiet = TRUE) |>
    expect_no_error() |>
    expect_no_warning()
  expect_error(bcgov_get_annual_stations(1979))
  bcgov_get_annual_stations(1980) |>
    expect_warning() |>
    expect_no_error()
  bcgov_get_annual_stations(2000)
  stations <- bcgov_get_annual_stations(2000)
  expect_true(nrow(stations) > 0 & ncol(stations) > 0)
  expect_true(tibble::is_tibble(stations))
})