test_that("basic case works", {
  station <- "0450307"
  date_range <- handle_date_range("now")
  obs <- expect_no_warning(expect_no_error(
    get_bcgov_data(
      stations = station,
      date_range = date_range,
      quiet = TRUE
    )
  ))

  # Case: All expected columns
  expect_equal(
    c(
      "date_utc",
      "date_local",
      "site_id",
      "quality_assured"
    ),
    names(obs)[1:4]
  )

  # Case: All date_utc non-NA
  expect_true(all(!is.na(obs$date_utc)))
  # Case: All date_local non-NA
  expect_true(all(!is.na(obs$date_local)))
  # Case: All date_utc within requested date range
  expect_true(all(obs$date_utc |> dplyr::between(date_range[1], date_range[2])))
  expect_true(all(unique(obs$site_id) %in% station))

  # Case: date_utc the same as converting date_local to UTC
  obs_2 <- obs |> convert_date_utc_to_local()
  expect_equal(obs_2$date_utc, obs_2$date_utc_from_local)
})

# Input: stations ---------------------------------------------------------

test_that("unknown stations cause warning", {
  stations <- c("bananas", "0450307")
  # Case: All stations invalid
  expect_error(get_bcgov_data(stations[1], quiet = TRUE))
  # Case: Some stations invalid
  expect_warning(get_bcgov_data(stations, quiet = TRUE))
})

# Inputs: date_range ------------------------------------------------------

# TODO: test handle_date_range() instead to save time
test_that("invalid date_range causes error", {
  station <- "0450307"
  # Case: invalid input value
  expect_error(get_bcgov_data(station, date_range = "bananas", quiet = TRUE))
  # Case: too many dates
  expect_error(get_bcgov_data(
    station,
    c("1919-01-01 00", "1919-01-01 01", "1919-01-01 02"),
    quiet = TRUE
  ))
})

test_that("too early date_range causes warning/error", {
  skip("Slow to run, skipping for now")
  # TODO: Error: No data available for provided stations / date_range / parameters.
  station <- "M110517"
  earliest_time <- lubridate::ymd_h("1980-01-01 01", tz = bcgov_tzone)
  # Case: All in the past
  expect_error(
    station |>
      get_bcgov_data(
        date_range = earliest_time - lubridate::hours(1),
        quiet = TRUE
      )
  )
  # Case: Partly in the past
  date_range <- c(earliest_time - lubridate::hours(1), earliest_time)
  expect_warning(
    station |>
      get_bcgov_data(date_range = date_range, quiet = TRUE)
  )
})

test_that("too late date_range causes warning/error", {
  skip("Slow to run, skipping for now")
  station <- "0450307"
  current_time <- lubridate::floor_date(Sys.time(), "hours")
  future_time <- current_time + lubridate::hours(24)
  # Case: All in the future
  expect_error(get_bcgov_data(station, future_time, quiet = TRUE))
  # Case: Partly in the future
  date_range <- c(current_time - lubridate::hours(24), future_time)
  expect_warning(get_bcgov_data(station, date_range, quiet = TRUE))
})

# Metadata ----------------------------------------------------------------

test_that("able to get metadata", {
  metadata <- get_bcgov_stations(quiet = TRUE) |>
    expect_no_error() |>
    expect_no_warning()
  # Dims/class correct
  expect_true(nrow(metadata) > 0 & ncol(metadata) > 0)
  expect_true(tibble::is_tibble(metadata))
  # No NAs where shouldn't be
  expect_true(all(!is.na(metadata$site_id)))
  expect_true(all(!is.na(metadata$site_name)))
  expect_true(all(!is.na(metadata$lat)))
  expect_true(all(!is.na(metadata$lng)))
  expect_true(all(!is.na(metadata$tz_local)))
})

test_that("use_sf arg works", {
  skip_if_not_installed("sf")
  metadata <- get_bcgov_stations(use_sf = TRUE, quiet = TRUE) |>
    expect_no_error() |>
    expect_no_warning()
  # Dims/class correct
  expect_true(nrow(metadata) > 0 & ncol(metadata) > 0)
  expect_s3_class(metadata, "sf")
  # No NAs where shouldn't be
  expect_true(all(!is.na(metadata$site_id)))
  expect_true(all(!is.na(metadata$site_name)))
  expect_true(all(!is.na(metadata$geometry)))
  expect_true(all(!is.na(metadata$tz_local)))
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

test_that("able to get raw stations", {
  raw_stations <- bcgov_get_raw_stations() |>
    expect_no_error() |>
    expect_no_warning()
  expect_true(length(raw_stations) > 0)
  expect_true(is.character(raw_stations))
})

# Helpers -----------------------------------------------------------------

test_that("able to get qaqc years", {
  qaqc_years <- bcgov_get_qaqc_years() |>
    expect_no_error() |>
    expect_no_warning()
  expect_true(length(qaqc_years) > 0)
  expect_true(is.numeric(qaqc_years))
  expect_true(min(qaqc_years) == 1980)
  expect_true(max(qaqc_years) >= 2023) # As of 2025-06-19
})

test_that("able to get qaqc year params", {
  qaqc_params <- bcgov_get_qaqc_year_params(2000) |>
    expect_no_error() |>
    expect_no_warning()
  expect_true(length(qaqc_params) > 0)
  expect_true(is.character(qaqc_params))
  expect_true(all(qaqc_params %in% .bcgov_columns$values))
})

test_that("able to make qaqc paths", {
  path <- bcgov_make_qaqc_paths(1980, c("PM25", "SO2")) |>
    expect_no_warning() |>
    expect_no_error()
  expect_true(length(path) == 1)
  expect_true(is.character(path))
  expect_true(
    stringr::str_detect(path, "SO2") &
      stringr::str_detect(path, "1980")
  )
})

test_that("able to differentiate qaqc/raw years", {
  date_range <- c(as.POSIXct("1980-01-01 00", tz = bcgov_tzone), lubridate::now(tz = bcgov_tzone))
  years <- 1980:(Sys.Date() |> lubridate::year())
  qaqc_years <- bcgov_get_qaqc_years()
  years_to_get <- date_range |>
    bcgov_determine_years_to_get(qaqc_years = qaqc_years) |>
    expect_no_error() |>
    expect_no_warning()
  expect_true(length(years_to_get) %in% (length(qaqc_years) + c(0, 1)))
  expect_true(is.numeric(years_to_get))
  expect_true(min(years_to_get) == 1980)
  expect_true(max(years_to_get) %in% years)
  expect_true(max(years_to_get) >= max(qaqc_years))
})
