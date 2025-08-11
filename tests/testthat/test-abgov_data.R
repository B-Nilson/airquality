test_that("basic case works", {
  date_range <- c(
    Sys.time() - lubridate::days(365 * 2),
    Sys.time() |>
      lubridate::floor_date("hours")
  ) |>
    lubridate::with_tz("UTC") |>
    format("%Y-%m-%d %H")
  stations <- c("Calgary Southeast", "Edmonton East")
  test <- stations |>
    get_abgov_data(date_range = date_range)
  expect_snapshot(test)
})

test_that("able to get stations", {
  test <- get_abgov_stations()
  expect_true(nrow(test) > 0 & ncol(test) > 0)
  expect_snapshot(names(test))
})

test_that("expected date range available", {})

test_that("qaqc data function works for all sites/parameters/date_range", {
  skip("Only to be run in interactive mode")
  date_range <- c("1990-01-01 00", "2025-06-16 00")
  test <- get_abgov_archive_data(
    stations = "all",
    date_range = date_range,
    fast = TRUE
  )
  expect_snapshot(test)
})
