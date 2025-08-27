test_that("basic case works", {
  obs <- get_abgov_data(variables = "pm25", quiet = TRUE)

  # Case: All expected columns
  expect_equal(
    c(
      "site_name",
      "quality_assured",
      "date_utc",
      "date_local",
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

test_that("able to get stations", {
  test <- get_abgov_stations()
  expect_true(nrow(test) > 0 & ncol(test) > 0)
  expect_snapshot(names(test))
})

test_that("quiet arg works", {
  date_range <- c(
    Sys.time() - lubridate::days(2),
    Sys.time() |>
      lubridate::floor_date("hours")
  ) |>
    lubridate::with_tz("UTC") |>
    format("%Y-%m-%d %H")
  stations <- get_abgov_stations()
  expect_no_message(expect_no_warning(expect_no_error(
    stations$site_name[1] |>
      get_abgov_data(quiet = TRUE, date_range = date_range)
  )))
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
