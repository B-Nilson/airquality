test_that("basic case works", {
  date_range <- handyr::check_date_range("now")
  obs <- get_abgov_data(
    date_range = date_range,
    variables = "pm25",
    quiet = TRUE
  )

  # Case: All expected columns
  expect_equal(
    c(
      "site_name",
      "quality_assured",
      "date_utc",
      "date_local",
      "pm25_1hr"
    ),
    names(obs)
  )

  # Case: All date_utc non-NA
  expect_true(all(!is.na(obs$date_utc)))
  # Case: All date_local non-NA
  expect_true(all(!is.na(obs$date_local)))
  # Case: All date_utc within requested date range
  expect_true(all(obs$date_utc |> dplyr::between(date_range[1], date_range[2])))

  # Case: date_utc the same as converting date_local to UTC
  obs_2 <- obs |> convert_date_utc_to_local()
  expect_equal(obs_2$date_utc, obs_2$date_utc_from_local)
})

test_that("able to get stations", {
  test <- get_abgov_stations(quiet = TRUE)
  expect_true(nrow(test) > 0 & ncol(test) > 0)
  expect_snapshot(names(test))
})

test_that("quiet arg works", {
  expect_no_message(expect_no_warning(expect_no_error(
    {
      stations <- get_abgov_stations(quiet = TRUE)
      stations$site_name[1] |>
        get_abgov_data(quiet = TRUE, variables = "pm25")
    }
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
