test_that("AQI returns expected output", {
  expect_snapshot(
    AQI(
      dates = lubridate::ymd_h("2024-01-01 00"),
      o3_8hr_ppm = 0.078,
      pm25_24hr_ugm3 = 35.9,
      co_8hr_ppm = 8.4
    )
  )
  expect_error(AQI())
})

# TODO: write test
test_that("AQI for PM2.5 is correct", {})

# TODO: write test
test_that("AQI for PM10 is correct", {})

# TODO: write test
test_that("AQI for NO2 is correct", {})

test_that("AQI for O3 is correct", {
  output <- AQI(
    dates = c(Sys.time(), Sys.time() - lubridate::days(1)),
    o3_8hr_ppm = c(0.07853333, 0.078),
    o3_1hr_ppm = c(NA, 0.162)
  )
  expect_equal(output$AQI, c(148, 126))
})

# TODO: write test
test_that("AQI for SO2 is correct", {})

# TODO: write test
test_that("AQI for CO is correct", {})

# TODO: add more values to test
test_that("AQI for multi-pollutant is correct", {
  output <- AQI(
    dates = Sys.time(),
    o3_8hr_ppm = 0.078,
    pm25_24hr_ugm3 = 35.9,
    co_8hr_ppm = 8.4
  )
  expect_equal(output$AQI, 126)
})

# TODO: write test for concentration beyond the AQI

# TODO: test that principal pol determination is correct
