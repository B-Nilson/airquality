# TODO: repeat for each pollutant
test_that("AQI for O3 1hr only is correct", {
  expect_equal(AQI(Sys.time(), o3_8hr_ppm = 0.07853333)$AQI, 126)
})

test_that("Throws error when no data provided", {
  expect_error(AQI())
})

test_that("AQI for O3 1hr and O3 8hr is correct", {
  expect_equal(AQI(Sys.time(), o3_8hr_ppm = 0.078, o3_1hr_ppm = 0.162)$AQI, 148)
})

# TODO: write test
test_that("AQI for SO2 1hr and SO2 24hr are correct", {
})

test_that("AQI for O3 8hr, PM2.5 and CO is correct", {
  expect_equal(AQI(Sys.time(), o3_8hr_ppm = 0.078,
                   pm25_24hr_ugm3 = 35.9, co_8hr_ppm = 8.4)$AQI, 126)
})

# TODO: write test for concentration beyond the AQI


# Inputs: dates -----------------------------------------------------------


# Inputs: Ozone -----------------------------------------------------------


# Inputs: PM2.5 and PM10 --------------------------------------------------


# Inputs: CO --------------------------------------------------------------


# Inputs: SO2 and NO2 -----------------------------------------------------


# Outputs: Data Structure -------------------------------------------------


# Outputs: AQI ------------------------------------------------------------
