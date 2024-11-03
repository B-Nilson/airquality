# Inputs: locations -------------------------------------------------------

# Inputs: date_range ------------------------------------------------------

test_that("invalid date_range causes error", {
  location <- "Vanderhoof, BC, Canada"
  # Case: invalid input value
  expect_error(get_station_data(location, "bananas"))
  # Case: too many dates
  expect_error(get_station_data(location, c("1919-01-01 00", "1919-01-01 01", "1919-01-01 02")))
})

# Inputs: buffer_dist -----------------------------------------------------


# Inputs: networks --------------------------------------------------------


# Inputs: sources ---------------------------------------------------------

# Outputs: dates ---------------------------------------------------------

# Outputs: data -----------------------------------------------------------

test_that("expected data returned", {
  date_range <- lubridate::ymd_h(c("2019-02-01 00", "2019-02-02 00"))
  obs <- get_station_data(
    "Vanderhoof, BC, Canada", date_range,
    verbose = FALSE
  )$data
  expect_snapshot(obs)
})
