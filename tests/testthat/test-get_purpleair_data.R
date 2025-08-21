skip_on_cran() # cran won't have keys defined
if (
  Sys.getenv("purpleair_api_read") == "" | # tests won't work without keys available
    Sys.getenv("purpleair_api_write") == ""
) {
  skip("PurpleAir API keys not defined in .Renviron")
}

if (TRUE) {
  skip("Only test PurpleAir functions on major changes to avoid costs")
}

test_that("Keys/Org. API calls work as expected", {
  # Written to .Renviron - make your own here: https://develop.purpleair.com/
  # and run usethis::edit_r_environ("project") to open the environ file for editting
  # Entries should look like `purpleair_api_read = "YOUR-API-KEY-HERE"`
  read_key <- Sys.getenv("purpleair_api_read")
  write_key <- Sys.getenv("purpleair_api_write")

  # Keys channel
  test <- purpleair_api(read_key = read_key, channel = "keys", quiet = TRUE)
  expect_equal(test$api_key_type, "READ")
  test <- purpleair_api(write_key = write_key, channel = "keys", quiet = TRUE)
  expect_equal(test$api_key_type, "WRITE")

  # Organization channel
  test <- purpleair_api(
    read_key = read_key,
    channel = "organization",
    quiet = TRUE
  )
  expect_type(test$remaining_points, "integer")
})

test_that("Sensors API calls work as expected", {
  # Written to .Renviron - make your own here: https://develop.purpleair.com/
  # and run usethis::edit_r_environ("project") to open the environ file for editting
  # Entries should look like `purpleair_api_read = "YOUR-API-KEY-HERE"`
  read_key <- Sys.getenv("purpleair_api_read")
  write_key <- Sys.getenv("purpleair_api_write")

  channel <- "sensors"

  parameters <- list(
    nwlat = 63.595851,
    nwlng = -135.899856,
    selat = 63.592657,
    selng = -135.891057,
    fields = "temperature",
    sensor_index = 198385,
    start_timestamp = Sys.time() |>
      lubridate::with_tz("UTC") -
      lubridate::minutes(15)
  )

  expected_headers <- c("time_stamp", "sensor_index", "temperature")

  # Get Sensors Data
  test <- purpleair_api(
    read_key = read_key,
    channel = channel,
    parameters = parameters[1:5],
    quiet = TRUE
  )
  expect_equal(names(test), expected_headers)

  # Get Sensor Data
  test <- purpleair_api(
    read_key = read_key,
    channel = channel,
    parameters = parameters[5:6],
    quiet = TRUE
  )
  Sys.sleep(0.5) # Avoid API call frequency limits
  expect_length(test$time_stamp, 1)
  expect_equal(names(test), expected_headers)

  # Get Sensor History
  test <- purpleair_api(
    read_key = read_key,
    channel = channel,
    parameters = parameters[5:7],
    quiet = TRUE
  )
  expect_equal(names(test), expected_headers)
})
