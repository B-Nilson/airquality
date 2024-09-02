
test_that("API calls work as expected", {
  # Written to .Renviron - make your own here: https://develop.purpleair.com/
  # and run usethis::edit_r_environ("project") to open the environ file for editting
  # Entries should look like `purpleair_api_read = "YOUR-API-KEY-HERE"`
  read_key = Sys.getenv("purpleair_api_read")
  write_key = Sys.getenv("purpleair_api_write")

  # Keys channel
  test = purpleair_api(read_key = read_key, channel = "keys", verbose = FALSE)
  expect_equal(test$api_key_type, "READ")
  test = purpleair_api(write_key = write_key, channel = "keys", verbose = FALSE)
  expect_equal(test$api_key_type, "WRITE")

  # Organization channel
  test = purpleair_api(read_key = read_key, channel = "organization", verbose = FALSE)
  expect_type(test$remaining_points, "integer")

  ## Sensors channel
  parameters = list(
    nwlat = 63.595851, nwlng = -135.899856, selat = 63.592657, selng = -135.891057,
    fields = c("temperature", "humidity"), 
    sensor_index = 198385, 
    start_timestamp = Sys.time() |> lubridate::with_tz("UTC") - lubridate::minutes(15))
  
  # Get Sensors Data
  test = purpleair_api(read_key = read_key, channel = "sensors", 
    parameters = parameters[1:5], verbose = FALSE)
  expect_length(names(test), 4)
  expect_equal(names(test), c("time_stamp", "sensor_index", "humidity", "temperature"))

  # Get Sensor Data
  test = purpleair_api(read_key = read_key, channel = "sensors", 
    parameters = parameters[5:6], verbose = FALSE)
  Sys.sleep(0.5) # Avoid API call frequency limits
  expect_length(names(test), 4)
  expect_length(test$time_stamp, 1)
  expect_equal(names(test), c("time_stamp", "sensor_index", "humidity", "temperature"))

  # Get Sensor History
  test = purpleair_api(read_key = read_key, channel = "sensors", 
    parameters = parameters[5:7], verbose = FALSE)
  expect_length(names(test), 4)
  expect_equal(names(test), c("time_stamp", "sensor_index", "humidity", "temperature"))

})
