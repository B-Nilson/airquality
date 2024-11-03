test_that("Plot created without error", {
  skip("Need to make proper example dataset")
  # Real test data
  data <- data.table::fread(
    "https://aqmap.ca/aqmap/data/plotting/purpleair/sensor_31411_recent_hourly.csv",
    data.table = FALSE
  ) |>
    dplyr::select(obs = pm25_80402_0.91km, mod = pm25) |>
    dplyr::mutate(sensor_id = "31411", sensor_type = "PA", province = "AB", colocated = TRUE) |>
    dplyr::bind_rows(
      data.table::fread(
        "https://aqmap.ca/aqmap/data/plotting/purpleair/sensor_201873_recent_hourly.csv",
        data.table = FALSE
      ) |>
        dplyr::select(obs = pm25_100202_0.02km, mod = pm25) |>
        dplyr::mutate(sensor_id = "201873", sensor_type = "PA", province = "BC", colocated = TRUE)
    ) |>
    dplyr::bind_rows(
      data.table::fread(
        "https://aqmap.ca/aqmap/data/plotting/purpleair/sensor_188701_recent_hourly.csv",
        data.table = FALSE
      ) |>
        dplyr::select(obs = pm25_21100004_107.8km, mod = pm25) |>
        dplyr::mutate(sensor_id = "103974", sensor_type = "PA", province = "BC", colocated = FALSE)
    ) |>
    dplyr::bind_rows(
      data.table::fread(
        "https://aqmap.ca/aqmap/data/plotting/aqegg/sensor_egg0004a30b00027b24_recent_hourly.csv",
        data.table = FALSE
      ) |>
        dplyr::select(obs = pm25_102701_0.43km, mod = pm25) |>
        dplyr::mutate(sensor_id = "egg0004a30b00027b24", sensor_type = "EGG", province = "BC", colocated = FALSE)
    ) |>
    dplyr::mutate(dplyr::across(-(1:2), factor))

  expect_no_error(taylor_diagram(data, group_by = c("sensor_id", "sensor_type"), facet_by = "province"))
})
