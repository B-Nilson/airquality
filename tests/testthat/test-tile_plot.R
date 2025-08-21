test_that("Plot created without error", {
  date_range <- lubridate::ymd_h(c("2019-02-01 00", "2019-04-02 00"))
  obs <- get_station_data(
    "Vanderhoof, BC, Canada", date_range,
    sources = "BCgov",
    quiet = TRUE
  )$data
  expect_no_error(obs |> 
    tile_plot(x = "hour", y = "day", z = "pm25_1hr_ugm3", FUN = mean, date_col = "date_utc"))
})
