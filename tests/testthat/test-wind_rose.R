test_that("no errors thrown", {
  date_range = lubridate::ymd_h(c("2019-02-01 00", "2019-02-28 23"), tz = "America/Vancouver")
  obs = get_station_data("Vanderhoof, BC, Canada", date_range, sources = "BCgov")$data |>
    dplyr::select("date_local", "site_id", "ws_1hr_ms", "wd_1hr_degrees") |>
    dplyr::distinct()
  expect_no_error(expect_no_warning(
    wind_rose(obs)))
})
