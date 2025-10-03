test_that("no errors thrown", {
  date_range <- lubridate::ymd_h(
    c("2019-02-01 00", "2019-02-28 23"),
    tz = "America/Vancouver"
  )
  obs <- get_bcgov_data(
    stations = "0450307",
    date_range = c(Sys.time() - lubridate::days(10), Sys.time()),
    quiet = TRUE
  )
  expect_no_error(expect_no_warning(
    wind_rose(obs, facet_by = "site_id")
  ))
})
