test_that("Plot created without error", {
  obs <- get_bcgov_data(
    stations = "0450307",
    date_range = c(Sys.time() - lubridate::days(10), Sys.time()),
    quiet = TRUE
  )
  expect_no_error(
    obs |>
      dplyr::mutate(pm25_1hr = as.numeric(pm25_1hr)) |> # drop units
      tile_plot( # TODO: handle z units properly
        x = "hour",
        y = "day",
        z = "pm25_1hr",
        FUN = mean,
        date_col = "date_utc"
      )
  )
})
