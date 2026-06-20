test_that("Plot created without error", {
  expect_no_error(expect_no_warning(
    example_obs |>
      tile_plot(
        x = "hour",
        y = "day",
        z = "pm25_1hr",
        FUN = mean,
        date_col = "date_local"
      ) |>
      print()
  ))
})
