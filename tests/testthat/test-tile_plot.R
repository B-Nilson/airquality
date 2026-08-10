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

test_that("default saved plot matches the golden image", {
  gg <- example_obs |>
    tile_plot(
      x = "hour",
      y = "day",
      z = "pm25_1hr",
      FUN = mean,
      date_col = "date_local"
    )

  out_file <- tempfile(fileext = ".png")
  # Low quality keeps the golden PNG small and the render fast. Rendering is
  # deterministic, so byte-for-byte comparison is stable across runs.
  gg |> handyr::save_figure(out_path = out_file, quality = "low")

  expect_snapshot_file(out_file, "tile_plot.png")
})
