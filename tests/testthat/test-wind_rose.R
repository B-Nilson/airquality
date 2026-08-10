test_that("no errors thrown", {
  expect_no_error(expect_no_warning(
    print(wind_rose(obs = example_obs))
  ))
})

test_that("missing/insufficient data handled properly", {
  expect_no_error(expect_no_warning(
    print(wind_rose(obs = example_obs[1, ]))
  ))

  print(wind_rose(obs = example_obs[1, ][-1, ])) |>
    expect_error(regexp = "nrow\\(obs\\) > 0")

  print(wind_rose(obs = example_obs |> dplyr::mutate(ws_1hr = NA))) |>
    expect_error(regexp = "No observations where wind speed is not NA")

  print(wind_rose(obs = example_obs |> dplyr::mutate(wd_1hr = NA))) |>
    expect_error(regexp = "No observations where .* wind direction is not NA")
})

test_that("default saved plot matches the golden image", {
  gg <- wind_rose(obs = example_obs)
  out_file <- tempfile(fileext = ".png")
  # Low quality keeps the golden PNG small and the render fast. Output is
  # deterministic, so byte-for-byte comparison is stable across runs.
  gg |> handyr::save_figure(out_path = out_file, quality = "low")

  expect_snapshot_file(out_file, "wind_rose.png")
})

test_that("sectors span 360/n degrees centered on compass bearings", {
  gg <- wind_rose(obs = example_obs)
  n_sectors <- nlevels(gg$data$wd_bin)
  expect_identical(gg$coordinates$limits$theta, c(0.5, n_sectors + 0.5))
})

test_that("extra features work", {
  example_obs |>
    wind_rose(facet_by = "month", facet_rows = 4, date_col = "date_local")
})
