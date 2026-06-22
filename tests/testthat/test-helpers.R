test_that("add_features works", {
  features <- c("year", "month", "houR", "Day", "SeAsOn")
  out <- example_obs |>
    dplyr::mutate(year = 1) |> # for ensuring not overwritten later
    add_features(features = features, date_col = "date_local") |>
    expect_no_error()

  # all features present
  expect_contains(names(out), features)

  # all features correct
  expect_identical(lubridate::month(out$date_local), out$month)
  expect_identical(lubridate::hour(out$date_local), out$houR)
  expect_identical(lubridate::day(out$date_local), out$Day)
  expect_identical(
    handyr::get_season(out$date_local, as_factor = TRUE),
    out$SeAsOn
  )

  # existing features not overwritten
  expect_identical(out$year, rep(1, nrow(out)))

  # error if date_col not provided
  expect_error(
    example_obs |>
      add_features(features = features),
    regexp = "`date_col` must be provided"
  )
})
