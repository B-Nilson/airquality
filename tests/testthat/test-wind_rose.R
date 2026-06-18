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
    expect_error(regexp = "No observations where wind speed >= 0")

  print(wind_rose(obs = example_obs |> dplyr::mutate(wd_1hr = NA))) |>
    expect_error(regexp = "No observations where .* wind direction is not NA")
})
