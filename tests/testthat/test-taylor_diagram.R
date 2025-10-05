test_that("Plot created without error", {
  test <- get_bcgov_data(
    variables = "pm2.5",
    date_range = c("2019-01-01 00:00:00", "2019-12-31 23:00:00"),
  )
  withr::with_seed(
    seed = 1,
    test |>
      dplyr::mutate(
        obs = as.numeric(pm25_1hr),
        mod = 0.97 *
          obs +
          sample(-50:50, size = length(pm25_1hr), replace = TRUE) / 10,
        mod = ifelse(mod < 0, 0, mod),
        season = handyr::get_season(date_utc)
      )
  ) |>
    taylor_diagram(group_by = "season") |>
    expect_no_error() |>
    expect_snapshot()
})
