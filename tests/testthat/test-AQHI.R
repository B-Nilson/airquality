test_that("AQHI returns expected output", {
  obs <- data.frame(
    date = seq(
      lubridate::ymd_h("2024-01-01 00"),
      lubridate::ymd_h("2024-01-01 23"), "1 hours"
    ),
    pm25 = 1:24, o3 = 1:24, no2 = 1:24
  )
  # All 3 pollutants (AQHI and AQHI+)
  expect_snapshot(
    AQHI(
      dates = obs$date, pm25_1hr_ugm3 = obs$pm25,
      o3_1hr_ppb = obs$o3, no2_1hr_ppb = obs$no2, verbose = FALSE
    )
  )

  # PM2.5 only (AQHI+)
  expect_snapshot(
    AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25, verbose = FALSE)
  )
})

# TODO: Not sure how to implement since AQHI changes size...
# test_that("There is a value for each non-NA input", {
#   output = AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25,
#                 o3_1hr_ppb = obs$o3, no2_1hr_ppb = obs$no2)
#
# })
