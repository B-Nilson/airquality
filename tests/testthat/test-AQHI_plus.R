test_that("AQHI+ returns expected output", {
  expect_snapshot(AQHI_plus(1:101))
})

test_that("There is a value for each non-NA input", {
  pm25_hourly <- c(NA, 1:5)
  aqhi_p <- AQHI_plus(pm25_hourly)
  # pm25_1hr_ugm3 should match inputs
  expect_identical(
    aqhi_p$pm25_1hr_ugm3, pm25_hourly
  )
  # NAs for all NA inputs
  expect_true(all(is.na(
    unlist(aqhi_p[is.na(pm25_hourly), ])
  )))
  # non-NAs for all non-NA inputs
  expect_true(all(!is.na(
    unlist(aqhi_p[!is.na(pm25_hourly), ])
  )))
})

test_that("Returned AQHI+ is accurate", {
  pm25_hourly <- c(0, 1, 9, 9.9, 10, 10.1, 30.1, 60.1, 100, 100.1, 101)
  aqhi_p <- as.numeric(AQHI_plus(pm25_hourly)$AQHI_plus)
  expected <- c(1, 1, 1, 1, 1, 2, 4, 7, 10, 11, 11)
  expect_identical(aqhi_p, expected)
})
