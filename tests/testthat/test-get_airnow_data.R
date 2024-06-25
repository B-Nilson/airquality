test_that("returns requested stations only", {
  obs = get_airnow_data("000010102", "2019-01-01 00")
  expect_true(all(unique(obs$site_id) %in% "000010102"))
  obs = get_airnow_data(c("000010102", "000010401"), "2019-01-01 00")
  expect_true(all(unique(obs$site_id) %in% c("000010102", "000010401")))
})

test_that("all dates non-na and within requested period", {
  date_range = lubridate::ymd_h(c("2019-01-01 00", "2019-01-01 01"), tz = "America/Toronto")
  obs = get_airnow_data("000010102", date_range)
  expect_true(all(!is.na(obs$date_utc)))
  expect_true(all(!is.na(obs$date_local)))
  # TODO: Fix dates being filtered by utc not daterange timezone
  expect_true(all(obs$date_utc %>% dplyr::between(date_range[1], date_range[2])))
})

test_that("invalid date_range causes error/warning", {
  # TODO: Improve messaging so tests pass
  expect_error(get_airnow_data("000010102", "bananas"))
  expect_error(get_airnow_data("000010102", c("1919-01-01 00", "1919-01-01 01", "1919-01-01 02")))
  expect_error(get_airnow_data("000010102", "2010-01-01 00"))
  expect_error(get_airnow_data("000010102", "2029-01-01 00"))
  expect_warning(get_airnow_data("000010102", date_range = c("1919-01-01 00","2014-01-01 01")))
  expect_warning(get_airnow_data("000010102", c(format(Sys.time() - lubridate::hours(4), "%F %H"), "2029-01-01 00")))
})

test_that("unknown stations cause warning", {
  # TODO: Improve messaging so tests pass
  expect_warning(get_airnow_data("bananas", date_range = "2019-01-02 00"))
  expect_warning(get_airnow_data(c("bananas", "000010102"), date_range = "2019-01-02 00"))
})
