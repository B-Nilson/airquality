test_that("returns requested stations only", {
  obs = get_bc_data("0450307", "2019-01-01 00")
  expect_true(all(unique(obs$site_id) %in% "0450307"))
  obs = get_bc_data(c("0450307", "E206898"), "2019-01-01 00")
  expect_true(all(unique(obs$site_id) %in% c("0450307", "E206898")))
})

test_that("all dates non-na and within requested period", {
  date_range = lubridate::ymd_h(c("2019-01-01 00", "2019-01-02 00"), tz = "America/Toronto")
  obs = get_bc_data("0450307", date_range)
  expect_true(all(!is.na(obs$date_utc) & !is.na(obs$date_local)))
  expect_true(all(obs$date_utc %>% dplyr::between(date_range[1], date_range[2])))
})

test_that("invalid date_range causes error/warning", {
  expect_error(get_bc_data("0450307", "bananas"))
  expect_error(get_bc_data("0450307", c("1919-01-01 00", "1919-01-01 01", "1919-01-01 02")))
  expect_error(get_bc_data("0450307", "1919-01-01 00"))
  expect_error(get_bc_data("0450307", "2029-01-01 00"))
  expect_warning(get_bc_data("0450307", date_range = c("1919-01-01 00","1990-01-01 01")))
  expect_warning(get_bc_data("0450307", c(format(Sys.time() - lubridate::hours(4), "%F %H"), "2029-01-01 00")))
})

test_that("unknown stations cause warning", {
  expect_warning(get_bc_data("bananas", date_range = "2019-01-02 00"))
  expect_warning(get_bc_data(c("bananas", "0450307"), date_range = "2019-01-02 00"))
})
