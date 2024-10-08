obs = data.frame(
  date = seq(lubridate::ymd_h("2020-01-01 00"),
             lubridate::ymd_h("2023-12-31 23"), "1 hours"),
  pm25 = sample(1:150, 35064, T), o3 = sample(1:150, 35064, T),
  no2 = sample(1:150, 35064, T), so2 = sample(1:150, 35064, T)
)

test_that("Returns a list of proper size", {
  output = CAAQS(dates = obs$date, pm25_1hr_ugm3 = obs$pm25,
                o3_1hr_ppb = obs$o3, so2_1hr_ppb = obs$so2)
  expect_type(output, "list")
  expect_length(output, 4)
})

# TODO: write test
# test_that("Providing less than 3 years of consecutive data throws an error", {
#
# })

# TODO: write test
# test_that("Providing less than min_completeness*100% hours of data for a pol/year throws a warning", {
#
# })
