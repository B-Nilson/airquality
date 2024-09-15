obs = data.frame(
  date = seq(lubridate::ymd_h("2024-01-01 00"),
             lubridate::ymd_h("2024-01-01 23"), "1 hours"),
  pm25 = sample(1:150, 24), o3 = sample(1:150, 24), no2 = sample(1:150, 24))

test_that("AQHI returns a dataframe of proper size", {
  output = AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25,
                o3_1hr_ppb = obs$o3, no2_1hr_ppb = obs$no2)
  expect_s3_class(output, "data.frame")
  expect_gte(nrow(output), nrow(obs)) # rows(output) >= nrows(input)
  expect_length(output[1,], n = 13) # cols == 13
})
test_that("AQHI (pm25 only) returns a dataframe of proper size", {
  output = AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25, verbose = FALSE)
  expect_warning(AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25))
  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), nrow(obs)) # rows(output) == nrows(input)
  expect_length(output[1,], n = 7) # cols == 7
})

test_that("Returned data.frame has expected column classes", {
  output = AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25,
                o3_1hr_ppb = obs$o3, no2_1hr_ppb = obs$no2)
  expect_type(output$pm25, "integer")
  expect_type(output$o3, "integer")
  expect_type(output$no2, "integer")
  expect_type(output$pm25_rolling_3hr, "double")
  expect_type(output$o3_rolling_3hr, "double")
  expect_type(output$no2_rolling_3hr, "double")
  expect_s3_class(output$AQHI, "factor")
  expect_s3_class(output$AQHI_plus, "factor")
  expect_s3_class(output$risk, "factor")
  expect_type(output$high_risk_pop_message, "character")
  expect_type(output$general_pop_message, "character")
  expect_type(output$AQHI_plus_exceeds_AQHI, "logical")
})

test_that("Returned (pm25 only) data.frame has expected column classes", {
  output = AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25, verbose = FALSE))
  expect_warning(AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25))
  expect_type(output$pm25, "integer")
  expect_s3_class(output$AQHI, "factor")
  expect_s3_class(output$AQHI_plus, "factor")
  expect_s3_class(output$risk, "factor")
  expect_type(output$high_risk_pop_message, "character")
  expect_type(output$general_pop_message, "character")
  expect_type(output$AQHI_plus_exceeds_AQHI, "logical")
})

# TODO: Not sure how to implement since AQHI changes size...
# test_that("There is a value for each non-NA input", {
#   output = AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25,
#                 o3_1hr_ppb = obs$o3, no2_1hr_ppb = obs$no2)
#
# })
