test_that("AQHI+ returns a dataframe of proper size", {
  input = 1:10
  output = AQHI_plus(input)
  expect_s3_class(output, "data.frame")
  expect_length(output[,1], n = length(input)) # rows == length(input)
  expect_length(output[1,], n = 5) # cols == 5
})

test_that("Returned data.frame has expected column classes", {
  output = AQHI_plus(1:5)
  output2 = AQHI_plus(c(1.0, 2.4, 4.8))
  expect_type(output$pm25_1hr_ugm3, "integer")
  expect_type(output2$pm25_1hr_ugm3, "double")
  expect_s3_class(output$AQHI_plus, c("factor"))
  expect_s3_class(output$risk, c("factor"))
  expect_type(output$high_risk_pop_message, c("character"))
  expect_type(output$general_pop_message, c("character"))
})

test_that("There is a value for each non-NA input", {
  input = c(NA,1:5)
  output = AQHI_plus(input)
  expect_identical(output[,1], input) # pm25_1hr_ugm3 should match inputs
  expect_true(all(is.na(unlist(output[is.na(input),])))) # NAs for all NA inputs
  expect_true(all(!is.na(unlist(output[!is.na(input),])))) # non-NAs for all non-NA inputs
})

test_that("Returned AQHI+ is accurate", {
  input = c(0, 1, 9, 9.9, 10, 10.1, 30.1, 60.1, 100, 100.1, 101)
  output = as.numeric(AQHI_plus(input)$AQHI_plus)
  expected = c(1, 1, 1, 1, 1, 2, 4, 7, 10, 11, 11)
  expect_identical(output, expected)
})
