test_that("PurpleAir timestamp query parameters never use scientific notation", {
  params <- list(
    sensor_index = 1090L,
    start_timestamp = 1566000000,
    end_timestamp = 1566000600,
    average = 10L
  )

  formatted <- airquality:::format_purpleair_query_parameters(params)

  expect_identical(formatted$start_timestamp, "1566000000")
  expect_identical(formatted$end_timestamp, "1566000600")
  expect_false(grepl("[eE][+-]?[0-9]+", formatted$start_timestamp))
  expect_false(grepl("[eE][+-]?[0-9]+", formatted$end_timestamp))
  expect_identical(formatted$sensor_index, 1090L)
  expect_identical(formatted$average, 10L)
})

test_that("PurpleAir timestamp query formatting rounds fractional epoch seconds", {
  params <- list(
    start_timestamp = 1566000000.4,
    end_timestamp = 1566000000.6
  )

  formatted <- airquality:::format_purpleair_query_parameters(params)

  expect_identical(formatted$start_timestamp, "1566000000")
  expect_identical(formatted$end_timestamp, "1566000001")
})

test_that("PurpleAir timestamp query formatting fails closed on invalid values", {
  expect_error(
    airquality:::format_purpleair_query_parameters(
      list(start_timestamp = NA_real_)
    ),
    "start_timestamp"
  )
  expect_error(
    airquality:::format_purpleair_query_parameters(
      list(end_timestamp = c(1, 2))
    ),
    "end_timestamp"
  )
})
