test_that("basic case works", {
  channel <- names(get_thingspeak_public_channels())[1]

  test <- get_thingspeak_data(channel = channel) |>
    expect_no_error() |>
    expect_no_warning()
  expect_true("data" %in% names(test))
  expect_true(nrow(test$data) > 0)
})
