make_model_obs <- function() {
  lapply(1:5, \(i) {
    withr::with_seed(
      seed = i,
      example_obs |>
        dplyr::mutate(
          obs = as.numeric(pm25_1hr),
          mod = sample(80:120 / 100, size = length(pm25_1hr), replace = TRUE) *
            obs +
            sample(-10:10, size = length(pm25_1hr), replace = TRUE) * obs / 10,
          mod = ifelse(mod < 0, 0, mod),
          model_name = paste0("model_", i)
        )
    )
  }) |>
    dplyr::bind_rows()
}

test_that("Plot created without error", {
  obs <- make_model_obs()

  obs |>
    taylor_diagram(group_by = "model_name") |>
    print() |>
    expect_no_warning() |>
    expect_no_error()

  obs |>
    taylor_diagram(
      group_by = "model_name",
      facet_by = "Season",
      date_col = "date_local",
      facet_rows = 2,
      obs_point_options = list(label_padding = 0.1),
      labels_padding = 2
    ) |>
    print() |>
    expect_no_warning() |>
    expect_no_error()
})

test_that("legend titles follow group_by names", {
  data <- example_obs |>
    dplyr::mutate(
      obs = as.numeric(pm25_1hr),
      mod = obs * 1.1,
      model_name = "model_1",
      temp_bin = cut(as.numeric(temp_1hr), breaks = 3),
      ws_bin = cut(as.numeric(ws_1hr), breaks = 3)
    )

  # Case: two grouping variables (colour + shape)
  gg <- data |>
    taylor_diagram(group_by = c(Model = "model_name", Temp = "temp_bin"))
  expect_identical(gg$labels$colour, "Model")
  expect_identical(gg$labels$shape, "Temp")

  # Case: three grouping variables (colour + shape + fill)
  gg <- data |>
    taylor_diagram(
      group_by = c(Model = "model_name", Temp = "temp_bin", Ws = "ws_bin")
    )
  expect_identical(gg$labels$colour, "Model")
  expect_identical(gg$labels$shape, "Temp")
  expect_identical(gg$labels$fill, "Ws")
})

test_that("default saved plot matches the golden image", {
  obs <- make_model_obs()

  gg <- obs |> taylor_diagram(group_by = "model_name")
  out_file <- tempfile(fileext = ".png")
  # Low quality keeps the golden PNG small and the render fast. The data is
  # built with fixed seeds, so rendering is deterministic and byte-for-byte
  # comparison is stable across runs.
  gg |> handyr::save_figure(out_path = out_file, quality = "low")

  expect_snapshot_file(out_file, "taylor_diagram.png")
})
