# Plot created without error

    Code
      expect_no_error(taylor_diagram(withr::with_seed(seed = 1, dplyr::mutate(test,
        obs = as.numeric(pm25_1hr), mod = 0.97 * obs + sample(-50:50, size = length(
          pm25_1hr), replace = TRUE) / 10, mod = ifelse(mod < 0, 0, mod), season = handyr::get_season(
          date_utc))), group_by = "season"))

