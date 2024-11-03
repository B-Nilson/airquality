# TODO: add option to display n obs inside each cell
# TODO: handle named periods instead of integers for (month, quarter, wday)
# TODO: handle timezones?
tile_plot <- function(obs, x, y, z, FUN = mean, date_col = "date_utc", ...) {
  # Handle date-based grouping options
  special_cases <- c(
    "year", "quarter", "month", "day", "wday",
    "hour", "minute", "second"
  )
  if (x %in% special_cases & !x %in% names(obs)) {
    lubridate_fun <- getExportedValue("lubridate", x)
    obs[[x]] <- lubridate_fun(obs[[date_col]])
  }
  if (y %in% special_cases & !y %in% names(obs)) {
    lubridate_fun <- getExportedValue("lubridate", y)
    obs[[y]] <- lubridate_fun(obs[[date_col]])
  }

  # Summarise using FUN(...) across each x/y pair, filling gaps with NAs
  pd <- obs |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(x = x, y = y)))) |>
    dplyr::summarise(z = get(z) |> FUN(...), .groups = "drop") |>
    dplyr::mutate(dplyr::across(c(x, y), factor)) |>
    tidyr::complete(x, y)
  
  # Make gg tile plot with good defaults
  pd |>
    ggplot2::ggplot() +
    ggplot2::geom_tile(
      ggplot2::aes(x, y, fill = z),
      colour = "black"
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_fill_viridis_c(na.value = NA, limits = c(0, NA)) +
    ggpubr::theme_pubr(border = TRUE) +
    ggplot2::labs(
      x = x, y = y,
      fill = z
    )
}
