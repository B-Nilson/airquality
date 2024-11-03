
tile_plot <- function(obs, x, y, z, FUN = mean, date_col = "date_utc", ...) {
  special_cases <- c("year", "quarter", "month", "day", "hour", "minute")
  if (x %in% special_cases & !x %in% names(obs)) {
    obs[[x]] <- getExportedValue("lubridate", x)(obs[[date_col]])
  }
  if (y %in% special_cases & !y %in% names(obs)) {
    obs[[y]] <- getExportedValue("lubridate", y)(obs[[date_col]])
  }
  pd <- obs |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(x = x, y = y)))) |>
    dplyr::summarise(z = get(z) |> FUN(...), .groups = "drop") |>
    dplyr::mutate(dplyr::across(c(x, y), factor)) |>
    tidyr::complete(x, y)

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
