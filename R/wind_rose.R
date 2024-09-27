
get_cardinal_direction = function(wd){
  wd_step = 22.5
  half_step = wd_step / 2
  dir_bins = c(
    -half_step,
    seq(half_step, 360 - half_step, by = wd_step),
    360 + half_step) 
  dir_labels = c(
    "N", "NNE", "NE", "ENE", 
    "E", "ESE", "SE", "SSE", 
    "S", "SSW", "SW", "WSW", 
    "W", "WNW", "NW", "NNW", "N")
  cut(wd, breaks = dir_bins, labels = dir_labels, right = FALSE)
}

wind_rose = function(
    obs, 
    data_cols = c(ws = "ws_1hr_ms", wd = "wd_1hr_degrees"), 
    groups = NULL, 
    ws_min = 0,
    ws_step = 2,
    alpha = 0.7) {
  if(is.null(names(data_cols))) names(data_cols) = c("ws", "wd")
  # Get wind speed bins
  max_ws = max(obs[[data_cols[1]]], na.rm = TRUE) + 0.01
  speed_bins  = unique(c(seq(ws_min, max_ws, by = ws_step), max_ws))
  # Get counts/percents for each ws bin at eac hcardinal direction
  rose_data = obs |>
    dplyr::rename(dplyr::all_of(data_cols)) |>
    dplyr::filter(ws >= ws_min) |>
    dplyr::mutate(
      ws_bin = ws |> cut(breaks = speed_bins, right = FALSE),
      wd_bin = .data$wd |> get_cardinal_direction()) |>
    dplyr::group_by(ws_bin, wd_bin, dplyr::across(dplyr::all_of(groups))) |> 
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::mutate(p = n / sum(n), .groups = "drop") |>
    tidyr::complete(wd_bin = factor(levels(wd_bin), levels(wd_bin)), ws_bin) |>
    dplyr::mutate(p = swap_na(p, 0))
  # Determine most frequent direction
  most_frequent = rose_data |>
    dplyr::group_by(wd_bin) |>
    dplyr::summarise(p = sum(p, na.rm = TRUE)) |>
    dplyr::filter(p == max(p, na.rm = TRUE))

  # Make base plot
  gg = ggplot2::ggplot(rose_data) +
    ggplot2::coord_radial(r.axis.inside = TRUE,
      start = -convert_units(22.5 / 2, "degrees", "radians")) +
    ggplot2::geom_hline(yintercept = most_frequent$p[1] * 1.05) + 
    ggplot2::scale_fill_viridis_d(direction = -1) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_x_discrete(labels = \(x) ifelse(nchar(x) < 3, x, "")) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(c(0, 0.02)),
      labels = \(x) ifelse(x > 0, round(x * 100) |> paste0("%"), ""))  +
    ggplot2::labs(
      x = ggplot2::element_blank(),
      y = ggplot2::element_blank())
  # Add data and basic theming
  gg +
    ggplot2::geom_bar( 
      ggplot2::aes(
        x = .data$wd_bin, y = .data$p, 
        fill = forcats::fct_rev(.data$ws_bin)), 
      colour = "black", alpha = alpha,
      stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.direction = "horizontal", 
      legend.position = "top") +
    ggplot2::labs(fill = "Wind Speed (m/s)")
}
