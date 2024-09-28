get_cardinal_direction = function(wd, wd_step = 22.5){
  dir_labels = c(
    "N", "NNE", "NE", "ENE", 
    "E", "ESE", "SE", "SSE", 
    "S", "SSW", "SW", "WSW", 
    "W", "WNW", "NW", "NNW", "N")
  if(wd_step == 22.5) {
    set = 1:length(dir_labels)
  }else if(wd_step == 45) {
    set = seq(1, length(dir_labels), 2)
  }else if(wd_step == 90) {
    set = seq(1, length(dir_labels), 4)
  }else{
    stop("Provided wd_step is not supported")
  }

  half_step = wd_step / 2
  dir_bins = c(
    -half_step,
    seq(half_step, 360 - half_step, by = wd_step),
    360 + half_step) 
  cut(wd, breaks = dir_bins, labels = dir_labels[set])
}

wind_rose = function(
    obs, 
    data_cols = c(ws = "ws_1hr_ms", wd = "wd_1hr_degrees"), 
    groups = NULL, 
    wd_nbins = c(16, 8, 4)[1],
    ws_min = 0,
    ws_step = 2,
    alpha = 0.7) {
  if(is.null(names(data_cols))) names(data_cols) = c("ws", "wd")
  if(is.null(names(groups))) names(groups) = groups
  wd_step = dplyr::case_when(
    wd_nbins == 16 ~ 22.5,
    wd_nbins == 8  ~ 45,
    wd_nbins == 4  ~ 90)
  if(is.na(wd_step)){
    stop("wd_step must be either 16, 8, or 4")
  }
  # Get counts/percents for each ws bin at eac hcardinal direction
  rose_data = obs |>
    dplyr::rename(dplyr::all_of(data_cols)) |>
    dplyr::filter(ws >= ws_min) |>
    dplyr::mutate(
      ws_bin = .data$ws |>
        cut_wind_speed(ws_min = ws_min, ws_step = ws_step),
      wd_bin = get_cardinal_direction(.data$wd, wd_step = wd_step)) |>
    dplyr::group_by(ws_bin, wd_bin, dplyr::across(dplyr::all_of(groups))) |> 
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::mutate(p = n / sum(n)) |>
    tidyr::complete(wd_bin = factor(levels(wd_bin), levels(wd_bin))) |>
    dplyr::mutate(p = swap_na(p, 0), ws_bin = swap_na(ws_bin, 1) |> factor(labels = levels(ws_bin)))
  # Determine most frequent direction
  most_frequent = rose_data |>
    dplyr::group_by(wd_bin) |>
    dplyr::summarise(p = sum(p, na.rm = TRUE)) |>
    dplyr::filter(p == max(p, na.rm = TRUE))

  # Make base plot
  gg = ggplot2::ggplot(rose_data) +
    ggplot2::coord_radial(r.axis.inside = TRUE,
       start = -convert_units(wd_step / 2, "degrees", "radians")) +
    ggplot2::geom_hline(yintercept = most_frequent$p[1] * 1.05) + 
    ggplot2::scale_fill_viridis_d(direction = -1) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_x_discrete(
      labels = \(x) ifelse(nchar(x) < 3, x, "")) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(c(0, 0.02)),
      labels = \(x) ifelse(x > 0, round(x * 100) |> paste0("%"), ""))  +
    ggplot2::labs(
      x = ggplot2::element_blank(),
      y = ggplot2::element_blank())
  if (!is.null(groups)) gg = gg + 
    ggplot2::facet_wrap(stats::setNames(names(groups), names(groups)),
      labeller = "label_both")
  # Add data and basic theming
  gg +
    ggplot2::geom_col(
      ggplot2::aes(
        x = .data$wd_bin, y = .data$p, 
        fill = forcats::fct_rev(.data$ws_bin)), 
      colour = "black", alpha = alpha) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.direction = "horizontal",
      legend.position = "bottom",
      strip.text = ggplot2::element_text(size = 12,
        face = "italic", margin = ggplot2::margin(b = 5)),
      plot.background = ggplot2::element_rect(
        fill = "white", colour = "black"),
      panel.border = ggplot2::element_rect(
        colour = NA, fill = NA)) +
    ggplot2::labs(fill = "Wind Speed (m/s)")
}

cut_wind_speed = function(ws, ws_min = 0, ws_step = 2) {
  max_ws = max(ws, na.rm = TRUE)
  speed_bins  = seq(ws_min, max_ws, by = ws_step) |> 
    c(max_ws) |> unique()
  n_bins = length(speed_bins)
  speed_labels = paste(
    speed_bins, "-", dplyr::lead(speed_bins))[1:(n_bins - 1)]

  cut(ws, include.lowest = TRUE, 
    breaks = speed_bins, labels = speed_labels)
}