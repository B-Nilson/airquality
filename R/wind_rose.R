# TODO: handle other units
# TODO: handle fill palletes
# TODO: Present amount of missing values inside center of plot
# TODO: add data date range to subtitle or caption

#' Create wind rose diagrams to assess patterns in wind speed and direction
#'
#' @param obs Observation data.frame with (at least) all columns in `data_cols` and (if provided) `facet_by`. 
#' @param data_cols (Optional) a character vector with 2 values indication column names in `obs` to get observed and modelled values.
#'   Default assumes columns "obs" and "mod" exist.
#' @param facet_by (Optional) a character vector with 1 or 2 column names to use as facets in `ggplot2::facet_wrap()`.
#'   If names are present they will be used as the corresponding facet titles.
#'   Default (NULL) does not facet the plot.
#' @param facet_rows (Optional) a single numeric value indicating the number of rows to use in facetting if `facet_by` values provided. 
#'   Default is a single row.
#' @param ws_min (Optional) a single value indicating the minimum wind speed value to include in the diagram.
#'   Default is 0 m/s.
#' @param ws_step (Optional) a single value indicating the spacing between each wind speed category.
#'   Default is a step of 2 m/s.
#' @param wd_nbins (Optional) a single value indicating the number of wind direction bars to plot.
#'   Must be either 16 (N, NNE, NE, ENE, ...), 8 (N, NE, E, SE, ...), or 4 (N, E, S, W).
#'   Default is 16.
#' @param fills (Optional) a vector of colours to use for the wind speed breaks
#'   Default uses "good looking" colours
#' @param alpha (Optional) a single value from 0-1 indicating the alpha (opacity) of the wind speed fill colours.
#'   Default is a step of 2 m/s.
#' @description
#' TODO: Add description
#' @return
#' A ggplot object of your wind rose.
#' @family Data Visualisation
#'
#' @export
#' @examples
#' \dontrun{
#' # Make test data
#' date_range = lubridate::ymd_h(c("2019-02-01 00", "2019-02-28 23"), tz = "America/Vancouver")
#' obs = get_station_data("Vanderhoof, BC, Canada", date_range, sources = "BCgov")$data |>
#'   dplyr::select("date_local", "site_id", "ws_1hr_ms", "wd_1hr_degrees") |>
#'   dplyr::distinct()
#' # Basic usage
#' gg = wind_rose(obs, facet_by = c(Site = "site_id"))
#' # Change titles
#' gg + ggplot2::labs(
#'   fill = "Legend Title", title = "Plot Title", 
#'   subtitle = "Plot Subtitle", caption = "Plot Caption")
#' 
#' # Save plot
#' # save_figure(gg, "./test.png")
#' }
wind_rose = function(
    obs, 
    data_cols = c(ws = "ws_1hr_ms", wd = "wd_1hr_degrees"), 
    facet_by = NULL, 
    facet_rows = 1,
    wd_nbins = c(16, 8, 4)[1],
    ws_min = 0,
    ws_step = 2,
    fills = "default",
    alpha = 0.8) {
  # Handle inputs
  if(is.null(names(data_cols))) names(data_cols) = c("ws", "wd")
  if(is.null(names(facet_by))) names(facet_by) = facet_by
  wd_step = dplyr::case_when(
    wd_nbins == 16 ~ 22.5,
    wd_nbins == 8  ~ 45,
    wd_nbins == 4  ~ 90)
  if(is.na(wd_step)){
    stop("wd_step must be either 16, 8, or 4")
  }
  rose_data = dplyr::rename(obs, 
    dplyr::all_of(c(data_cols, facet_by))) 
  facet_by = names(facet_by)

  # Get counts/percents for each ws bin at eac hcardinal direction
  rose_data = rose_data |>
    dplyr::filter(.data$ws >= ws_min) |>
    dplyr::mutate(
      ws_bin = cut_wind_speed(.data$ws, ws_min = ws_min, ws_step = ws_step),
      wd_bin = get_cardinal_direction(.data$wd, wd_step = wd_step)) |>
    # Get counts for each ws/wd bin in each rose
    dplyr::group_by(
      .data$ws_bin, .data$wd_bin, 
      dplyr::across(dplyr::all_of(facet_by))) |> 
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    # Calculate % of obs within each rose
    dplyr::group_by(dplyr::across(dplyr::all_of(facet_by))) |>
    dplyr::mutate(p = .data$n / sum(.data$n)) |>
    # Infill any missing wd bins so plot spacing is correct
    tidyr::complete(.data$wd_bin, .data$ws_bin, fill = list(n = 0, p = 0))

  # Determine most frequent direction across all facets
  most_frequent = rose_data |>
    dplyr::group_by(.data$wd_bin) |>
    dplyr::summarise(p = sum(.data$p, na.rm = TRUE)) |>
    dplyr::filter(.data$p == max(.data$p, na.rm = TRUE))

  # Make base plot
  gg = ggplot2::ggplot(rose_data) |>
    facet_plot(by = facet_by, rows = facet_rows) |>
    add_default_theme() +
    # Radial plot with internal y axis labels and a black border
    ggplot2::coord_radial(r.axis.inside = TRUE,
       start = -convert_units(wd_step / 2, "degrees", "radians")) +
    ggplot2::geom_hline(yintercept = most_frequent$p[1] * 1.05) + 
    # Format axis labels and extents
    ggplot2::scale_x_discrete(
      expand = ggplot2::expansion(0),
      labels = \(x) ifelse(nchar(x) < 3, x, "")) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(c(0, 0.02)),
      labels = \(x) ifelse(x > 0, round(x * 100) |> paste0("%"), ""))  +
    # Reverse legend colour order (to match reversed bar and colour order)
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    # Add/remove titles
    ggplot2::labs(
      x = ggplot2::element_blank(),
      y = ggplot2::element_blank(),
      fill = "Wind Speed (m/s)")
  
  # Add fill colours
  if(fills[1] != "default") {
    gg = gg + ggplot2::scale_fill_manual(values = rev(fills)) 
  }else gg = gg + ggplot2::scale_fill_viridis_d(direction = -1)
  
  # Add data
  gg + ggplot2::geom_col(
    ggplot2::aes(
      x = .data$wd_bin,
      y = .data$p, 
      fill = forcats::fct_rev(.data$ws_bin)), 
    colour = colour, alpha = alpha, width = bar_width, ...)
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