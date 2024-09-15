# TODO: add ability to normalize data for multiple obs sites

#' Create a Taylor diagram to assess model performance using the relationship between correlation, standard deviation, and centered RMS error.
#'
#' @param dat Paired observation and model data with (at least) all columns in `data_cols` and `groups`. 
#' @param data_cols (Optional) a character vector with 2 values indication column names in `dat` to get observed and modelled values. Must have names `"obs","mod"`.
#' @param groups a character vector with between 1 and 3 column names to use as groups. The first value will be used for `colour`, the second (if present) will be used for `shape`, and the third (if present) will be used for `fill` when adding model data points.
#' @param left_cor_limit (Optional) a single numeric value indicating the minimum correlation value to display (from -1 to +1). If not provided, the nearest 0.1 below the minimum correlation in the data will be used.
#' @param right_sd_limit (Optional) a single numeric value indicating the maximum standard deviation value to display (>=0). If not provided, the nearest "pretty" above the maximum standard deviation in the data will be used.
#' @param mod_colours,mod_shapes,mod_fills (Optional) a named vector of colours/shapes to use for the provided `groups` where the names correspond to values in that group column to assign each colour/shape to (i.e `c("group_1" = "red", ...)`)
#' @param mod_size,mod_stroke (Optional) single numeric value indicating the size/stroke of the model data points
#' @param obs_colour,obs_shape,obs_size,obs_stroke (Optional) a single value indicating the colour/shape/size/stroke of the observed data point
#' @param obs_label (Optional) a single character value indicating the text to displat for the observed point.
#' @param obs_label_vjust,obs_label_hjust (Optional) a single numeric value indicating how to position the observed label relative to the observed point.
#' @param cor_colour,cor_linetype (Optional) a single value indicating the colour/linetype of the correlation grid lines.
#' @param cor_label_step (Optional) a single value indicating the spacing between each corrlation line.
#' @param rmse_colour,rmse_linetype (Optional) a single value indicating the colour/linetype of the rmse circles originating from the observed point.
#' @param rmse_label_pos (Optional) a single value (0-360)indicating the location of the labels for the rmse circles.
#' @param sd_colour (Optional) a single value indicating the colour of the standard deviation arcs.
#' @param sd_linetypes (Optional) a character vector with 3 line types and names `"obs", "max", "other"` indicating the line types of standard deviation arcs.
#' @param plot_padding,labels_padding (Optional) a single numeric value indicating how much spacing (standard deviation units) to add to most text labels.
#' @description
#' Blah Blah Blah Taylor (2001) Blah Blah Blah 
#'
#' @return
#' A ggplot object of your taylor diagram.
#' @family Data Visualisation
#' @family Model Validation
#'
#' @export
#' @examples
#' \dontrun{
#' # Make test data
#' data = as.data.frame(datasets::ChickWeight) |>
#'   dplyr::filter(.data$Chick == 1) |>
#'   tidyr::pivot_wider(names_from = "Chick", values_from = "weight") |>
#'   dplyr::full_join(
#'     as.data.frame(datasets::ChickWeight) |>
#'       dplyr::filter(.data$Chick != 1)
#'   ) |> 
#'   dplyr::rename(obs = `1`, mod = "weight") |>
#'   dplyr::mutate(Chick = factor(round(as.numeric(.data$Chick) / 20)))
#' # Basic usage
#' taylor_diagram(data, groups = c(Diet = "Diet", Chick = "Chick"))
#' # Force 0 on left axis
#' taylor_diagram(data, groups = c(Group = "group"), 
#'   left_cor_limit = 0, rmse_label_pos = 130)
#' # Change colours / shapes
#' taylor_diagram(data, groups = c(Group = "group"), 
#'   mod_colours = c("AB" = "pink", "BC" = "blue"), 
#'   mod_fills = c("EGG" = "white", "PA" = "darkgrey"),
#'   mod_shapes = c("FALSE" = 23, "TRUE" = 22),
#'   mod_size = 4, mod_stroke = 6,
#'   obs_colour = "brown", obs_shape = 23, obs_size = 6, 
#'   cor_colour = "orange", cor_linetype = "dotdash",
#'   rmse_colour = "green", rmse_linetype = "longdash",
#'   sd_colour = "purple", sd_linetypes = c(obs = "solid", max = "dotted", other = "dashed")
#'   )
#' # Adjust text positioning
#' taylor_diagram(data, groups = c(Group = "group"),
#'   plot_padding = 4, labels_padding = 1, rmse_label_pos = 80)
#' 
#' # Save plot
#' # ggplot2::ggsave("test.png", width = 7, height = 5, units = "in")
#' }
taylor_diagram = function(dat, 
    data_cols = c(obs = "obs", mod = "mod"), 
    groups,
    left_cor_limit = NULL, 
    right_sd_limit = NULL,
    mod_colours = "default", 
    mod_fills = "default", 
    mod_shapes = "default", 
    mod_size = 3, 
    mod_stroke = 2, 
    obs_colour = "purple", 
    obs_shape = 16, 
    obs_size = 3, 
    obs_stroke = 2, 
    obs_label = "Observed", 
    obs_label_vjust = -1, 
    obs_label_hjust = -0.05,
    cor_colour = "lightgrey", 
    cor_linetype = "solid",
    cor_label_step = 0.1,
    rmse_colour = "brown", 
    rmse_linetype = "dotted", 
    rmse_label_pos = 80,
    sd_colour = "black", 
    sd_linetypes = c(obs = "dashed", max = "solid", other = "dotted"),
    plot_padding = 2, 
    labels_padding = 2){

  # Get observed standard deviation and correlation with obs by group(s)
  modelled = dat |>
    dplyr::rename(dplyr::all_of(data_cols)) |> 
    dplyr::group_by(dplyr::across(dplyr::all_of(unname(groups)))) |>
    dplyr::summarise(.groups = "drop",
      sd = sd(.data$mod, na.rm = TRUE),
      cor = cor(.data$obs, .data$mod, use = "pairwise.complete.obs"))
  # Get observed standard deviation
  observed = dat |> dplyr::summarise(sd = sd(.data$obs, na.rm = TRUE))

  # Make Taylor Diagram
  taylor = make_taylor_diagram_template(
      observed, modelled, 
      left_cor_limit = left_cor_limit,
      right_sd_limit = right_sd_limit,
      cor_colour = cor_colour,
      cor_linetype = cor_linetype,
      cor_label_step = cor_label_step,
      rmse_colour = rmse_colour,
      rmse_linetype = rmse_linetype,
      rmse_label_pos = rmse_label_pos,
      sd_colour = sd_colour,
      sd_linetypes = sd_linetypes,
      padding_limits = plot_padding, 
      nudge_labels = labels_padding) |>
    add_taylor_observed_point(
      observed, 
      colour = obs_colour,
      shape = obs_shape, 
      size = obs_size, 
      stroke = obs_stroke, 
      label = obs_label,
      label_vjust = obs_label_vjust, 
      label_hjust = obs_label_hjust) |>
    add_taylor_modelled_points(
      modelled, 
      groups = groups, 
      stroke = mod_stroke, 
      size = mod_size,
      colours = mod_colours,  
      fills = mod_fills, 
      shapes = mod_shapes)
  while(length(groups) < 3) groups = c(groups, "")
  if(!is.null(names(groups))) 
    taylor = taylor +
      ggplot2::labs(
        fill = names(groups)[3], 
        shape = names(groups)[2], 
        colour = names(groups)[1])
  return(taylor)
}

# Real test data
# data = data.table::fread(
#   "https://aqmap.ca/aqmap/data/plotting/purpleair/sensor_31411_recent_hourly.csv",
#   data.table = FALSE) |>
#   dplyr::select(obs = pm25_80402_0.91km, mod = pm25) |>
#   dplyr::mutate(sensor_id = "31411", sensor_type = "PA", province = "AB", colocated = TRUE) |>
#   dplyr::bind_rows(
#     data.table::fread(
#       "https://aqmap.ca/aqmap/data/plotting/purpleair/sensor_201873_recent_hourly.csv",
#       data.table = FALSE) |>
#       dplyr::select(obs = pm25_100202_0.02km, mod = pm25) |>
#       dplyr::mutate(sensor_id = "201873", sensor_type = "PA", province = "BC", colocated = TRUE) 
#   ) |>
#     dplyr::bind_rows(
#       data.table::fread(
#         "https://aqmap.ca/aqmap/data/plotting/purpleair/sensor_188701_recent_hourly.csv",
#         data.table = FALSE) |>
#         dplyr::select(obs = pm25_21100004_107.8km, mod = pm25) |>
#         dplyr::mutate(sensor_id = "103974", sensor_type = "PA", province = "BC", colocated = FALSE) 
#     ) |>
#   dplyr::bind_rows(
#     data.table::fread(
#       "https://aqmap.ca/aqmap/data/plotting/aqegg/sensor_egg0004a30b00027b24_recent_hourly.csv",
#       data.table = FALSE) |>
#       dplyr::select(obs = pm25_102701_0.43km, mod = pm25) |>
#       dplyr::mutate(sensor_id = "egg0004a30b00027b24", sensor_type = "EGG", province = "BC", colocated = FALSE) 
#   )

# Add raial correlation lines (and labels) to Taylor Diagrams
add_taylor_cor_lines = function(taylor, 
    min_cor = 0, sd_max, label_step = 0.1, nudge_labels = 2, 
    colour = "lightgrey", linetype = "solid") {
  
  mean_cor = mean(c(min_cor, 1))
  draw_at = seq(min_cor, 1, label_step)

  dont_label = c(if (min_cor %in% c(0, -1)) 1, length(draw_at))
  # TODO: allow for percents
  label_at = round(draw_at[-dont_label], 1)

  taylor + 
    # Correlation lines
    ggplot2::annotate(
      geom = "segment", 
      x = 0, y = 0,
      xend = convert_x(sd_max, convert_cor(draw_at)),
      yend = convert_y(sd_max, convert_cor(draw_at)),
      linetype = linetype, 
      colour = colour) +
    # Labels for each correlation line
    ggplot2::annotate(
      geom = "text",
      x = convert_x(sd_max + nudge_labels * 0.6, convert_cor(label_at)),
      y = convert_y(sd_max + nudge_labels * 0.6, convert_cor(label_at)),
      label = label_at) + 
    # Correlation axis label
    ggplot2::annotate(
      geom = "text",
      x = convert_x(sd_max + nudge_labels * 1.5, convert_cor(mean_cor)),
      y = convert_y(sd_max + nudge_labels * 1.5, convert_cor(mean_cor)),
      label = "Correlation", 
      angle = mean_cor * -90) 
}

# Add standard deviation arcs to Taylor Diagrams
add_taylor_sd_lines = function(
    taylor, min_cor, sd_obs, lines_at, colour = "black", 
    linetypes = c(obs = "dashed", max = "solid", other = "dotted")) {
  arc_data = data.frame(
    start =  -0.5 * pi * -min_cor, end = .5 * pi, r = lines_at)
  arc_data$linetype = ifelse(lines_at == max(lines_at), 
    "max", ifelse(lines_at == sd_obs, "obs", "other"))

  taylor +
    ggforce::geom_arc(
      data = arc_data, colour = colour,
      ggplot2::aes(x0 = 0, y0 = 0, 
        r = r, start = start, end = end, linetype = linetype)) +
    ggplot2::scale_linetype_manual(
      values = linetypes, guide = "none") 
}

# Add SD axes lines to Taylor Diagrams
add_taylor_axes_lines = function(taylor, min_cor, sd_max) {
  taylor +
    ggplot2::annotate(
      geom = "segment", x = 0, y = 0,
      xend = convert_x(sd_max, convert_cor(c(min_cor, 1))),
      yend = convert_y(sd_max, convert_cor(c(min_cor, 1))))
}

add_taylor_rmse_lines = function(taylor, sd_obs, sd_max, min_cor, y_max, 
    label_pos = 80,
    n = 5, colour = "brown", linetype = "dotted", axis_label = "centered\nRMS error", 
    nudge_labels, padding_limits) {
  rms_lines = make_taylor_rmse_lines(sd_obs, sd_max, min_cor, label_pos = label_pos, n)
  taylor +
    # Draw semicircles originating at the observed point for centered RMS error
    ggplot2::geom_line(
      data = rms_lines$lines |> dplyr::filter(
        get_dist_from_origin(.data$x, .data$y) < sd_max,
        get_correlation(.data$x, .data$y) >= min_cor
      ),
      ggplot2::aes(x, y, group = rmse_values),
      linetype = linetype, colour = colour) +
    # Line labels
    ggplot2::geom_text(
      data = rms_lines$labels |> dplyr::filter(
        get_dist_from_origin(.data$x, .data$y) < sd_max-padding_limits,
        get_correlation(.data$x, .data$y) >= min_cor),
      ggplot2::aes(x, y, label = label), 
      vjust = 0, colour = colour,
      nudge_y = nudge_labels * -0.75) +
    # Legend text
    ggplot2::annotate(
      geom = "text", x = sd_max, y = y_max, 
      label = axis_label, colour = colour,
      hjust = 1, vjust = 1.4 # TODO: better locating control
    )
}

make_taylor_rmse_lines = function(sd_obs, sd_max, min_cor, label_pos = 80, n = 5){
  max_rmse = ifelse(min_cor < 0, sd_max + sd_max * -min_cor, sd_max)
  rmse_values = pretty(seq(0, max_rmse, length.out = n + 1)[-1]) # TODO: allow for manual specification
  labelpos = seq(45, 70, length.out = length(rmse_values)) + label_pos # TODO: better label pos control

  rmse_lines = lapply(1:length(rmse_values), \(i) {
    if(rmse_values[i] == 0) return(NULL)
    # Get x coordinates of a half-circle transposed to x=sd_obs for each rmse_value
    xcurve = cos(seq(0, pi, by = 0.01)) * rmse_values[i] + sd_obs
    # End the circle once it reaches the end of our x-axis
    endcurve = which(xcurve < ifelse(min_cor < 0, -sd_max, 0))
    endcurve = ifelse(length(endcurve), min(endcurve) - 1, length(xcurve))
    # Get y coordinates of a circle for each rmse_value
    ycurve = sin(seq(0, pi, by = 0.01)) * rmse_values[i]
    # a2 + b2 = c2
    maxcurve = xcurve^2 + ycurve^2
    startcurve = which(maxcurve > sd_max^2)
    startcurve = ifelse(length(startcurve), max(startcurve) + 1, 0)
    
    list(
      lines = data.frame(
        x = xcurve[startcurve:endcurve],
        y = ycurve[startcurve:endcurve],
        rmse_values = as.factor(rmse_values[i])), 
      labels = data.frame(
        x = xcurve[labelpos[i]],
        y = ycurve[labelpos[i]],
        label = as.character(rmse_values[i]),
        rmse_values = as.factor(rmse_values[i]))
    )
  })
  list(
    lines = rmse_lines |> lapply_and_bind(\(x) x$lines),
    labels = rmse_lines |> lapply_and_bind(\(x) x$labels)
  )
}

make_taylor_diagram_template = function(
    observed, modelled, 
    left_cor_limit = NULL, 
    right_sd_limit = NULL,
    cor_colour = "lightgrey", 
    cor_linetype = "solid",  
    cor_label_step = 0.1,
    rmse_colour = "brown", 
    rmse_linetype = "dotted", 
    rmse_label_pos = 80, 
    sd_colour = "black", 
    sd_linetypes = c(obs = "dashed", max = "solid", other = "dotted"),
    padding_limits = 2, 
    nudge_labels = 2){
  
  sd_max = max(c(observed$sd, modelled$sd))
  sd_max = ceiling(sd_max / 10) * 10
  if(!is.null(right_sd_limit)) sd_max = right_sd_limit
      
  # TODO: allow manual specification and better control
  sd_lines_at =  pretty(seq(0, sd_max, length.out = 4)) |>
    c(observed$sd, sd_max) |>
    unique()

  min_cor = min(modelled$cor, na.rm = TRUE)
  min_cor = floor(min_cor * 10) / 10
  if(min_cor > 0.5) min_cor = 0.5
  if(!is.null(left_cor_limit)) min_cor = left_cor_limit

  y_max = ifelse(min_cor < 0, 
    sd_max + padding_limits, 
    convert_y(sd_max + padding_limits, convert_cor(min_cor)))
  
  xlims = ifelse(min_cor < 0, 
      -sd_max - padding_limits, 0) |> 
    c(sd_max + padding_limits)
  
  ggplot2::ggplot() |>
    add_taylor_cor_lines(
      min_cor = min_cor, 
      label_step = cor_label_step,
      sd_max = sd_max, 
      colour = cor_colour, 
      linetype = cor_linetype, 
      nudge_labels = nudge_labels) |> 
    add_taylor_sd_lines(
      min_cor = min_cor, 
      sd_obs = observed$sd,
      lines_at = sd_lines_at, 
      colour = sd_colour, 
      linetypes = sd_linetypes) |>
    add_taylor_axes_lines(
      min_cor = min_cor, 
      sd_max = sd_max) |>
    add_taylor_rmse_lines(
      sd_obs = observed$sd, 
      sd_max = sd_max,
      label_pos = rmse_label_pos,
      min_cor = min_cor, 
      y_max = y_max - padding_limits, 
      colour = rmse_colour, 
      linetype = rmse_linetype,
      nudge_labels = nudge_labels, 
      padding_limits = padding_limits)  +
    # Presentation
    ggplot2::coord_equal(
      xlim = xlims,
      ylim = c(0, y_max + padding_limits),
      expand = FALSE, 
      clip = "on") +
    ggplot2::scale_x_continuous(
      labels = \(l) ifelse(l < 0, "", l)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.line.y  = ggplot2::element_blank(),
      axis.text.y  = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_rect(colour = NA, fill = NA),
      panel.grid   = ggplot2::element_blank())  +
    ggplot2::labs(x = "Standard Deviation")
}

add_taylor_observed_point = function(taylor, observed, shape = 16, size = 3, stroke = 2, colour = "purple", label = "Observed", label_vjust = -1, label_hjust = -0.05){
  taylor + 
    ggplot2::geom_point(
      data = observed, 
      ggplot2::aes(x = sd, y = 0),
      shape = shape, 
      stroke = stroke,
      colour = colour, 
      size = size) +
    ggplot2::geom_text(
      data = observed,
      ggplot2::aes(x = sd), 
      y = 0, 
      label = label, 
      colour = colour, 
      size = size,
      vjust = label_vjust, 
      hjust = label_hjust
    )
}

add_taylor_modelled_points = function(taylor, modelled, groups, size = 3, stroke = 2, shapes = "default", colours = "default", fills = "default") {
  modelled = modelled |>
    dplyr::mutate(
      x = convert_x(sd, convert_cor(cor)),
      y = convert_y(sd, convert_cor(cor)))
  
  if(length(groups) == 3) {
    taylor = taylor  + 
      ggplot2::geom_point(
        data = modelled, 
        size = size, 
        stroke = stroke,
        ggplot2::aes(
          x = x, y = y,
          colour = .data[[groups[1]]], 
          shape = .data[[groups[2]]], 
          fill = .data[[groups[3]]])) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          override.aes = list(shape = 21)))   
    if(shapes[1] != "default") {
      taylor = taylor +
        ggplot2::scale_shape_manual(values = shapes)
    }else taylor = taylor +
      ggplot2::scale_shape_manual(values = 21:30)
    if(fills[1] != "default") {
      taylor = taylor +
        ggplot2::scale_fill_manual(values = fills) 
    }else taylor = taylor +
      ggplot2::scale_fill_viridis_d()
    }else if(length(groups) == 2) {
    taylor = taylor  + 
      ggplot2::geom_point(
        data = modelled, 
        size = size, 
        stroke = stroke, 
        ggplot2::aes(
          x = x, y = y,
          colour = .data[[groups[1]]], 
          shape = .data[[groups[2]]]))
    if(shapes[1] != "default") taylor = taylor +
      ggplot2::scale_shape_manual(values = shapes) 
  }else if(length(groups) == 1) {
    taylor = taylor  + 
      ggplot2::geom_point(
        data = modelled,
         size = size,
          stroke = stroke, 
        shape = if(shapes[1] == "default") 21 else shapes[1],
        ggplot2::aes(x = x, y = y, colour = .data[[groups[1]]]))
  }else {
    stop(paste("groups must have a length between 1 and 3, not", length(groups)))
  }

  if(colours[1] != "default") {
    taylor = taylor +
      ggplot2::scale_colour_manual(values = colours) 
  }else taylor = taylor +
    ggplot2::scale_colour_brewer(palette = "Dark2") 

  return(taylor)
}

convert_x = function(dist_from_origin, theta, alpha = pi / 6)
  dist_from_origin * cos(alpha * theta)
convert_y = function(dist_from_origin, theta, alpha = pi / 6)
  dist_from_origin * sin(alpha * theta)
convert_cor = function(correlation)
  3 - correlation * 3
get_dist_from_origin = function(x, y)
  sqrt(x^2 + y^2)
get_correlation = function(x, y)
  atan2(y, x) / pi * -2 + 1