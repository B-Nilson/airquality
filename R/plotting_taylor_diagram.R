# TODO: add ability to normalize data for multiple obs sites
# TODO: test patchworking
# TODO: add ggrepel labels if desired
# TODO: handle sd_maximum < sd_observed
# TODO: add sd_units argument
# TODO: add y axis (with same labels as x) if min_cor == 0
# TODO: place observed label on other side of axis
# TODO: add description documentation
# TODO: dark mode option
# TODO: "solar diagram" variant? (see https://doi.org/10.1016%2Fj.geoderma.2021.115332)

#' Create a Taylor diagram to assess model performance using the relationship between correlation, standard deviation, and centered RMS error.
#'
#' @param dat Paired observation and model data with (at least) all columns in `data_cols`, `group_by`, and (if provided) `facet_by`. 
#' @param data_cols (Optional) a character vector with 2 values indication column names in `dat` to get observed and modelled values.
#'   Default assumes columns "obs" and "mod" exist.
#' @param group_by a character vector with between 1 and 3 column names to use as groups. 
#'   The first value will be used for `colour`, the second (if present) will be used for `shape`, and the third (if present) will be used for `fill` when adding model data points.
#'   If names are present they will be used as the corresponding legend titles.
#' @param facet_by (Optional) a character vector with 1 or 2 column names to use as facets in `ggplot2::facet_wrap()`.
#'   If names are present they will be used as the corresponding facet titles.
#'   Default (NULL) does not facet the plot.
#' @param facet_rows (Optional) a single numeric value indicating the number of rows to use in facetting if `facet_by` values provided. 
#'   Default is a single row.
#' @param obs_colour,obs_shape,obs_size,obs_stroke (Optional) a single value indicating the colour/shape/size/stroke of the observed data point.
#'   Default is a full-colour purple circle.
#' @param obs_label (Optional) a single character value indicating the text to display for the observed point.
#'   Default is "Obs."
#' @param mod_colours,mod_shapes,mod_fills (Optional) a named vector of colours/shapes to use for the provided `group_by`
#'   where the names correspond to values in that group column to assign each colour/shape to (i.e `c("group_1" = "red", ...)`).
#'   Default uses "good looking" colours/shapes/fills.
#' @param mod_size,mod_stroke (Optional) a single numeric value indicating the size/stroke of the model data points.
#'   Default matches the size/stroke of the observed point.
#' @param cor_minimum (Optional) a single numeric value indicating the minimum correlation value to display (from -1 to +1). 
#'   Default uses the nearest 0.1 below the minimum correlation.
#' @param cor_step (Optional) a single value indicating the spacing between each correlation line.
#'   Default is a step of 0.1 (10\%).
#' @param cor_colour,cor_linetype (Optional) a single value indicating the colour/linetype of the correlation grid lines.
#'   Default is grey long-dash lines.
#' @param cor_label (Optional) a single character value indicating the text to display for the correlation axis title.
#'   Default is "Correlation".
#' @param rmse_minimum (Optional) a single numeric value indicating the minimum rmse line to display (>= 0).
#'   Default is 0 (meaning the first line to display is at `rmse_step`).
#' @param rmse_step (Optional) a single value indicating the spacing between each rmse line.
#'   Default produces approximatley 4 lines with "pretty" spacing.
#' @param rmse_colour,rmse_linetype (Optional) a single value indicating the colour/linetype of the rmse circles originating from the observed point.
#'   Default is brown dotted lines.
#' @param rmse_label (Optional) a single character value indicating the text to display for the RMSE axis title.
#'   Default is "Centered RMS Error".
#' @param rmse_label_pos (Optional) a single value (0-1) indicating the location of the labels for the rmse circles 
#'   (0 == far left along x-axis, 0.5 = top of cirles, 1 = far right along x-axis).
#'   Default is 10\% greater than the minimum correlation.
#' @param sd_maximum (Optional) a single numeric value indicating the maximum standard deviation value to display (>= 0). 
#'   Default is the nearest "pretty" value above the maximum standard deviation.
#' @param sd_step (Optional) a single value indicating the spacing between each standard deviation line.
#'   Default produces approximatley 4 lines with "pretty" spacing.
#' @param sd_colour (Optional) a single value indicating the colour of the standard deviation arcs.
#'   Default is black.
#' @param sd_linetypes (Optional) a character vector with 2 line types and names `"obs", "other"` indicating the line types of standard deviation arcs.
#'   Default is dashed for the observed line, dotted for others.
#' @param sd_label (Optional) a single character value indicating the text to display for the standard deviation axis title.
#'   Default is "Standard Deviation".
#' @param plot_padding,labels_padding (Optional) a single numeric value indicating how much spacing (standard deviation units) to add to most text labels.
#'   Default is 2 for both, likely needs to be adjusted depeding on the figure size and number of facets.
#' @description
#' Blah Blah Blah Taylor (2001) Blah Blah Blah 
#' TODO: Add description
#' @return
#' A ggplot object of your taylor diagram.
#' @family Data Visualisation
#' @family Model Validation
#'
#' @export
#' @examples
#' \dontrun{
#' # Make test data
#' data = as.data.frame(datasets::ChickWeight) |> # TODO: make better test dataset
#'   dplyr::filter(.data$Chick == 1) |>
#'   tidyr::pivot_wider(names_from = "Chick", values_from = "weight") |>
#'   dplyr::full_join(
#'     as.data.frame(datasets::ChickWeight) |>
#'       dplyr::filter(.data$Chick != 1)
#'   ) |> 
#'   dplyr::rename(obs = `1`, mod = "weight") |>
#'   dplyr::mutate(Chick = factor(round(as.numeric(.data$Chick) / 20)))
#' # Basic usage
#' taylor_diagram(data, group_by = c(Diet = "Diet", Chick = "Chick"))
#' # Force 0 on left axis
#' taylor_diagram(data, group_by = c(Diet = "Diet", Chick = "Chick"), 
#'   cor_minimum = 0, rmse_label_pos = 130) # TODO: fix this
#' # Change colours / shapes
#' taylor_diagram(data, group_by = c(Diet = "Diet", Chick = "Chick"), 
#'   mod_colours = c("AB" = "pink", "BC" = "blue"), 
#'   mod_fills = c("EGG" = "white", "PA" = "darkgrey"), # TODO: update this
#'   mod_shapes = c("FALSE" = 23, "TRUE" = 22),
#'   mod_size = 4, mod_stroke = 6,
#'   obs_colour = "brown", obs_shape = 23, obs_size = 6, 
#'   cor_colour = "orange", cor_linetype = "dotdash",
#'   rmse_colour = "green", rmse_linetype = "longdash",
#'   sd_colour = "purple", sd_linetypes = c(obs = "solid", other = "dashed")
#'   )
#' # Adjust text positioning
#' taylor_diagram(data, group_by = c(Diet = "Diet", Chick = "Chick"),
#'   plot_padding = 4, labels_padding = 1, rmse_label_pos = 0.7)
#' 
#' # Save plot
#' # gg = taylor_diagram(data, group_by = c(Diet = "Diet", Chick = "Chick"))
#' # save_figure(gg, "./test.png")
#' }
taylor_diagram = function(dat, 
    data_cols = c(obs = "obs", mod = "mod"), 
    group_by,
    facet_by = NULL, facet_rows = 1,
    obs_colour = "purple", obs_shape = 16, obs_size = 1.5, obs_stroke = 1, 
    obs_label = "Obs.", 
    mod_colours = "default", mod_fills = "default", mod_shapes = "default", 
    mod_size = 1.5, mod_stroke = 1, 
    cor_minimum = NULL, cor_step = 0.1,
    cor_colour = "grey30", cor_linetype = "longdash",
    cor_label = "Correlation",
    rmse_minimum = 0, rmse_step = 'default', 
    rmse_colour = "brown", rmse_linetype = "dotted", 
    rmse_label = "Centered RMS Error", rmse_label_pos = "default",
    sd_maximum = NULL, sd_step = 'default',
    sd_colour = "black", 
    sd_linetypes = c(obs = "dashed", other = "dashed"),
    sd_label = "Standard Deviation",
    plot_padding = 0.5, labels_padding = 2){
  
  # Handle inputs
  if(is.null(names(data_cols))) names(data_cols) = c("obs", "mod")
  if(length(data_cols) != 2) {
    stop(paste("argument `data_cols` must have a length of two, not", length(data_cols)))
  }
  if(!all(data_cols %in% names(dat))) {
    stop(paste("All column names in argument `data_cols` must be found in `dat`, these ones are missing:", 
      paste(data_cols[!data_cols %in% names(dat)], collapse = ", ")))
  }
  if(!all(group_by %in% names(dat))) {
    stop(paste("All column names in argument `group_by` must be found in `dat`, these ones are missing:", 
      paste(group_by[!group_by %in% names(dat)], collapse = ", ")))
  }
  if(!is.null(facet_by)) if(!all(facet_by %in% names(dat))) {
    stop(paste("All column names in argument `facet_by` must be found in `dat`, these ones are missing:", 
      paste(facet_by[!facet_by %in% names(dat)], collapse = ", ")))
  }
  if(!is.null(cor_minimum)) if(cor_minimum < -1 | cor_minimum > 1) {
    stop(paste("argument `cor_minimum` must be between -1 and 1, not", cor_minimum))
  }
  if(rmse_minimum < 0) {
    stop(paste("argument `rmse_minimum` must be greater than or equal to 0, not", rmse_minimum))
  }
  if(!is.null(sd_maximum)) if(sd_maximum <= 0) {
    stop(paste("argument `sd_maximum` must be greater than 0, not", sd_maximum))
  }

  # Cleanup input data
  dat = dplyr::ungroup(dat) |>
    dplyr::rename(dplyr::all_of(data_cols)) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(unname(group_by)), as.factor))
  if(!is.null(facet_by)) {
    dat = dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(facet_by)))
    if(!is.null(names(facet_by))) facet_by = names(facet_by) 
  }
  
  # Get modelled standard deviation and correlation with obs by group(s)
  modelled = dat|> 
    dplyr::group_by(.add = TRUE, 
      dplyr::across(dplyr::all_of(unname(group_by)))) |>
    dplyr::summarise(.groups = "drop",
      sd  = stats::sd(.data$mod, na.rm = TRUE),
      cor = stats::cor(.data$obs, .data$mod, use = "pairwise.complete.obs"),
      x = get_x(.data$sd, .data$cor), 
      y = get_y(.data$sd, .data$cor))
  # Get observed standard deviation (by facet_by if provided)
  observed = dat |> dplyr::summarise(
    sd = stats::sd(.data$obs, na.rm = TRUE))

  # Make Taylor Diagram
  taylor = make_taylor_diagram_template(
      observed, modelled, 
      facet_by = facet_by,
      cor_minimum = cor_minimum,
      cor_step = cor_step,
      cor_colour = cor_colour,
      cor_linetype = cor_linetype,
      cor_label = cor_label,
      rmse_minimum = rmse_minimum,
      rmse_step = rmse_step,
      rmse_colour = rmse_colour,
      rmse_linetype = rmse_linetype,
      rmse_label = rmse_label,
      rmse_label_pos = rmse_label_pos,
      sd_maximum = sd_maximum,
      sd_step = sd_step,
      sd_colour = sd_colour,
      sd_linetypes = sd_linetypes,
      sd_label = sd_label,
      padding_limits = plot_padding, 
      nudge_labels = labels_padding) |>
    add_taylor_observed_point(
      observed, 
      colour = obs_colour,
      shape = obs_shape, 
      size = obs_size, 
      stroke = obs_stroke, 
      label = obs_label, 
      nudge_labels = labels_padding) |>
    add_taylor_modelled_points(
      modelled, 
      group_by = group_by, 
      stroke = mod_stroke, 
      size = mod_size,
      colours = mod_colours,  
      fills = mod_fills, 
      shapes = mod_shapes) |>
    facet_plot(by = facet_by, rows = facet_rows)
  
  while(length(group_by) < 3) group_by = c(group_by, "")
  if(!is.null(names(group_by))) 
    taylor = taylor +
      ggplot2::labs(
        fill = names(group_by)[3], 
        shape = names(group_by)[2], 
        colour = names(group_by)[1])
  return(taylor)
}

make_taylor_diagram_template = function(
    observed, modelled, 
    facet_by = NULL,
    cor_minimum = NULL, 
    cor_step = 0.1,
    cor_colour = "grey30", 
    cor_linetype = "solid", 
    cor_label = "Correlation",
    rmse_minimum = 0, 
    rmse_step = 'default', 
    rmse_colour = "brown", 
    rmse_linetype = "dotted", 
    rmse_label = "Centered RMS Error",
    rmse_label_pos = "default", 
    sd_maximum = NULL,
    sd_step = "default",
    sd_colour = "black", 
    sd_linetypes = c(obs = "dashed", other = "dashed"),
    sd_label = "Standard Deviation",
    padding_limits = 2, 
    nudge_labels = 2){

  sd_max = ceiling(max(c(observed$sd, modelled$sd)) / 5) * 5
  if(!is.null(sd_maximum)) sd_max = sd_maximum
      
  min_cor = floor(min(modelled$cor, na.rm = TRUE) * 10) / 10
  if(min_cor > 0.5) min_cor = 0.5
  if(!is.null(cor_minimum)) min_cor = cor_minimum

  y_max = ifelse(min_cor < 0, 
    sd_max, get_y(sd_max, min_cor)) + padding_limits

  xlims = c(
    ifelse(min_cor < 0, get_x(sd_max, min_cor), 0),
    sd_max)
  
  x_title_hjust = ifelse(min_cor >= 0 | min_cor == -1 |  !is.null(facet_by), 0.5, 1 - (xlims[2] / 2 / (xlims[2] - xlims[1]))) 

  if(rmse_label_pos == "default") 
    rmse_label_pos = (min_cor + 1) / 2 * 0.9
  
  blank = ggplot2::element_blank()
  taylor = ggplot2::ggplot() |>
    add_default_theme() |>
    add_taylor_cor_lines(
      observed = observed,
      min_cor = min_cor, 
      step = cor_step,
      sd_max = sd_max, 
      colour = cor_colour, 
      linetype = cor_linetype, 
      axis_label = cor_label,
      nudge_labels = nudge_labels) |> 
    add_taylor_sd_lines(
      min_cor = min_cor, 
      sd_max = sd_max,
      sd_step = sd_step,
      observed = observed,
      colour = sd_colour, 
      linetypes = sd_linetypes) |>
    add_taylor_rmse_lines(
      observed = observed, 
      sd_max = sd_max,
      min_cor = min_cor, 
      rmse_minimum = rmse_minimum, 
      rmse_step = rmse_step,
      label_pos = rmse_label_pos,
      y_max = y_max - padding_limits, 
      colour = rmse_colour, 
      linetype = rmse_linetype,
      nudge_labels = nudge_labels, 
      padding_limits = padding_limits) |>
    add_taylor_axes_lines(
      observed = observed, 
      min_cor = min_cor, 
      sd_max = sd_max)   +
    # Presentation
    ggplot2::coord_equal(clip = "off") +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(c(0, 0.03))) +
    ggplot2::theme(
      axis.line.y  = blank,
      axis.ticks.y = blank,
      axis.text.y  = blank,
      axis.title.y = blank,
      panel.grid   = blank,
      axis.title.x = ggtext::element_markdown(hjust = x_title_hjust),
      legend.box.spacing = ggplot2::unit("2", "pt"))  +
    ggplot2::labs(
      x = paste0(
        sd_label,"<br>",
        "<span style='color: ", rmse_colour, "; font-size: 8pt;'>",
        rmse_label, "</span>"))
  return(taylor)
}

# Add raial correlation lines (and labels) to Taylor Diagrams
add_taylor_cor_lines = function(
    taylor, observed,
    min_cor = 0, sd_max, 
    colour = "grey30", 
    linetype = "solid",
    axis_label = "Correlation",
    step = 0.1, 
    label_type = "decimal",
    nudge_labels = 2) {
  draw_at = seq(min_cor, 1, step)

  # Make locations for the correlation line end points and labels
  label_dist = sd_max + nudge_labels * 0.6
  cor_lines = lapply_and_bind(draw_at, \(at)
    observed |> dplyr::mutate(
      xend = get_x(sd_max, at),
      yend = get_y(sd_max, at),
      x_label = get_x(label_dist, at),
      y_label = get_y(label_dist, at),
      label = ifelse(label_type == "percent", # TODO: implement in taylor_diagram()
        paste(at * 100, "%"), at)
    ))
  # Make location for the label for the axis title
  dist_from_origin = label_dist + nudge_labels * 0.75
  mean_cor = mean(c(min_cor, 1))
  axis_title = observed |> dplyr::mutate(
    x = get_x(dist_from_origin, mean_cor),
    y = get_y(dist_from_origin, mean_cor))

  taylor + 
    # Correlation lines
    ggplot2::geom_segment(
      data = cor_lines, linewidth = 0.25,
      linetype = linetype, colour = colour,
      ggplot2::aes(x = 0, y = 0, 
        xend = .data$xend, yend = .data$yend)) +
    # Labels for each correlation line
    ggplot2::geom_text(
      data = cor_lines, size = 3, 
      ggplot2::aes(
        .data$x_label, .data$y_label, label = .data$label),
      colour = ggplot2::theme_get()$axis.text$colour) + 
    # Correlation axis label
    ggplot2::geom_text(
      data = axis_title,  size = 4,
      colour = "black", 
      ggplot2::aes(.data$x, .data$y),
      label = axis_label, 
      angle = mean_cor * -90) 
}

# Add standard deviation arcs to Taylor Diagrams
add_taylor_sd_lines = function(
    taylor, observed,
    min_cor, sd_max,
    sd_step = "default",
    colour = "black", 
    linetypes = c(obs = "dashed", other = "dashed")) {
  
  linetypes = c(linetypes, max = "solid")
  if (sd_step == "default") {
    lines_at = pretty(seq(0, sd_max, length.out = 4))
    lines_at = lines_at[lines_at < sd_max]
  }else lines_at =  seq(0, sd_max, sd_step)
  lines_at = unique(c(lines_at, sd_max))

  arc_data = lapply_and_bind(1:nrow(observed), \(i) {
    at = unique(c(lines_at, observed$sd[i]))
    data.frame(
        observed[i, ],
        start =  -0.5 * pi * -min_cor,
        end = .5 * pi, 
        r = at,
        linetype = ifelse(at == max(at), 
          "max", ifelse(at == observed$sd[i], "obs", "other")))})
  linewidths = c(0.5, 0.25, 0.5) |>
    stats::setNames(names(linetypes))
  
  taylor +
    ggforce::geom_arc(
      data = arc_data, 
      colour = colour,
      ggplot2::aes(
        x0 = 0, y0 = 0, r = .data$r, 
        start = .data$start, end = .data$end, 
        linewidth = .data$linetype,
        linetype = .data$linetype)) +
    ggplot2::scale_linetype_manual(
      values = linetypes, guide = "none")  +
    ggplot2::scale_linewidth_manual(
      values = linewidths, guide = "none") +
    # TODO: get facet pairs in order added, get obs sd for each pair, add to global var whenever labels checked, don't label if global index of obs sd within x% of label
    ggplot2::scale_x_continuous(
      breaks = if(min_cor > -1) lines_at else c(-lines_at, lines_at),
      labels = \(l) ifelse(l < 0 & min_cor > -1, "", abs(l))) 
}

# Add SD axes lines to Taylor Diagrams
# TODO: add customizability
add_taylor_axes_lines = function(taylor, observed, min_cor, sd_max) {
  axes_lines = lapply_and_bind(c(min_cor, 1), \(correlation)
    observed |>
      dplyr::mutate(
        xend = get_x(sd_max, correlation),
        yend = get_y(sd_max, correlation)))
  taylor +
    ggplot2::geom_segment(
      data = axes_lines,
      ggplot2::aes(xend = .data$xend, yend = .data$yend),
      x = 0, y = 0)
}

add_taylor_rmse_lines = function(
    taylor, observed, 
    sd_max, min_cor, y_max, 
    label_pos = 0.6,
    rmse_minimum = 0, rmse_step = "default", 
    colour = "brown", linetype = "dotted", 
    nudge_labels, padding_limits) {
  rms_lines = make_taylor_rmse_lines(
    observed = observed, sd_max = sd_max, min_cor = min_cor, 
    label_pos = ((1-label_pos) * 255 - 20), 
    rmse_minimum = rmse_minimum, 
    rmse_step = rmse_step,
    padding_limits = padding_limits)
  taylor +
    # Draw semicircles originating at the observed point for centered RMS error
    ggplot2::geom_line(
      data = rms_lines$lines,
      ggplot2::aes(.data$x, .data$y, group = .data$rmse_values),
      linetype = linetype, colour = colour) +
    # Line labels
    ggplot2::geom_text(
      data = rms_lines$labels, 
      size = 3, # TODO: make input
      ggplot2::aes(.data$x, .data$y, label = .data$label), 
      vjust = 1, hjust = ifelse(label_pos <= 0.4, 0, ifelse(label_pos >= 0.6, 1, 0)),
      colour = colour,
      nudge_y = nudge_labels * -0.1,
      nudge_x = nudge_labels * 
        ifelse(label_pos <= 0.4, 0.1, ifelse(label_pos >= 0.1, -0.1, 0))
      )
}

make_taylor_rmse_lines = function(
    observed, sd_max, min_cor, 
    label_pos = 80, 
    rmse_minimum = 0, 
    rmse_step = "default",
    padding_limits = 2){
  max_rmse = ifelse(min_cor < 0, sd_max + sd_max * -min_cor, sd_max)
  if(rmse_step == "default"){
    rmse_values = pretty(seq(rmse_minimum, max_rmse, length.out = 5))
    if(rmse_values[1] == 0) rmse_values = rmse_values + rmse_minimum
  }else rmse_values = seq(rmse_minimum, max_rmse, rmse_step)
  rmse_values = rmse_values[rmse_values != 0]

  labelpos = seq(45, 70, length.out = length(rmse_values)) + label_pos

  lines = lapply(1:nrow(observed), \(obs_i){
    rmse_lines = lapply(1:length(rmse_values), \(i) {
      if(rmse_values[i] == 0) return(NULL)
      # Get x coordinates of a half-circle transposed to x=sd_obs for each rmse_value
      xcurve = cos(seq(0, pi, by = 0.01)) * rmse_values[i] + observed$sd[obs_i]
      # Get y coordinates of a circle for each rmse_value
      ycurve = sin(seq(0, pi, by = 0.01)) * rmse_values[i]
      
      list(
        lines = data.frame(
            observed[obs_i, ],
            x = xcurve,
            y = ycurve,
            rmse_values = as.factor(rmse_values[i])) |> 
          dplyr::filter(
            get_standard_deviation(.data$x, .data$y) < sd_max,
            get_correlation(.data$x, .data$y) >= min_cor), 
        labels = data.frame(
            observed[obs_i, ],
            x = xcurve[labelpos[i]],
            y = ycurve[labelpos[i]],
            label = as.character(rmse_values[i]),
            rmse_values = as.factor(rmse_values[i])) |> 
          dplyr::filter(
            get_standard_deviation(.data$x, .data$y) < sd_max - padding_limits,
            get_correlation(.data$x, .data$y) >= min_cor,
            get_correlation(.data$x, .data$y) <= 1))
    })
  })
  list(
    lines  = lines |> lapply_and_bind(\(x) x |> lapply_and_bind(\(y) y$lines)),
    labels = lines |> lapply_and_bind(\(x) x |> lapply_and_bind(\(y) y$labels)))
}

add_taylor_observed_point = function(
    taylor, observed, 
    shape = 16, size = 1.5, stroke = 1, 
    colour = "purple", label = "Obs.", 
    nudge_labels = 2){
  taylor + 
    ggplot2::geom_point(
      data = observed, 
      ggplot2::aes(x = .data$sd, y = 0),
      shape = shape, 
      stroke = stroke,
      colour = colour, 
      size = size) +
    ggplot2::geom_text(
      data = observed,
      ggplot2::aes(x = .data$sd, y = 0),  
      label = label, 
      colour = colour, 
      size = 3, 
      vjust = 2, hjust = 0.5
    )
}

get_shape_pairs = function(shapes){
  pairs = list(
    filled = c(21:25),
    not_filled = c(1, 0, 5, 2, 6) # cirle, square, diamond, tri-up, tri-down
  )
  is_filled = shapes %in% pairs$filled
  is_not_filled = shapes %in% pairs$not_filled
  if(any(!is_filled & !is_not_filled)) {
    stop("Cannot find paired filled/not-filled shapes for shape(s)", 
      paste(collapse = ", ", shapes[!is_filled & !is_not_filled]))
  }
  data.frame(shapes) |>
    dplyr::mutate(
      filled = ifelse(is_filled, shapes, pairs$filled[match(shapes, pairs$not_filled)]),
      not_filled = ifelse(is_not_filled, shapes, pairs$not_filled[match(shapes, pairs$filled)])
    )
}

add_taylor_modelled_points = function(taylor, modelled, group_by, size = 1.5, stroke = 1, shapes = "default", colours = "default", fills = "default") {
  # TODO: instead, find matching shapes with fill/no fill and combine last two group_by into shape + fill/no fill
  if(length(group_by) == 3) {
    taylor = taylor  + 
      ggplot2::geom_point(
        data   = modelled, 
        size   = size, stroke = stroke,
        ggplot2::aes(
          x = get_x(.data$sd, .data$cor), 
          y = get_y(.data$sd, .data$cor),
          colour = .data[[group_by[1]]], 
          shape  = .data[[group_by[2]]], 
          fill   = .data[[group_by[3]]])) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          override.aes = list(shape = 21)))   
    if(fills[1] != "default") {
      taylor = taylor +
        ggplot2::scale_fill_manual(values = fills) 
    }else taylor = taylor +
      ggplot2::scale_fill_viridis_d()
  }else if(length(group_by) == 2) {
    taylor = taylor  + 
      ggplot2::geom_point(
        data = modelled, 
        size = size, stroke = stroke, 
        ggplot2::aes(x = .data$x, y = .data$y,
          colour = .data[[group_by[1]]], 
          shape = .data[[group_by[2]]]))
  }else if(length(group_by) == 1) {
    taylor = taylor  + 
      ggplot2::geom_point(
        data = modelled,
        ggplot2::aes(x = .data$x, y = .data$y, 
          colour = .data[[group_by[1]]]),
        size = size, stroke = stroke, 
        shape  = ifelse(shapes[1] == "default", 21, shapes[1]))
  }else {
    stop(paste(
      "group_by must have a length between 1 and 3, not", length(group_by)))
  }

  if(colours[1] != "default") taylor = taylor +
    ggplot2::scale_colour_manual(values = colours) 
  else taylor = taylor +
    ggplot2::scale_colour_brewer(palette = "Dark2") 
  # Add shapes scales if 2+ group_by
  if(length(group_by > 1)) {
    if(shapes[1] != "default") taylor = taylor +
      ggplot2::scale_shape_manual(values = shapes)
    else taylor = taylor +
      ggplot2::scale_shape_manual(values = 21:30)
  }
  return(taylor)
}

get_x = function(standard_deviation, correlation)
  standard_deviation * cos(pi / 6 * (3 - 3 * correlation))
get_y = function(standard_deviation, correlation)
  standard_deviation * sin((3 - 3 * correlation) * pi / 6)
get_standard_deviation = function(x, y)
  sqrt(x^2 + y^2)
get_correlation = function(x, y)
  atan2(y, x) / pi * -2 + 1