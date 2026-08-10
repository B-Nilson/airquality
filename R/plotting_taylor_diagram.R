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
# TODO: fix RMSE line not cutoff properly for wedge style plot

#' Create a Taylor diagram
#'
#' Visualises model performance using the geometric relationship between
#' correlation, standard deviation, and centred root-mean-square (RMS) error,
#' following Taylor (2001).
#'
#' @param dat A data frame containing at least the columns specified in
#'   `data_cols`, `group_by`, and `facet_by`. Must contain more than 2 rows.
#' @param data_cols A named character vector of length 2 with names `"obs"` and
#'   `"mod"` specifying the column names in `dat` for observed and modelled
#'   values. Defaults to `c(obs = "obs", mod = "mod")`.
#' @param group_by A named character vector of 1–3 column names in `dat` used
#'   to distinguish model output. The first element maps to `colour`, the
#'   second (if present) to `shape`, and the third (if present) to `fill`.
#'   Element names become legend titles.
#' @param facet_by A named character vector of 1–2 column names passed to
#'   [ggplot2::facet_wrap()]. Element names become facet strip labels. Defaults
#'   to `NULL` (no faceting).
#' @param date_col A single string naming a date/time column in `dat` used by
#'   `add_features()` to derive additional grouping variables. Defaults to
#'   `NULL`.
#' @param facet_rows A positive integer giving the number of rows in the facet
#'   layout. Defaults to `1`.
#' @param obs_point_options A named list controlling the appearance of the
#'   observed data point. Accepted elements:
#'   \describe{
#'     \item{`colour`}{Colour of the point. Defaults to `"purple"`.}
#'     \item{`shape`}{Shape of the point. Defaults to `16` (solid circle).}
#'     \item{`size`}{Size of the point. Defaults to `1.5`.}
#'     \item{`stroke`}{Stroke width of the point. Defaults to `1`.}
#'     \item{`label`}{Text label displayed beside the point.
#'       Defaults to `"Obs."`.}
#'     \item{`label_padding`}{Distance (in standard-deviation units) between
#'       the point and its label. Defaults to `labels_padding`.}
#'   }
#' @param mod_point_options A named list controlling the appearance of modelled
#'   data points. Accepted elements:
#'   \describe{
#'     \item{`colours`}{A named character vector mapping `group_by[[1]]` levels
#'       to colours. Defaults to the `"Dark2"` palette from
#'       [ggplot2::scale_colour_brewer()].}
#'     \item{`fills`}{A named character vector mapping `group_by[[3]]` levels to
#'       fill colours (only used when `group_by` has three elements). Defaults
#'       to [ggplot2::scale_fill_viridis_d()].}
#'     \item{`shapes`}{A named integer vector mapping `group_by[[2]]` levels to
#'       point shapes. Defaults to shapes `21` through `30`.}
#'     \item{`size`}{Size of the points. Defaults to `1.5`.}
#'     \item{`stroke`}{Stroke width of the points. Defaults to `1`.}
#'   }
#' @param cor_line_options A named list controlling the radial correlation lines.
#'   Accepted elements:
#'   \describe{
#'     \item{`minimum`}{Minimum correlation value shown, from -1 to 1. Defaults
#'       to the nearest 0.1 at or below the smallest correlation in `dat`, with
#'       a floor of `0.5`.}
#'     \item{`step`}{Spacing between correlation lines. Defaults to `0.1`.}
#'     \item{`colour`}{Line colour. Defaults to `"grey30"`.}
#'     \item{`linetype`}{Line type. Defaults to `"longdash"`.}
#'     \item{`label`}{Axis title. Defaults to `"Correlation"`.}
#'     \item{`label_type`}{Type of label to display for the axis. Options are `"decimal"`
#'       (default) or `"percent".}
#'   }
#' @param rmse_line_options A named list controlling the centred-RMS-error arcs.
#'   Accepted elements:
#'   \describe{
#'     \item{`minimum`}{Minimum RMS error arc drawn (must be `>= 0`). The first
#'       arc is drawn one `step` above this value. Defaults to `0`.}
#'     \item{`step`}{Spacing between RMS error arcs. Defaults to a value
#'       producing approximately 4 arcs with "pretty" spacing.}
#'     \item{`colour`}{Arc colour. Defaults to `"brown"`.}
#'     \item{`linetype`}{Arc line type. Defaults to `"dotted"`.}
#'     \item{`label`}{Axis title. Defaults to `"Centered RMS Error"`.}
#'     \item{`label_pos`}{Position of arc labels as a proportion in \[0, 1\]:
#'       `0` places labels at the leftmost point on the x-axis, `0.5` at the
#'       arc apex, and `1` at the rightmost point. Defaults to 10% above the
#'       midpoint between `minimum` and `1`.}
#'   }
#' @param sd_line_options A named list controlling the standard-deviation arcs.
#'   Accepted elements:
#'   \describe{
#'     \item{`maximum`}{Maximum standard deviation displayed (must be `>= 0`).
#'       Defaults to the nearest multiple of 5 above the largest standard
#'       deviation in `dat`.}
#'     \item{`step`}{Spacing between standard-deviation arcs. Defaults to a
#'       value producing approximately 4 arcs with "pretty" spacing.}
#'     \item{`colour`}{Arc colour. Defaults to `"black"`.}
#'     \item{`linetypes`}{A named character vector with elements `"obs"` and
#'       `"other"` specifying the line type for the observed standard-deviation
#'       arc and all other arcs, respectively. Defaults to
#'       `c(obs = "dashed", other = "dashed")`.}
#'     \item{`label`}{Axis title. Defaults to `"Standard Deviation"`.}
#'   }
#' @param plot_padding A single non-negative number giving extra space (in
#'   standard-deviation units) added beyond the outermost arc. Increase this
#'   value if text labels are clipped. Defaults to `0.5`.
#' @param labels_padding A single non-negative number controlling the distance
#'   (in standard-deviation units) between grid lines or arcs and their text
#'   labels. Adjust to suit the figure size and number of facets.
#'   Defaults to `2`.
#'
#' @details
#' A Taylor diagram represents three performance statistics simultaneously:
#'
#' * **Standard deviation**: the radial distance from the origin.
#' * **Correlation**: the azimuthal angle from the positive x-axis.
#' * **Centred RMS error**: the distance from the observed point on the x-axis.
#'
#' The observed point always sits on the positive x-axis at a distance equal to
#' the observed standard deviation. A model point that overlaps the observed
#' point has perfect agreement (correlation of 1, centred RMS error of 0, and
#' matching standard deviation).
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @references
#' Taylor, K. E. (2001). Summarizing model performance in a single diagram.
#' *Journal of Geophysical Research: Atmospheres*, **106**(D7), 7183–7192.
#' \doi{10.1029/2000JD900719}
#'
#' @family Data Visualisation
#' @family Model Validation
#' @importFrom rlang `%||%`
#' @export
#'
#' @examples
#' \dontrun{
#' # Prepare example data
#' data <- as.data.frame(datasets::ChickWeight) |>
#'   dplyr::filter(.data$Chick == 1) |>
#'   tidyr::pivot_wider(names_from = "Chick", values_from = "weight") |>
#'   dplyr::full_join(
#'     as.data.frame(datasets::ChickWeight) |>
#'       dplyr::filter(.data$Chick != 1)
#'   ) |>
#'   dplyr::rename(obs = `1`, mod = "weight") |>
#'   dplyr::mutate(Chick = factor(round(as.numeric(.data$Chick) / 20)))
#'
#' # Basic usage with two grouping variables
#' taylor_diagram(data, group_by = c(Diet = "Diet", Chick = "Chick"))
#'
#' # Extend the correlation axis to include 0 and reposition RMS error labels
#' taylor_diagram(
#'   data,
#'   group_by = c(Diet = "Diet", Chick = "Chick"),
#'   cor_line_options  = list(minimum = 0),
#'   rmse_line_options = list(label_pos = 0.3)
#' )
#'
#' # Custom colours, shapes, and line styles
#' taylor_diagram(
#'   data,
#'   group_by  = c(Diet = "Diet", Chick = "Chick"),
#'   obs_point_options = list(colour = "brown", shape = 23, size = 6),
#'   mod_point_options = list(
#'     colours = c("1" = "pink", "2" = "blue", "3" = "green", "4" = "orange"),
#'     size  = 4,
#'     stroke  = 2
#'   ),
#'   cor_line_options = list(colour = "orange", linetype = "dotdash"),
#'   rmse_line_options = list(colour = "green",  linetype = "longdash"),
#'   sd_line_options = list(
#'     colour = "purple",
#'     linetypes = c(obs = "solid", other = "dashed")
#'   )
#' )
#'
#' # Adjust label and plot padding
#' taylor_diagram(
#'   data,
#'   group_by = c(Diet = "Diet", Chick = "Chick"),
#'   plot_padding = 4,
#'   labels_padding = 1,
#'   rmse_line_options = list(label_pos = 0.7)
#' )
#' }
taylor_diagram <- function(
  dat,
  data_cols = c(obs = "obs", mod = "mod"),
  group_by,
  facet_by = NULL,
  date_col = NULL,
  facet_rows = 1,
  obs_point_options = list(
    colour = "purple",
    shape = 16,
    size = 2,
    stroke = 1,
    label = "Obs.",
    label_padding = labels_padding
  ),
  mod_point_options = list(
    colours = NULL,
    fills = NULL,
    shapes = 21,
    size = 3,
    stroke = 1
  ),
  cor_line_options = list(
    minimum = NULL,
    step = 0.1,
    colour = "grey30",
    linetype = "longdash",
    label = "Correlation",
    label_type = "decimal"
  ),
  rmse_line_options = list(
    minimum = 0,
    step = NULL,
    colour = "brown",
    linetype = "dotted",
    label = "Centered RMS Error",
    label_pos = NULL
  ),
  sd_line_options = list(
    maximum = NULL,
    step = NULL,
    colour = "black",
    linetypes = c(obs = "dashed", other = "dashed"),
    label = "Standard Deviation"
  ),
  plot_padding = 0.5,
  labels_padding = 0.5
) {
  stopifnot(
    is.data.frame(dat) & nrow(dat) > 2,
    is.character(data_cols) & length(data_cols) == 2,
    all(data_cols %in% names(dat)),
    is.null(names(data_cols)) || all(c("obs", "mod") %in% names(data_cols)),
    is.null(date_col) || (is.character(date_col) & length(date_col) == 1),
    is.character(group_by) & length(group_by) > 0,
    is.null(facet_by) || (is.character(facet_by) & length(facet_by) > 0),
    is.numeric(facet_rows) & facet_rows > 0,
    is.list(obs_point_options) &
      all(
        names(obs_point_options) %in%
          c("colour", "shape", "size", "stroke", "label", "label_padding")
      ) #,
    # is.null(cor_minimum) ||
    #   (is.numeric(cor_minimum) & length(cor_minimum) == 1),
    # is.null(cor_minimum) || (cor_minimum >= -1 & cor_minimum <= 1),
    # # TODO: check other cor_ args

    # is.numeric(rmse_minimum) & rmse_minimum >= 0,
    # # TODO: check other rmse_ args

    # is.null(sd_maximum) || (is.numeric(sd_maximum) & sd_maximum >= 0)
    # # TODO: check other sd_ args
  )

  # Add any missing columns if possible
  dat <- dat |>
    add_features(features = c(group_by, facet_by), date_col = date_col)
  stopifnot(
    all(group_by %in% names(dat)),
    is.null(facet_by) || all(facet_by %in% names(dat))
    # ensure names/cols unique (cant group and facet by same col, cant rename 2 cols same name)
  )

  # Handle inputs
  if (is.null(names(data_cols))) {
    names(data_cols) <- c("obs", "mod")
  }
  if (is.null(names(group_by))) {
    names(group_by) <- group_by
  }
  if (is.null(names(facet_by))) {
    names(facet_by) <- facet_by
  }

  # Select, rename and factorize columns
  dat <- dat |>
    dplyr::select(dplyr::all_of(c(data_cols, group_by, facet_by))) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(names(c(group_by, facet_by))), factor)
    )

  # Get modelled standard deviation and correlation with obs by group/facet
  modelled <- dat |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(names(c(group_by, facet_by))))
    ) |>
    dplyr::summarise(
      .groups = "drop",
      sd = .data$mod |> stats::sd(na.rm = TRUE),
      cor = .data$obs |> stats::cor(.data$mod, use = "pairwise.complete.obs"),
      x = .data$sd |> get_x(.data$cor),
      y = .data$sd |> get_y(.data$cor)
    )

  # Get observed standard deviation by facet
  observed <- dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(names(facet_by)))) |>
    dplyr::summarise(sd = .data$obs |> stats::sd(na.rm = TRUE))

  # Make Taylor Diagram
  taylor <- observed |>
    make_taylor_diagram_template(
      modelled = modelled,
      facet_by = names(facet_by),
      facet_rows = facet_rows,
      cor_minimum = cor_line_options$minimum,
      cor_step = cor_line_options$step %||% 0.1,
      cor_colour = cor_line_options$colour %||% "grey30",
      cor_linetype = cor_line_options$linetype %||% "longdash",
      cor_label_type = cor_line_options$label_type %||% "decimal",
      cor_label = cor_line_options$label %||% "Correlation",
      rmse_minimum = rmse_line_options$minimum %||% 0,
      rmse_step = rmse_line_options$step,
      rmse_colour = rmse_line_options$colour %||% "brown",
      rmse_linetype = rmse_line_options$linetype %||% "dotted",
      rmse_label = rmse_line_options$label %||% "Centered RMS Error",
      rmse_label_pos = rmse_line_options$label_pos,
      sd_maximum = sd_line_options$maximum,
      sd_step = sd_line_options$step,
      sd_colour = sd_line_options$colour %||% "black",
      sd_linetypes = sd_line_options$linetypes %||%
        c(obs = "dashed", other = "dashed"),
      sd_label = sd_line_options$label %||% "Standard Deviation",
      padding_limits = plot_padding,
      nudge_labels = labels_padding
    ) |>
    add_taylor_observed_point(
      observed = observed,
      colour = obs_point_options$colour %||% "purple",
      shape = obs_point_options$shape %||% 16,
      size = obs_point_options$size %||% 1.5,
      stroke = obs_point_options$stroke %||% 1,
      label = obs_point_options[["label"]] %||% "Obs.",
      nudge_labels = obs_point_options$label_padding %||% labels_padding
    ) |>
    add_taylor_modelled_points(
      modelled = modelled,
      group_by = group_by,
      stroke = mod_point_options$stroke %||% 1,
      size = mod_point_options$size %||% 1.5,
      colours = mod_point_options$colours,
      fills = mod_point_options$fills,
      shapes = mod_point_options$shapes
    )

  if (length(group_by) >= 1) {
    taylor <- taylor +
      ggplot2::labs(fill = names(group_by)[1])
  }
  if (length(group_by) >= 2) {
    taylor <- taylor +
      ggplot2::labs(colour = names(group_by)[1], shape = names(group_by)[2])
  }
  if (length(group_by) >= 3) {
    taylor <- taylor +
      ggplot2::labs(fill = names(group_by)[3])
  }
  return(taylor)
}

make_taylor_diagram_template <- function(
  observed,
  modelled,
  facet_by = NULL,
  facet_rows = 1,
  cor_minimum = NULL,
  cor_step = 0.1,
  cor_colour = "grey30",
  cor_linetype = "solid",
  cor_label = "Correlation",
  cor_label_type = c("decimal", "percent")[1],
  rmse_minimum = 0,
  rmse_step = NULL,
  rmse_colour = "brown",
  rmse_linetype = "dotted",
  rmse_label = "Centered RMS Error",
  rmse_label_pos = NULL,
  sd_maximum = NULL,
  sd_step = NULL,
  sd_colour = "black",
  sd_linetypes = c(obs = "dashed", other = "dashed"),
  sd_label = "Standard Deviation",
  padding_limits = 2,
  nudge_labels = 0.5
) {
  # Handle NULL defaults
  sd_max <- sd_maximum %||% ceiling(max(c(observed$sd, modelled$sd)) / 5) * 5
  min_cor <- cor_minimum %||%
    pmin(floor(min(modelled$cor, na.rm = TRUE) * 10) / 10, 0.5)
  rmse_label_pos <- rmse_label_pos %||% (min_cor + 1) / 2 * 0.9

  # Make limits/labels
  x_min <- if (min_cor < 0) get_x(sd_max, min_cor) else 0
  xlims <- c(x_min, sd_max)
  x_title_hjust <- if (min_cor >= 0 || min_cor == -1 || !is.null(facet_by)) {
    0.5
  } else {
    1 - (xlims[2] / 2 / (xlims[2] - xlims[1]))
  }
  x_lab <- "%s<br><span style='color: %s; font-size: 8pt;'>%s</span>" |>
    sprintf(sd_label, rmse_colour, rmse_label)

  # Build plot
  blank <- ggplot2::element_blank()
  ggplot2::ggplot() |>
    add_taylor_cor_lines(
      observed = observed,
      min_cor = min_cor,
      step = cor_step,
      sd_max = sd_max,
      colour = cor_colour,
      linetype = cor_linetype,
      axis_label = cor_label,
      label_type = cor_label_type,
      nudge_labels = nudge_labels
    ) |>
    add_taylor_sd_lines(
      min_cor = min_cor,
      sd_max = sd_max,
      sd_step = sd_step,
      observed = observed,
      colour = sd_colour,
      linetypes = sd_linetypes
    ) |>
    add_taylor_rmse_lines(
      observed = observed,
      sd_max = sd_max,
      min_cor = min_cor,
      rmse_minimum = rmse_minimum,
      rmse_step = rmse_step,
      label_pos = rmse_label_pos,
      colour = rmse_colour,
      linetype = rmse_linetype,
      nudge_labels = nudge_labels,
      padding_limits = padding_limits
    ) |>
    add_taylor_axes_lines(
      observed = observed,
      min_cor = min_cor,
      sd_max = sd_max
    ) |>
    facet_plot(by = facet_by, rows = facet_rows) |>
    add_default_theme() +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.05))) +
    ggplot2::theme(
      axis.line.y = blank,
      axis.ticks.y = blank,
      axis.text.y = blank,
      axis.title.y = blank,
      panel.grid = blank,
      axis.title.x = ggtext::element_markdown(hjust = x_title_hjust),
      legend.box.spacing = ggplot2::unit("2", "pt")
    ) +
    ggplot2::labs(x = x_lab)
}

# Add radial correlation lines (and labels) to Taylor Diagrams
add_taylor_cor_lines <- function(
  taylor,
  observed,
  min_cor = 0,
  sd_max,
  colour = "grey30",
  linetype = "solid",
  axis_label = "Correlation",
  step = 0.1,
  label_type = "decimal",
  nudge_labels = 0.5
) {
  draw_at <- seq(from = min_cor, to = 1, by = step)

  # Make locations for the correlation line end points and labels
  label_dist <- sd_max + nudge_labels * 0.6
  cor_lines <- draw_at |>
    lapply(\(at) {
      label <- ifelse(
        label_type == "percent",
        paste(round(at * 100), "%"),
        round(at, digits = 2)
      )
      observed |>
        dplyr::mutate(
          xend = get_x(sd_max, at),
          yend = get_y(sd_max, at),
          x_label = get_x(label_dist, at),
          y_label = get_y(label_dist, at),
          label = label
        )
    }) |>
    dplyr::bind_rows()
  # Make location for the label for the axis title
  dist_from_origin <- label_dist + nudge_labels
  mean_cor <- mean(c(min_cor, 1))
  axis_title <- observed |>
    dplyr::mutate(
      x = get_x(dist_from_origin, mean_cor),
      y = get_y(dist_from_origin, mean_cor)
    )

  taylor +
    # Correlation lines
    ggplot2::geom_segment(
      data = cor_lines,
      linewidth = 0.25,
      linetype = linetype,
      colour = colour,
      ggplot2::aes(
        x = 0,
        y = 0,
        xend = .data$xend,
        yend = .data$yend
      )
    ) +
    # Labels for each correlation line
    ggplot2::geom_text(
      data = cor_lines,
      size = 3,
      ggplot2::aes(
        .data$x_label,
        .data$y_label,
        label = .data$label
      ),
      colour = ggplot2::theme_get()$axis.text$colour
    ) +
    # Correlation axis label
    ggplot2::geom_text(
      data = axis_title,
      size = 4,
      colour = "black",
      ggplot2::aes(.data$x, .data$y),
      label = axis_label,
      angle = mean_cor * -90
    )
}

# Add standard deviation arcs to Taylor Diagrams
add_taylor_sd_lines <- function(
  taylor,
  observed,
  min_cor,
  sd_max,
  sd_step = NULL,
  colour = "black",
  linetypes = c(obs = "dashed", other = "dashed")
) {
  linetypes <- c(linetypes, max = "solid")
  if (is.null(sd_step)) {
    lines_at <- pretty(seq(0, sd_max, length.out = 4))
    lines_at <- lines_at[lines_at < sd_max]
  } else {
    lines_at <- seq(0, sd_max, sd_step)
  }
  lines_at <- unique(c(lines_at, sd_max))

  arc_data <- seq_len(nrow(observed)) |>
    lapply(\(i) {
      at <- unique(c(lines_at, observed$sd[i]))
      data.frame(
        observed[i, ],
        start = -0.5 * pi * -min_cor,
        end = 0.5 * pi,
        r = at,
        linetype = ifelse(
          at == max(at),
          "max",
          ifelse(at == observed$sd[i], "obs", "other")
        )
      )
    }) |>
    dplyr::bind_rows()
  linewidths <- c(obs = 0.5, other = 0.25, max = 0.5)

  taylor +
    ggforce::geom_arc(
      data = arc_data,
      colour = colour,
      ggplot2::aes(
        x0 = 0,
        y0 = 0,
        r = .data$r,
        start = .data$start,
        end = .data$end,
        linewidth = .data$linetype,
        linetype = .data$linetype
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = linetypes,
      guide = "none"
    ) +
    ggplot2::scale_linewidth_manual(
      values = linewidths,
      guide = "none"
    ) +
    # TODO: get facet pairs in order added, get obs sd for each pair, add to global var whenever labels checked, don't label if global index of obs sd within x% of label
    ggplot2::scale_x_continuous(
      breaks = if (min_cor > -1) lines_at else c(-lines_at, lines_at),
      labels = \(l) ifelse(l < 0 & min_cor > -1, "", abs(l))
    )
}

# Add SD axes lines to Taylor Diagrams
# TODO: add customizability
add_taylor_axes_lines <- function(taylor, observed, min_cor, sd_max) {
  axes_lines <- c(min_cor, 1) |>
    lapply(\(correlation) {
      observed |>
        dplyr::mutate(
          xend = get_x(sd_max, correlation),
          yend = get_y(sd_max, correlation)
        )
    }) |>
    dplyr::bind_rows()
  taylor +
    ggplot2::geom_segment(
      data = axes_lines,
      ggplot2::aes(xend = .data$xend, yend = .data$yend),
      x = 0,
      y = 0
    )
}

add_taylor_rmse_lines <- function(
  taylor,
  observed,
  sd_max,
  min_cor,
  label_pos = 0.6,
  rmse_minimum = 0,
  rmse_step = NULL,
  colour = "brown",
  linetype = "dotted",
  nudge_labels,
  padding_limits
) {
  rms_lines <- make_taylor_rmse_lines(
    observed = observed,
    sd_max = sd_max,
    min_cor = min_cor,
    label_pos = ((1 - label_pos) * 255 - 20),
    rmse_minimum = rmse_minimum,
    rmse_step = rmse_step,
    padding_limits = padding_limits
  )
  taylor +
    # Draw semicircles originating at the observed point for centered RMS error
    ggplot2::geom_line(
      data = rms_lines$lines,
      ggplot2::aes(.data$x, .data$y, group = .data$rmse_values),
      linetype = linetype,
      colour = colour
    ) +
    # Line labels
    ggplot2::geom_text(
      data = rms_lines$labels,
      size = 3, # TODO: make input
      ggplot2::aes(.data$x, .data$y, label = .data$label),
      vjust = 1,
      hjust = ifelse(label_pos <= 0.4, 0, ifelse(label_pos >= 0.6, 1, 0)),
      colour = colour,
      nudge_y = nudge_labels * -0.1,
      nudge_x = nudge_labels *
        ifelse(label_pos <= 0.4, 0.1, ifelse(label_pos >= 0.1, -0.1, 0))
    )
}

make_taylor_rmse_lines <- function(
  observed,
  sd_max,
  min_cor,
  label_pos = 80,
  rmse_minimum = 0,
  rmse_step = NULL,
  padding_limits = 2
) {
  max_rmse <- if (min_cor < 0) sd_max + sd_max * -min_cor else sd_max
  if (is.null(rmse_step)) {
    rmse_values <- pretty(seq(rmse_minimum, max_rmse, length.out = 5))
    if (rmse_values[1] == 0) rmse_values <- rmse_values + rmse_minimum
  } else {
    rmse_values <- seq(rmse_minimum, max_rmse, rmse_step)
  }
  rmse_values <- rmse_values[rmse_values != 0]
  labelpos <- seq(45, 70, length.out = length(rmse_values)) + label_pos

  lines_lables <- lapply(seq_len(nrow(observed)), \(obs_i) {
    lapply(seq_along(rmse_values), \(i) {
      rmse_value <- rmse_values[i]
      # Get x/y coordinates of a half-circle transposed to x=sd_obs for each rmse_value
      curve_points <- seq(0, pi, by = 0.01)
      xcurve <- cos(curve_points) * rmse_value + observed$sd[obs_i]
      ycurve <- sin(curve_points) * rmse_value

      list(
        lines = data.frame(
          observed[obs_i, ],
          x = xcurve,
          y = ycurve,
          rmse_values = as.factor(rmse_value)
        ),
        labels = data.frame(
          observed[obs_i, ],
          x = xcurve[labelpos[i]],
          y = ycurve[labelpos[i]],
          label = as.character(rmse_value),
          rmse_values = as.factor(rmse_value)
        )
      )
    })
  })
  list(
    lines = lines_lables |>
      lapply(\(x) x |> lapply(\(y) y$lines) |> dplyr::bind_rows()) |>
      dplyr::bind_rows() |>
      dplyr::filter(
        get_standard_deviation(.data$x, .data$y) < sd_max,
        get_correlation(.data$x, .data$y) >= min_cor
      ),
    labels = lines_lables |>
      lapply(\(x) x |> lapply(\(y) y$labels) |> dplyr::bind_rows()) |>
      dplyr::bind_rows() |>
      dplyr::filter(
        get_standard_deviation(.data$x, .data$y) < sd_max - padding_limits,
        get_correlation(.data$x, .data$y) >= min_cor,
        get_correlation(.data$x, .data$y) <= 1
      )
  )
}

add_taylor_observed_point <- function(
  taylor,
  observed,
  shape = 16,
  size = 1.5,
  stroke = 1,
  colour = "purple",
  label = "Obs.",
  nudge_labels = 2
) {
  taylor +
    ggplot2::geom_point(
      data = observed,
      ggplot2::aes(x = .data$sd, y = 0),
      shape = shape,
      stroke = stroke,
      colour = colour,
      size = size
    ) +
    ggplot2::geom_text(
      data = observed,
      ggplot2::aes(x = .data$sd, y = 0),
      label = label,
      colour = colour,
      size = 3,
      vjust = 3 + nudge_labels,
      hjust = 0.5
    )
}

# TODO: implement?
get_shape_pairs <- function(shapes) {
  pairs <- list(
    filled = 21:25,
    not_filled = c(1, 0, 5, 2, 6) # cirle, square, diamond, tri-up, tri-down
  )
  is_filled <- shapes %in% pairs$filled
  is_not_filled <- shapes %in% pairs$not_filled
  if (any(!is_filled & !is_not_filled)) {
    stop(
      "Cannot find paired filled/not-filled shapes for shape(s) ",
      paste(collapse = ", ", shapes[!is_filled & !is_not_filled])
    )
  }
  data.frame(shapes) |>
    dplyr::mutate(
      filled = ifelse(
        is_filled,
        shapes,
        pairs$filled[match(shapes, pairs$not_filled)]
      ),
      not_filled = ifelse(
        is_not_filled,
        shapes,
        pairs$not_filled[match(shapes, pairs$filled)]
      )
    )
}

add_taylor_modelled_points <- function(
  taylor,
  modelled,
  group_by,
  size = 1.5,
  stroke = 1,
  shapes = NULL,
  colours = NULL,
  fills = NULL
) {
  # TODO: instead, find matching shapes with fill/no fill and combine last two group_by into shape + fill/no fill
  if (length(group_by) == 3) {
    taylor <- taylor +
      ggplot2::geom_point(
        data = modelled,
        size = size,
        stroke = stroke,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          colour = .data[[group_by[1]]],
          shape = .data[[group_by[2]]],
          fill = .data[[group_by[3]]]
        )
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          override.aes = list(shape = 21)
        )
      )
    if (!is.null(fills)) {
      taylor <- taylor +
        ggplot2::scale_fill_manual(values = fills)
    } else {
      taylor <- taylor +
        ggplot2::scale_fill_viridis_d()
    }
  } else if (length(group_by) == 2) {
    taylor <- taylor +
      ggplot2::geom_point(
        data = modelled,
        size = size,
        stroke = stroke,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          colour = .data[[group_by[1]]],
          shape = .data[[group_by[2]]]
        )
      )
  } else if (length(group_by) == 1) {
    taylor <- taylor +
      ggplot2::geom_point(
        data = modelled,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          fill = .data[[group_by[1]]]
        ),
        colour = "black",
        size = size,
        stroke = stroke,
        shape = ifelse(is.null(shapes), 21, shapes[1])
      )
  } else {
    stop(paste(
      "group_by must have a length between 1 and 3, not",
      length(group_by)
    ))
  }

  if (!is.null(colours)) {
    taylor <- taylor +
      ggplot2::scale_colour_manual(values = colours)
  } else {
    taylor <- taylor +
      ggplot2::scale_colour_brewer(palette = "Dark2")
  }
  # Add shapes scales if 2+ group_by
  if (length(group_by) > 1) {
    if (!is.null(shapes)) {
      taylor <- taylor +
        ggplot2::scale_shape_manual(values = shapes)
    } else {
      taylor <- taylor +
        ggplot2::scale_shape_manual(values = 21:30)
    }
  }
  return(taylor)
}

get_x <- function(standard_deviation, correlation) {
  standard_deviation * cos(pi / 6 * (3 - 3 * correlation))
}
get_y <- function(standard_deviation, correlation) {
  standard_deviation * sin((3 - 3 * correlation) * pi / 6)
}
get_standard_deviation <- function(x, y) {
  sqrt(x^2 + y^2)
}
get_correlation <- function(x, y) {
  atan2(y, x) / pi * -2 + 1
}
