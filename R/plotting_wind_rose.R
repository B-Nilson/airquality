# TODO: handle other units
# TODO: Present amount of missing values inside center of plot
# TODO: add data date range to subtitle or caption
# TODO: handle when a facet doesn't have any data

#' Create wind rose diagrams
#'
#' Generates polar bar charts (wind roses) that summarise the joint
#' distribution of wind speed and direction. Bars are stacked by wind speed
#' category and oriented toward the compass direction from which the wind
#' originates.
#'
#' @param obs A data frame with at least the columns named in `data_cols` and,
#'   when supplied, `facet_by`. Must contain at least one row.
#' @param data_cols A named character vector of length 2 mapping the logical
#'   roles `"ws"` (wind speed) and `"wd"` (wind direction) to column names in
#'   `obs`. Defaults to `c(ws = "ws_1hr", wd = "wd_1hr")`.
#' @param facet_by A character vector of one or two column names in `obs` used
#'   as faceting variables in [ggplot2::facet_wrap()]. Names, if present, are
#'   used as strip labels. Defaults to `NULL` (no faceting).
#' @param facet_rows A single positive integer giving the number of rows in the
#'   facet layout. Ignored when `facet_by` is `NULL`. Defaults to `1`.
#' @param wd_nbins Number of directional bins. Must be one of `16`
#'   (N, NNE, NE, ENE, …), `8` (N, NE, E, SE, …), or `4` (N, E, S, W).
#'   Defaults to `16`.
#' @param freq_labels_position A single numeric value in \[0, 360\] specifying
#'   the compass bearing (degrees) at which the frequency-axis labels are
#'   placed. Values are snapped to the nearest cardinal direction. Defaults to
#'   `NULL`, which places labels at the least busy cardinal direction.
#' @param ws_min Minimum wind speed to include. Observations below this
#'   threshold are dropped before plotting. Units are inferred from the data or
#'   from `ws_out_units`. Defaults to `0`.
#' @param ws_step Width of each wind speed bin in the units given by
#'   `ws_out_units`. Defaults to `2`.
#' @param ws_out_units A single character string specifying the output units for
#'   wind speed (e.g. `"m/s"`, `"km/h"`, `"mph"`). When the wind speed column
#'   does not carry unit metadata it is assumed to be in m/s and converted as
#'   needed. Defaults to `"m/s"`.
#' @param fills A character vector of colours for the wind speed fill scale.
#'   Should have at least as many values as there are wind speed bins. Pass
#'   `"default"` to use [ggplot2::scale_fill_viridis_d()]. Defaults to
#'   `"default"`.
#' @param colour A single character string giving the colour used for bar
#'   outlines. Defaults to `"black"`.
#' @param alpha A single numeric value in \[0, 1\] controlling the opacity of
#'   the bar fills. `0` is fully transparent; `1` is fully opaque. Defaults
#'   to `0.8`.
#' @param bar_width A single numeric value in \[0, 1\] controlling the relative
#'   width of each directional bar. A value of `1` makes adjacent bars touch.
#'   Defaults to `1`.
#' @param ... Additional arguments passed to [ggplot2::geom_col()], such as
#'   `linewidth` or `linetype`.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @family Data Visualisation
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Basic usage
#' wind_rose(example_obs)
#'
#' # Coarser 8-point rose with output in km/h
#' wind_rose(example_obs, wd_nbins = 8, ws_out_units = "km/h", ws_step = 10)
#' }
wind_rose <- function(
  obs,
  data_cols = c(ws = "ws_1hr", wd = "wd_1hr"),
  facet_by = NULL,
  facet_rows = 1,
  wd_nbins = c(16, 8, 4)[1],
  freq_labels_position = NULL,
  ws_min = 0,
  ws_step = 2,
  ws_out_units = "m/s",
  fills = "default",
  colour = "black",
  alpha = 0.8,
  bar_width = 1,
  ...
) {
  # fmt: skip
  stopifnot(
    is.data.frame(obs) & nrow(obs) > 0,
    is.character(data_cols) & length(data_cols) == 2,
    length(names(data_cols)) %in% c(0, 2),
    is.null(names(facet_by)) | is.character(facet_by),
    length(facet_rows) == 1 & is.numeric(facet_rows),
    length(wd_nbins) == 1 & is.numeric(wd_nbins) & wd_nbins %in% c(16, 8, 4),
    is.null(freq_labels_position) | (length(freq_labels_position) == 1 & is.numeric(freq_labels_position)),
    freq_labels_position >= 0 & freq_labels_position <= 360,
    length(ws_min) == 1 & is.numeric(ws_min),
    length(ws_step) == 1 & is.numeric(ws_step) & ws_step > 0,
    length(ws_out_units) == 1 & is.character(ws_out_units),
    length(fills) >= 1 & is.character(fills),
    length(colour) == 1 & is.character(colour),
    length(alpha) == 1 & is.numeric(alpha) & alpha >= 0 & alpha <= 1,
    length(bar_width) == 1 & is.numeric(bar_width) & bar_width >= 0 & bar_width <= 1
  )

  # Handle inputs
  if (is.null(names(data_cols))) {
    names(data_cols) <- c("ws", "wd")
  }
  if (is.null(names(facet_by))) {
    names(facet_by) <- facet_by
  }
  wd_step <- 360 / wd_nbins
  ws_min <- ws_min |> units::set_units(ws_out_units, mode = "standard")

  rose_data <- obs |>
    dplyr::select(dplyr::all_of(c(data_cols, facet_by)))
  facet_by <- names(facet_by)

  rose_data <- rose_data |>
    dplyr::mutate(
      ws = .data$ws |>
        units::set_units(ws_out_units, mode = "standard"),
      wd = .data$wd |>
        units::set_units("degrees")
    ) |>
    # Drop too low of winds and make ws/wd bins
    dplyr::filter(.data$ws >= ws_min, !is.na(.data$wd))

  if (nrow(rose_data) == 0) {
    "No observations where wind speed >= %s %s and wind direction is not NA" |>
      sprintf(ws_min, ws_out_units) |>
      stop()
  }

  rose_data <- rose_data |>
    # Make ws/wd bins
    dplyr::mutate(
      ws_bin = .data$ws |>
        cut_wind_speed(ws_min = ws_min, ws_step = ws_step),
      wd_bin = .data$wd |>
        get_cardinal_direction(wd_step = wd_step)
    ) |>
    # Get counts for each ws/wd bin in each facet
    dplyr::group_by(
      .data$ws_bin,
      .data$wd_bin,
      dplyr::across(dplyr::all_of(facet_by))
    ) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    # Calculate % of obs within each rose
    dplyr::group_by(dplyr::across(dplyr::all_of(facet_by))) |>
    dplyr::mutate(p = .data$n / sum(.data$n)) |>
    # Infill any missing wd bins so plot spacing is correct
    tidyr::complete(.data$wd_bin, fill = list(n = 0, p = 0))

  rose_data |>
    make_wind_rose_base(
      facet_by = facet_by,
      facet_rows = facet_rows,
      freq_labels_position = freq_labels_position,
      wd_step = wd_step,
      ws_out_units = ws_out_units,
      fills = fills,
      colour = colour
    ) +
    ggplot2::geom_col(
      ggplot2::aes(
        x = .data$wd_bin,
        y = .data$p,
        fill = forcats::fct_rev(.data$ws_bin)
      ),
      colour = colour,
      alpha = alpha,
      width = bar_width,
      ...
    )
}

#' Build the base ggplot object for a wind rose
#'
#' Sets up polar coordinates, axis scales, fill scale, and optional faceting.
#' This is a low-level helper intended to be called by [wind_rose()]; it
#' returns a [ggplot2::ggplot()] object with no geometry layer so that the
#' caller can append the appropriate `geom_*` call.
#'
#' @param rose_data A summarised data frame produced inside [wind_rose()],
#'   containing at minimum the columns `wd_bin`, `ws_bin`, `p`, and any
#'   columns named in `facet_by`.
#' @inheritParams wind_rose
#' @param wd_step Arc width of each directional bin in degrees. Derived from
#'   `wd_nbins` in [wind_rose()] as `360 / wd_nbins`.
#'
#' @return A [ggplot2::ggplot()] object with polar coordinates, scales, and
#'   theme applied but no geometry layer.
#'
#' @keywords internal
#' @noRd
make_wind_rose_base <- function(
  rose_data,
  facet_by = NULL,
  facet_rows = 1,
  freq_labels_position = NULL,
  wd_step = 22.5,
  ws_out_units = "m/s",
  fills = "default",
  colour = "black"
) {
  if (is.null(freq_labels_position)) {
    dir_totals <- rose_data |>
      dplyr::group_by(.data$wd_bin) |>
      dplyr::summarise(
        max_p = max(.data$p, na.rm = TRUE),
        sum_p = sum(.data$p, na.rm = TRUE)
      )
    freq_labels_position <- dir_totals |>
      dplyr::filter(.data$max_p == min(.data$max_p, na.rm = TRUE)) |>
      dplyr::pull("wd_bin") |>
      dplyr::first()
  } else {
    freq_labels_position <- freq_labels_position |> get_cardinal_direction()
  }

  # Determine most frequent direction across all facets
  most_frequent <- dir_totals |>
    dplyr::filter(.data$sum_p == max(.data$sum_p, na.rm = TRUE)) |>
    dplyr::pull("sum_p") |>
    dplyr::first()

  # Make base plot
  gg <- ggplot2::ggplot(rose_data) |>
    facet_plot(by = facet_by, rows = facet_rows) |>
    add_default_theme() +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(colour = colour),
      legend.position = "right",
      legend.direction = "vertical"
    ) +
    ggplot2::coord_radial(
      r.axis.inside = as.integer(freq_labels_position),
      start = -handyr::convert_units(
        wd_step / 2,
        from = "degrees",
        to = "radians"
      )
    ) +
    ggplot2::scale_x_discrete(
      expand = ggplot2::expansion(0),
      labels = \(x) ifelse(nchar(x) < 3, x, "")
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(c(0, 0.05)),
      labels = \(x) ifelse(x > 0, paste0(round(x * 100), "%"), "")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = "Wind Speed [%s]" %>% sprintf(ws_out_units),
    )

  # TODO: handle fill palletes
  # Add fill colours
  if (fills[1] != "default") {
    gg <- gg + ggplot2::scale_fill_manual(values = rev(fills)) # TODO: test this works as expected
  } else {
    gg <- gg +
      ggplot2::scale_fill_viridis_d(direction = -1, na.translate = FALSE)
  }

  return(gg)
}

#' Cut wind speeds into labelled bins
#'
#' Discretises a numeric wind speed vector into equal-width intervals starting
#' at `ws_min`. The uppermost bin is closed on the right at the observed
#' maximum wind speed.
#'
#' @param ws A numeric or `units` vector of wind speeds.
#' @param ws_min Lower boundary of the first bin. Defaults to `0`.
#' @param ws_step Width of each bin. Defaults to `2`.
#'
#' @return An ordered [factor()] with levels of the form `"a - b"`, where `a`
#'   and `b` are the lower and upper bin boundaries.
#'
#' @keywords internal
#' @noRd
cut_wind_speed <- function(ws, ws_min = 0, ws_step = 2) {
  max_ws <- max(ws, na.rm = TRUE)
  speed_bins <- seq(ws_min, max_ws, by = ws_step) |>
    c(max_ws) |>
    unique()
  speed_labels <- paste(
    speed_bins,
    "-",
    dplyr::lead(speed_bins)
  )[
    1:(length(speed_bins) - 1)
  ]

  cut(ws, include.lowest = TRUE, breaks = speed_bins, labels = speed_labels)
}

#' Map numeric wind directions to cardinal direction labels
#'
#' Bins a numeric wind direction vector (0–360°) into named compass sectors.
#' The number of sectors is controlled by `wd_step`: `22.5°` gives a 16-point
#' rose (N, NNE, NE, ENE, …), `45°` gives 8-point (N, NE, E, SE, …), and
#' `90°` gives 4-point (N, E, S, W). Values of exactly 0° and 360° are both
#' mapped to North.
#'
#' @param wd A numeric or `units` vector of wind directions in degrees (0–360).
#' @param wd_step Arc width of each bin in degrees. Must be one of `22.5`,
#'   `45`, or `90`. Defaults to `22.5`.
#'
#' @return A [factor()] with levels ordered clockwise starting from `"N"`.
#'
#' @keywords internal
#' @noRd
get_cardinal_direction <- function(wd, wd_step = 22.5) {
  dir_labels <- c(
    "N",
    "NNE",
    "NE",
    "ENE",
    "E",
    "ESE",
    "SE",
    "SSE",
    "S",
    "SSW",
    "SW",
    "WSW",
    "W",
    "WNW",
    "NW",
    "NNW",
    "N"
  )
  if (wd_step == 22.5) {
    set <- seq_along(dir_labels)
  } else if (wd_step == 45) {
    set <- seq(1, length(dir_labels), 2)
  } else if (wd_step == 90) {
    set <- seq(1, length(dir_labels), 4)
  } else {
    stop("Provided wd_step is not supported.")
  }

  half_step <- wd_step / 2
  dir_bins <- c(
    -half_step,
    seq(half_step, 360 - half_step, by = wd_step),
    360 + half_step
  )
  cut(wd, breaks = dir_bins, labels = dir_labels[set])
}
