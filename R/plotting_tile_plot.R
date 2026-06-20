#' Create tiled summary diagrams
#'
#' Assesses relationships in a variable based on two grouping variables by
#' producing a tile (heatmap-style) plot that summarises a `z` variable across
#' all combinations of `x` and `y`.
#'
#' @param obs A data frame containing at least all columns named in `x`, `y`,
#'   `z`, and (if provided) `facet_by`.
#' @param x,y Character strings giving the column names in `obs` to use as the
#'   horizontal and vertical axes, respectively. If either value is a function found in the `lubridate` package, 
#'   and that column is absent from `obs`, it will be derived from `date_col`.
#' @param z A character string giving the column name in `obs` whose values are
#'   summarised by `FUN` for each `x`/`y` combination.
#' @param z_lims A numeric vector of length 2 giving the lower and upper limits
#'   for the fill colour scale. Use `NA` for either value to defer to the data
#'   range. Default is `c(NA, NA)`.
#' @param z_lab A character string for the fill legend label. Default is the
#'   value of `z`.
#' @param date_col A single character string giving the column name in `obs`
#'   containing observation dates. Used to derive temporal columns when `x` or
#'   `y` are date-part values (e.g. `"hour"`, `"month"`). Defaults to
#'   `NULL`.
#' @param facet_by A character vector of one or two column names to use as
#'   faceting variables passed to [ggplot2::facet_wrap()]. Named elements will
#'   be used as the corresponding facet strip titles. 
#'   If any value matches a function found in the `lubridate` package, 
#'   and that column is absent from `obs`, it will be derived from `date_col`.
#'   Default (`NULL`) produces no facetting.
#' @param facet_rows A single integer giving the number of rows to use when
#'   facetting. Default is `1`.
#' @param facet_scales A single character string controlling axis scale
#'   behaviour across facet panels: `"fixed"`, `"free"`, `"free_x"`, or
#'   `"free_y"`. Default is `"fixed"`.
#' @param add_counts Logical. If `TRUE`, the number of observations in each
#'   cell is overlaid as text. Default is `FALSE`.
#' @param colour A character string giving the colour used for tile borders.
#'   Default is `"black"`.
#' @param missing_colour A character string giving the fill colour for missing
#'   (`NA`) values. Default is `NA` (transparent).
#' @param count_colour A character string giving the colour of the observation
#'   count text when `add_counts = TRUE`. Defaults to the value of `colour`.
#' @param FUN A function used to summarise `z` values within each `x`/`y`
#'   combination. Must accept a vector as its first argument and return a
#'   single value. Default is [mean].
#' @param na.rm Logical. If `TRUE`, missing values in `z` are removed before
#'   summarising. Default is `TRUE`.
#' @param ... Additional arguments passed to `FUN()`.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @family Data Visualisation
#' @export
#'
#' @examples
#' \dontrun{
#' gg <- example_obs |>
#'   tile_plot(
#'     x = "hour",
#'     y = "day",
#'     z = "pm25_1hr",
#'     date_col = "date_local"
#'   )
#'
#' # Overlay observation counts on each cell
#' gg <- example_obs |>
#'   tile_plot(
#'     x = "hour",
#'     y = "day",
#'     z = "pm25_1hr",
#'     date_col = "date_local",
#'     add_counts = TRUE,
#'     count_colour = "white"
#'   )
#' }
tile_plot <- function(
  obs,
  x,
  y,
  z,
  z_lims = c(NA, NA),
  z_lab = z,
  date_col = NULL,
  facet_by = NULL,
  facet_rows = 1,
  facet_scales = "fixed",
  add_counts = FALSE,
  colour = "black",
  missing_colour = NA,
  count_colour = colour,
  FUN = mean,
  na.rm = TRUE,
  ...
) {
  if (is.null(names(facet_by))) {
    names(facet_by) <- facet_by
  }
  if (na.rm) {
    obs <- obs |>
      dplyr::filter(!is.na(get(z)))
  }

  pd <- obs |>
    add_features(features = c(x, y, facet_by), date_col = date_col) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(x = x, y = y, facet_by)))) |>
    dplyr::summarise(
      n = sum(!is.na(get(z))),
      z = get(z) |> FUN(),
      .groups = "drop"
    ) |>
    tidyr::complete(x, y, !!!(rlang::syms(names(facet_by)))) |>
    dplyr::mutate(dplyr::across(
      c(x, y, dplyr::all_of(names(facet_by))),
      factor
    ))

  # Drop infilled rows to allow for free facet scales if desired
  if (facet_scales %in% c("free_y", "free")) {
    pd <- pd |>
      dplyr::group_by(y, !!!(rlang::syms(names(facet_by)))) |>
      dplyr::filter(!all(is.na(z))) |>
      dplyr::ungroup()
  }
  if (facet_scales %in% c("free_x", "free")) {
    pd <- pd |>
      dplyr::group_by(x, !!!(rlang::syms(names(facet_by)))) |>
      dplyr::filter(!all(is.na(z))) |>
      dplyr::ungroup()
  }

  hour_label <- paste0("Hour (", lubridate::tz(obs[[date_col]]), ")")
  xlab <- ifelse(x == "hour", hour_label, x)
  ylab <- ifelse(y == "hour", hour_label, y)

  if (inherits(pd$z, "units")) {
    z_lab <- units::make_unit_label(z_lab, pd$z)
    pd$z <- units::drop_units(pd$z)
  }

  gg <- pd |>
    ggplot2::ggplot() |>
    add_default_theme() |>
    facet_plot(by = names(facet_by), rows = facet_rows, scales = facet_scales) +
    ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = z), colour = colour) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_fill_viridis_c(na.value = missing_colour, limits = z_lims) +
    ggplot2::labs(x = xlab, y = ylab, fill = z_lab)

  if (add_counts) {
    gg <- gg +
      ggplot2::geom_text(
        ggplot2::aes(x = x, y = y, label = n),
        colour = count_colour
      )
  }
  return(gg)
}
