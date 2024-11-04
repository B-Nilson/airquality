# TODO: add option to display n obs inside each cell
# TODO: handle named periods instead of integers for (month, quarter, wday)
# TODO: handle timezones?
#' Create tiled summary diagrams to assess relationships in a variable based on two grouping variables
#'
#' @param obs Observation data.frame with (at least) all columns in `data_cols` and (if provided) `facet_by`.
#' @param x,y,z charactor values indicating column names in `obs` to summarise (`x` and `y`) values (`z`) by using `FUN`.
#'   If `x` or `y` are one of
#'   `"year", "quarter", "month", "day", "wday", "hour", "minute", "second"`,
#'   and those columns are not present in `obs` then they will be calulcated based on `date_col`
#' @param date_col (Optional) a single charactor value indicating the column name in `obs` containing observation dates.
#'   Default assume "date_utc" exists.
#' @param facet_by (Optional) a character vector with 1 or 2 column names to use as facets in `ggplot2::facet_wrap()`.
#'   If names are present they will be used as the corresponding facet titles.
#'   Default (NULL) does not facet the plot.
#' @param facet_rows (Optional) a single numeric value indicating the number of rows to use in facetting if `facet_by` values provided.
#'   Default is a single row.
#' @param FUN (Optional) a function to use to summarise `z` values - must take a vector of values as it's first argument and return a single value.
#'   Default is to calculate the `mean` value.
#' @param ... Any other named arguments will be passed on to `FUN()` when summarizing `z` values.
#' @description
#' TODO: Add description
#' @return
#' A ggplot object of your tile plot.
#' @family Data Visualisation
#'
#' @export
#' @examples
#' \dontrun{
#' # Make test data
#' date_range <- lubridate::ymd_h(c("2019-02-01 00", "2019-03-28 23"), tz = "America/Vancouver")
#' obs <- get_station_data("Vanderhoof, BC, Canada", date_range, sources = "BCgov")$data |>
#'   dplyr::select("date_utc", "site_id", "pm25_1hr_ugm3") |>
#'   dplyr::distinct()
#' # Basic usage
#' gg <- tile_plot(obs, x = "day", y = "hour", z = "pm25_1hr_ugm3")
#' }

#' # Change titles
#' gg + ggplot2::labs(
#'   fill = "Legend Title", title = "Plot Title",
#'   subtitle = "Plot Subtitle", caption = "Plot Caption"
#' )
#'
#' # Save plot
#' # save_figure(gg, "./test.png")
#' }
tile_plot <- function(obs, x, y, z, date_col = "date_utc", facet_by = NULL, facet_rows = 1, FUN = mean, ...) {
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
  if (is.null(names(facet_by))) names(facet_by) <- facet_by
  facets_to_make <- facet_by %in% special_cases & !facet_by %in% names(obs)
  if (any(facets_to_make)) {
    lubridate_funs <- facet_by[facets_to_make] |>
      lapply(getExportedValue, ns = "lubridate")
    obs[facet_by[facets_to_make]] <- lubridate_funs |>
      lapply(\(fun) fun(obs[[date_col]]))
  }


  # Summarise using FUN(...) across each x/y pair, filling gaps with NAs
  pd <- obs |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(x = x, y = y, facet_by)))) |>
    dplyr::summarise(z = get(z) |> FUN(), .groups = "drop") |>
    tidyr::complete(x, y, !!!(rlang::syms(names(facet_by)))) |>
    dplyr::mutate(dplyr::across(
      c(x, y, dplyr::all_of(names(facet_by))),
      factor
    ))


  # Make gg tile plot with good defaults
  pd |>
    ggplot2::ggplot() |>
    facet_plot(by = names(facet_by), rows = facet_rows) +
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
