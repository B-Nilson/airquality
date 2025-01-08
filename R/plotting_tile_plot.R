# TODO: add option to display n obs inside each cell
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
#' @param facet_scales (Optional) a single character value indicating wheter the facet x/y scales should be "fixed", "free", "free_y", or "free_x".
#'   Default is "fixed" (each panel with have matching x/y scales).
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
#' date_range <- c("2019-02-01 00", "2019-03-28 23")
#' obs <- get_station_data("Vanderhoof, BC, Canada", date_range, sources = "BCgov")$data |>
#'   dplyr::select("date_utc", "site_id", "pm25_1hr_ugm3") |>
#'   dplyr::distinct()
#' # Basic usage
#' gg <- obs |> tile_plot(
#'   x = "day",
#'   y = "hour",
#'   z = "pm25_1hr_ugm3",
#'   facet_by = c("Year" = "year", "Month" = "month")
#' )
#' # Change titles
#' gg + ggplot2::labs(
#'   fill = "Legend Title", title = "Plot Title",
#'   subtitle = "Plot Subtitle", caption = "Plot Caption"
#' )
#'
#' # Save plot
#' # save_figure(gg, "./test.png")
#' }
tile_plot <- function(obs, x, y, z, date_col = "date_utc", facet_by = NULL, facet_rows = 1, facet_scales = "fixed", FUN = mean, ...) {
  if (is.null(names(facet_by))) names(facet_by) <- facet_by

  # Handle date-based grouping options
  #   (i.e. add "year" column if year provided but not present in obs)
  obs <- obs |>
    add_lubridate_cols(c(x, y, facet_by), date_col)

  # Summarise using FUN(...) across each x/y pair, filling gaps with NAs
  pd <- obs |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(x = x, y = y, facet_by)))) |>
    dplyr::summarise(z = get(z) |> FUN(), .groups = "drop") |>
    tidyr::complete(x, y, !!!(rlang::syms(names(facet_by)))) |>
    dplyr::mutate(dplyr::across(
      c(x, y, dplyr::all_of(names(facet_by))),
      factor
    ))

  # Drop infilled rows to allow for free facet scales if desired
  if(facet_scales %in% c("free_y", "free")){
    pd = pd |>
      dplyr::group_by(y, !!!(rlang::syms(names(facet_by)))) |>
      dplyr::filter(!all(is.na(z))) |>
      dplyr::ungroup()
  }
  if(facet_scales %in% c("free_x", "free")){
    pd = pd |>
      dplyr::group_by(x, !!!(rlang::syms(names(facet_by)))) |>
      dplyr::filter(!all(is.na(z))) |>
      dplyr::ungroup()
  }

  hour_label <- paste0("hour (", lubridate::tz(obs[[date_col]]), ")")
  xlab <- ifelse(x == "hour", hour_label, x)
  ylab <- ifelse(y == "hour", hour_label, y)

  # Make gg tile plot with good defaults
  pd |>
    ggplot2::ggplot() |>
    add_default_theme() |>
    facet_plot(by = names(facet_by), rows = facet_rows, scales = facet_scales) +
    ggplot2::geom_tile(
      ggplot2::aes(x, y, fill = z),
      colour = "black"
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_fill_viridis_c(na.value = NA, limits = c(0, NA)) +
    ggplot2::labs(
      x = xlab, y = ylab,
      fill = z
    )
}

add_lubridate_cols <- function(obs, FUN_names, date_col = "date_utc") {
  special_cases <- c(
    "year", "quarter", "month", "day", "wday",
    "hour", "minute", "second"
  )
  cols_to_make <- FUN_names %in% special_cases &
    !FUN_names %in% names(obs)
  for (col in FUN_names[cols_to_make]) {
    lubridate_fun <- getExportedValue("lubridate", col)
    if (col %in% c("month", "wday")) { # Use abreviated text labels where available
      obs[[col]] <- obs[[date_col]] |> lubridate_fun(label = TRUE, abbr = TRUE)
      col_levels <- levels(obs[[col]])[levels(obs[[col]]) %in% obs[[col]]]
      obs[[col]] <- obs[[col]] |> factor(col_levels)
    } else {
      obs[[col]] <- obs[[date_col]] |> lubridate_fun()
    }
  }
  return(obs)
}
