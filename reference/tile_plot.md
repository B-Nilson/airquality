# Create tiled summary diagrams to assess relationships in a variable based on two grouping variables

TODO: Add description

## Usage

``` r
tile_plot(
  obs,
  x,
  y,
  z,
  date_col = "date_utc",
  facet_by = NULL,
  facet_rows = 1,
  facet_scales = "fixed",
  FUN = mean,
  ...
)
```

## Arguments

- obs:

  Observation data.frame with (at least) all columns in \`data_cols\`
  and (if provided) \`facet_by\`.

- x, y, z:

  charactor values indicating column names in \`obs\` to summarise
  (\`x\` and \`y\`) values (\`z\`) by using \`FUN\`. If \`x\` or \`y\`
  are one of \`"year", "quarter", "month", "day", "wday", "hour",
  "minute", "second"\`, and those columns are not present in \`obs\`
  then they will be calulcated based on \`date_col\`

- date_col:

  (Optional) a single charactor value indicating the column name in
  \`obs\` containing observation dates. Default assume "date_utc"
  exists.

- facet_by:

  (Optional) a character vector with 1 or 2 column names to use as
  facets in \`ggplot2::facet_wrap()\`. If names are present they will be
  used as the corresponding facet titles. Default (NULL) does not facet
  the plot.

- facet_rows:

  (Optional) a single numeric value indicating the number of rows to use
  in facetting if \`facet_by\` values provided. Default is a single row.

- facet_scales:

  (Optional) a single character value indicating wheter the facet x/y
  scales should be "fixed", "free", "free_y", or "free_x". Default is
  "fixed" (each panel with have matching x/y scales).

- FUN:

  (Optional) a function to use to summarise \`z\` values - must take a
  vector of values as it's first argument and return a single value.
  Default is to calculate the \`mean\` value.

- ...:

  Any other named arguments will be passed on to \`FUN()\` when
  summarizing \`z\` values.

## Value

A ggplot object of your tile plot.

## See also

Other Data Visualisation:
[`taylor_diagram()`](https://b-nilson.github.io/airquality/reference/taylor_diagram.md),
[`wind_rose()`](https://b-nilson.github.io/airquality/reference/wind_rose.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Make test data
date_range <- c("2019-02-01 00", "2019-03-28 23")
obs <- get_station_data("Vanderhoof, BC, Canada", date_range, sources = "BCgov")$data |>
  dplyr::select("date_utc", "site_id", "pm25_1hr_ugm3") |>
  dplyr::distinct()
# Basic usage
gg <- obs |> tile_plot(
  x = "day",
  y = "hour",
  z = "pm25_1hr_ugm3",
  facet_by = c("Year" = "year", "Month" = "month")
)
# Change titles
gg + ggplot2::labs(
  fill = "Legend Title", title = "Plot Title",
  subtitle = "Plot Subtitle", caption = "Plot Caption"
)
} # }
```
