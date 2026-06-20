# Create tiled summary diagrams

Assesses relationships in a variable based on two grouping variables by
producing a tile (heatmap-style) plot that summarises a \`z\` variable
across all combinations of \`x\` and \`y\`.

## Usage

``` r
tile_plot(
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
)
```

## Arguments

- obs:

  A data frame containing at least all columns named in \`x\`, \`y\`,
  \`z\`, and (if provided) \`facet_by\`.

- x, y:

  Character strings giving the column names in \`obs\` to use as the
  horizontal and vertical axes, respectively. If either value is a
  function found in the \`lubridate\` package, and that column is absent
  from \`obs\`, it will be derived from \`date_col\`.

- z:

  A character string giving the column name in \`obs\` whose values are
  summarised by \`FUN\` for each \`x\`/\`y\` combination.

- z_lims:

  A numeric vector of length 2 giving the lower and upper limits for the
  fill colour scale. Use \`NA\` for either value to defer to the data
  range. Default is \`c(NA, NA)\`.

- z_lab:

  A character string for the fill legend label. Default is the value of
  \`z\`.

- date_col:

  A single character string giving the column name in \`obs\` containing
  observation dates. Used to derive temporal columns when \`x\` or \`y\`
  are date-part values (e.g. \`"hour"\`, \`"month"\`). Defaults to
  \`NULL\`.

- facet_by:

  A character vector of one or two column names to use as faceting
  variables passed to \[ggplot2::facet_wrap()\]. Named elements will be
  used as the corresponding facet strip titles. If any value matches a
  function found in the \`lubridate\` package, and that column is absent
  from \`obs\`, it will be derived from \`date_col\`. Default (\`NULL\`)
  produces no facetting.

- facet_rows:

  A single integer giving the number of rows to use when facetting.
  Default is \`1\`.

- facet_scales:

  A single character string controlling axis scale behaviour across
  facet panels: \`"fixed"\`, \`"free"\`, \`"free_x"\`, or \`"free_y"\`.
  Default is \`"fixed"\`.

- add_counts:

  Logical. If \`TRUE\`, the number of observations in each cell is
  overlaid as text. Default is \`FALSE\`.

- colour:

  A character string giving the colour used for tile borders. Default is
  \`"black"\`.

- missing_colour:

  A character string giving the fill colour for missing (\`NA\`) values.
  Default is \`NA\` (transparent).

- count_colour:

  A character string giving the colour of the observation count text
  when \`add_counts = TRUE\`. Defaults to the value of \`colour\`.

- FUN:

  A function used to summarise \`z\` values within each \`x\`/\`y\`
  combination. Must accept a vector as its first argument and return a
  single value. Default is \[mean\].

- na.rm:

  Logical. If \`TRUE\`, missing values in \`z\` are removed before
  summarising. Default is \`TRUE\`.

- ...:

  Additional arguments passed to \`FUN()\`.

## Value

A \[ggplot2::ggplot()\] object.

## See also

Other Data Visualisation:
[`taylor_diagram()`](https://b-nilson.github.io/airquality/reference/taylor_diagram.md),
[`wind_rose()`](https://b-nilson.github.io/airquality/reference/wind_rose.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gg <- example_obs |>
  tile_plot(
    x = "hour",
    y = "day",
    z = "pm25_1hr",
    date_col = "date_local"
  )

# Overlay observation counts on each cell
gg <- example_obs |>
  tile_plot(
    x = "hour",
    y = "day",
    z = "pm25_1hr",
    date_col = "date_local",
    add_counts = TRUE,
    count_colour = "white"
  )
} # }
```
