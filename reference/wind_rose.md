# Create wind rose diagrams

Generates polar bar charts (wind roses) that summarise the joint
distribution of wind speed and direction. Bars are stacked by wind speed
category and oriented toward the compass direction from which the wind
originates.

## Usage

``` r
wind_rose(
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
  show_missing = TRUE,
  ...
)
```

## Arguments

- obs:

  A data frame with at least the columns named in \`data_cols\` and,
  when supplied, \`facet_by\`. Must contain at least one row.

- data_cols:

  A named character vector of length 2 mapping the logical roles
  \`"ws"\` (wind speed) and \`"wd"\` (wind direction) to column names in
  \`obs\`. Defaults to \`c(ws = "ws_1hr", wd = "wd_1hr")\`.

- facet_by:

  A character vector of one or two column names in \`obs\` used as
  faceting variables in \[ggplot2::facet_wrap()\]. Names, if present,
  are used as strip labels. Defaults to \`NULL\` (no faceting).

- facet_rows:

  A single positive integer giving the number of rows in the facet
  layout. Ignored when \`facet_by\` is \`NULL\`. Defaults to \`1\`.

- wd_nbins:

  Number of directional bins. Must be one of \`16\` (N, NNE, NE, ENE,
  …), \`8\` (N, NE, E, SE, …), or \`4\` (N, E, S, W). Defaults to
  \`16\`.

- freq_labels_position:

  A single numeric value in \\0, 360\\ specifying the compass bearing
  (degrees) at which the frequency-axis labels are placed. Values are
  snapped to the nearest cardinal direction. Defaults to \`NULL\`, which
  places labels at the least busy cardinal direction.

- ws_min:

  Minimum wind speed to include. Observations below this threshold are
  dropped before plotting. Units are inferred from the data or from
  \`ws_out_units\`. Defaults to \`0\`.

- ws_step:

  Width of each wind speed bin in the units given by \`ws_out_units\`.
  Defaults to \`2\`.

- ws_out_units:

  A single character string specifying the output units for wind speed
  (e.g. \`"m/s"\`, \`"km/h"\`, \`"mph"\`). When the wind speed column
  does not carry unit metadata it is assumed to be in m/s and converted
  as needed. Defaults to \`"m/s"\`.

- fills:

  A character vector of colours for the wind speed fill scale. Should
  have at least as many values as there are wind speed bins. Pass
  \`"default"\` to use \[ggplot2::scale_fill_viridis_d()\]. Defaults to
  \`"default"\`.

- colour:

  A single character string giving the colour used for bar outlines.
  Defaults to \`"black"\`.

- alpha:

  A single numeric value in \\0, 1\\ controlling the opacity of the bar
  fills. \`0\` is fully transparent; \`1\` is fully opaque. Defaults to
  \`0.8\`.

- bar_width:

  A single numeric value in \\0, 1\\ controlling the relative width of
  each directional bar. A value of \`1\` makes adjacent bars touch.
  Defaults to \`1\`.

- show_missing:

  A single logical value indicating whether to display a text label of
  the proportion of observations missing a wind direction or wind speed.
  Defaults to \`TRUE\`.

- ...:

  Additional arguments passed to \[ggplot2::geom_col()\], such as
  \`linewidth\` or \`linetype\`.

## Value

A \[ggplot2::ggplot()\] object.

## See also

Other Data Visualisation:
[`taylor_diagram()`](https://b-nilson.github.io/airquality/reference/taylor_diagram.md),
[`tile_plot()`](https://b-nilson.github.io/airquality/reference/tile_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# Basic usage
wind_rose(example_obs)

# Coarser 8-point rose with output in km/h
wind_rose(example_obs, wd_nbins = 8, ws_out_units = "km/h", ws_step = 10)
} # }
```
