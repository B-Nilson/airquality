# Create wind rose diagrams to assess patterns in wind speed and direction

TODO: Add description

## Usage

``` r
wind_rose(
  obs,
  data_cols = c(ws = "ws_1hr", wd = "wd_1hr"),
  facet_by = NULL,
  facet_rows = 1,
  wd_nbins = c(16, 8, 4)[1],
  ws_min = 0,
  ws_step = 2,
  fills = "default",
  colour = "black",
  alpha = 0.8,
  bar_width = 1,
  ...
)
```

## Arguments

- obs:

  Observation data.frame with (at least) all columns in \`data_cols\`
  and (if provided) \`facet_by\`.

- data_cols:

  (Optional) a character vector with 2 values indicating column names in
  \`obs\` to get wind speed (ws) and wind direction (wd) values. Default
  assumes columns "ws_1hr" and "wd_1hr" exist.

- facet_by:

  (Optional) a character vector with 1 or 2 column names to use as
  facets in \`ggplot2::facet_wrap()\`. If names are present they will be
  used as the corresponding facet titles. Default (NULL) does not facet
  the plot.

- facet_rows:

  (Optional) a single numeric value indicating the number of rows to use
  in facetting if \`facet_by\` values provided. Default is a single row.

- wd_nbins:

  (Optional) a single value indicating the number of wind direction bars
  to plot. Must be either 16 (N, NNE, NE, ENE, ...), 8 (N, NE, E, SE,
  ...), or 4 (N, E, S, W). Default is 16.

- ws_min:

  (Optional) a single value indicating the minimum wind speed value to
  include in the diagram. Default is 0 m/s.

- ws_step:

  (Optional) a single value indicating the spacing between each wind
  speed category. Default is a step of 2 m/s.

- fills:

  (Optional) a vector of colours to use for the wind speed breaks
  Default uses "good looking" colours

- colour:

  (Optional) a single character value indicating the colour to use for
  the . Default is a step of 2 m/s.

- alpha:

  (Optional) a single value from 0-1 indicating the alpha (opacity) of
  the wind speed fill colours. Default is a step of 2 m/s.

- bar_width:

  (Optional) a single value from 0-1 indicating the relative width of
  the observation data bars. Default is full width (=1).

- ...:

  Any other named argument will be passed on to ggplot2::geom_col() when
  drawing the observation data (See ?ggplot2::geom_col for possible
  arguments)

## Value

A ggplot object of your wind rose.

## See also

Other Data Visualisation:
[`taylor_diagram()`](https://b-nilson.github.io/airquality/reference/taylor_diagram.md),
[`tile_plot()`](https://b-nilson.github.io/airquality/reference/tile_plot.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Make test data
date_range <- lubridate::ymd_h(c("2019-02-01 00", "2019-02-28 23"), tz = "America/Vancouver")
obs <- get_station_data("Vanderhoof, BC, Canada", date_range, sources = "BCgov")$data |>
  dplyr::select("date_local", "site_id", "ws_1hr_ms", "wd_1hr_degrees") |>
  dplyr::distinct()
# Basic usage
gg <- wind_rose(obs, facet_by = c(Site = "site_id"))
# Change titles
gg + ggplot2::labs(
  fill = "Legend Title", title = "Plot Title",
  subtitle = "Plot Subtitle", caption = "Plot Caption"
)
} # }
```
