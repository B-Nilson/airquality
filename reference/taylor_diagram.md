# Create a Taylor diagram

Visualises model performance using the geometric relationship between
correlation, standard deviation, and centred root-mean-square (RMS)
error, following Taylor (2001).

## Usage

``` r
taylor_diagram(
  dat,
  data_cols = c(obs = "obs", mod = "mod"),
  group_by,
  facet_by = NULL,
  date_col = NULL,
  facet_rows = 1,
  obs_point_options = list(colour = "purple", shape = 16, size = 2, stroke = 1, label =
    "Obs.", label_padding = labels_padding),
  mod_point_options = list(colours = NULL, fills = NULL, shapes = 21, size = 3, stroke =
    1),
  cor_line_options = list(minimum = NULL, step = 0.1, colour = "grey30", linetype =
    "longdash", label = "Correlation", label_type = "decimal"),
  rmse_line_options = list(minimum = 0, step = NULL, colour = "brown", linetype =
    "dotted", label = "Centered RMS Error", label_pos = NULL),
  sd_line_options = list(maximum = NULL, step = NULL, colour = "black", linetypes = c(obs
    = "dashed", other = "dashed"), label = "Standard Deviation"),
  plot_padding = 0.5,
  labels_padding = 0.5
)
```

## Arguments

- dat:

  A data frame containing at least the columns specified in
  \`data_cols\`, \`group_by\`, and \`facet_by\`. Must contain more than
  2 rows.

- data_cols:

  A named character vector of length 2 with names \`"obs"\` and
  \`"mod"\` specifying the column names in \`dat\` for observed and
  modelled values. Defaults to \`c(obs = "obs", mod = "mod")\`.

- group_by:

  A named character vector of 1–3 column names in \`dat\` used to
  distinguish model output. The first element maps to \`colour\`, the
  second (if present) to \`shape\`, and the third (if present) to
  \`fill\`. Element names become legend titles.

- facet_by:

  A named character vector of 1–2 column names passed to
  \[ggplot2::facet_wrap()\]. Element names become facet strip labels.
  Defaults to \`NULL\` (no faceting).

- date_col:

  A single string naming a date/time column in \`dat\` used by
  \`add_features()\` to derive additional grouping variables. Defaults
  to \`NULL\`.

- facet_rows:

  A positive integer giving the number of rows in the facet layout.
  Defaults to \`1\`.

- obs_point_options:

  A named list controlling the appearance of the observed data point.
  Accepted elements:

  \`colour\`

  :   Colour of the point. Defaults to \`"purple"\`.

  \`shape\`

  :   Shape of the point. Defaults to \`16\` (solid circle).

  \`size\`

  :   Size of the point. Defaults to \`1.5\`.

  \`stroke\`

  :   Stroke width of the point. Defaults to \`1\`.

  \`label\`

  :   Text label displayed beside the point. Defaults to \`"Obs."\`.

  \`label_padding\`

  :   Distance (in standard-deviation units) between the point and its
      label. Defaults to \`labels_padding\`.

- mod_point_options:

  A named list controlling the appearance of modelled data points.
  Accepted elements:

  \`colours\`

  :   A named character vector mapping \`group_by\[\[1\]\]\` levels to
      colours. Defaults to the \`"Dark2"\` palette from
      \[ggplot2::scale_colour_brewer()\].

  \`fills\`

  :   A named character vector mapping \`group_by\[\[3\]\]\` levels to
      fill colours (only used when \`group_by\` has three elements).
      Defaults to \[ggplot2::scale_fill_viridis_d()\].

  \`shapes\`

  :   A named integer vector mapping \`group_by\[\[2\]\]\` levels to
      point shapes. Defaults to shapes \`21\` through \`30\`.

  \`size\`

  :   Size of the points. Defaults to \`1.5\`.

  \`stroke\`

  :   Stroke width of the points. Defaults to \`1\`.

- cor_line_options:

  A named list controlling the radial correlation lines. Accepted
  elements:

  \`minimum\`

  :   Minimum correlation value shown, from -1 to 1. Defaults to the
      nearest 0.1 at or below the smallest correlation in \`dat\`, with
      a floor of \`0.5\`.

  \`step\`

  :   Spacing between correlation lines. Defaults to \`0.1\`.

  \`colour\`

  :   Line colour. Defaults to \`"grey30"\`.

  \`linetype\`

  :   Line type. Defaults to \`"longdash"\`.

  \`label\`

  :   Axis title. Defaults to \`"Correlation"\`.

  \`label_type\`

  :   Type of label to display for the axis. Options are \`"decimal"\`
      (default) or \`"percent".

- rmse_line_options:

  A named list controlling the centred-RMS-error arcs. Accepted
  elements:

  \`minimum\`

  :   Minimum RMS error arc drawn (must be \`\>= 0\`). The first arc is
      drawn one \`step\` above this value. Defaults to \`0\`.

  \`step\`

  :   Spacing between RMS error arcs. Defaults to a value producing
      approximately 4 arcs with "pretty" spacing.

  \`colour\`

  :   Arc colour. Defaults to \`"brown"\`.

  \`linetype\`

  :   Arc line type. Defaults to \`"dotted"\`.

  \`label\`

  :   Axis title. Defaults to \`"Centered RMS Error"\`.

  \`label_pos\`

  :   Position of arc labels as a proportion in \\0, 1\\: \`0\` places
      labels at the leftmost point on the x-axis, \`0.5\` at the arc
      apex, and \`1\` at the rightmost point. Defaults to 10 midpoint
      between \`minimum\` and \`1\`.

- sd_line_options:

  A named list controlling the standard-deviation arcs. Accepted
  elements:

  \`maximum\`

  :   Maximum standard deviation displayed (must be \`\>= 0\`). Defaults
      to the nearest multiple of 5 above the largest standard deviation
      in \`dat\`.

  \`step\`

  :   Spacing between standard-deviation arcs. Defaults to a value
      producing approximately 4 arcs with "pretty" spacing.

  \`colour\`

  :   Arc colour. Defaults to \`"black"\`.

  \`linetypes\`

  :   A named character vector with elements \`"obs"\` and \`"other"\`
      specifying the line type for the observed standard-deviation arc
      and all other arcs, respectively. Defaults to \`c(obs = "dashed",
      other = "dashed")\`.

  \`label\`

  :   Axis title. Defaults to \`"Standard Deviation"\`.

- plot_padding:

  A single non-negative number giving extra space (in standard-deviation
  units) added beyond the outermost arc. Increase this value if text
  labels are clipped. Defaults to \`0.5\`.

- labels_padding:

  A single non-negative number controlling the distance (in
  standard-deviation units) between grid lines or arcs and their text
  labels. Adjust to suit the figure size and number of facets. Defaults
  to \`2\`.

## Value

A \[ggplot2::ggplot()\] object.

## Details

A Taylor diagram represents three performance statistics simultaneously:

\* \*\*Standard deviation\*\*: the radial distance from the origin. \*
\*\*Correlation\*\*: the azimuthal angle from the positive x-axis. \*
\*\*Centred RMS error\*\*: the distance from the observed point on the
x-axis.

The observed point always sits on the positive x-axis at a distance
equal to the observed standard deviation. A model point that overlaps
the observed point has perfect agreement (correlation of 1, centred RMS
error of 0, and matching standard deviation).

## References

Taylor, K. E. (2001). Summarizing model performance in a single diagram.
\*Journal of Geophysical Research: Atmospheres\*, \*\*106\*\*(D7),
7183–7192.
[doi:10.1029/2000JD900719](https://doi.org/10.1029/2000JD900719)

## See also

Other Data Visualisation:
[`tile_plot()`](https://b-nilson.github.io/airquality/reference/tile_plot.md),
[`wind_rose()`](https://b-nilson.github.io/airquality/reference/wind_rose.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Prepare example data
data <- as.data.frame(datasets::ChickWeight) |>
  dplyr::filter(.data$Chick == 1) |>
  tidyr::pivot_wider(names_from = "Chick", values_from = "weight") |>
  dplyr::full_join(
    as.data.frame(datasets::ChickWeight) |>
      dplyr::filter(.data$Chick != 1)
  ) |>
  dplyr::rename(obs = `1`, mod = "weight") |>
  dplyr::mutate(Chick = factor(round(as.numeric(.data$Chick) / 20)))

# Basic usage with two grouping variables
taylor_diagram(data, group_by = c(Diet = "Diet", Chick = "Chick"))

# Extend the correlation axis to include 0 and reposition RMS error labels
taylor_diagram(
  data,
  group_by = c(Diet = "Diet", Chick = "Chick"),
  cor_line_options  = list(minimum = 0),
  rmse_line_options = list(label_pos = 0.3)
)

# Custom colours, shapes, and line styles
taylor_diagram(
  data,
  group_by  = c(Diet = "Diet", Chick = "Chick"),
  obs_point_options = list(colour = "brown", shape = 23, size = 6),
  mod_point_options = list(
    colours = c("1" = "pink", "2" = "blue", "3" = "green", "4" = "orange"),
    size  = 4,
    stroke  = 2
  ),
  cor_line_options = list(colour = "orange", linetype = "dotdash"),
  rmse_line_options = list(colour = "green",  linetype = "longdash"),
  sd_line_options = list(
    colour = "purple",
    linetypes = c(obs = "solid", other = "dashed")
  )
)

# Adjust label and plot padding
taylor_diagram(
  data,
  group_by = c(Diet = "Diet", Chick = "Chick"),
  plot_padding = 4,
  labels_padding = 1,
  rmse_line_options = list(label_pos = 0.7)
)
} # }
```
