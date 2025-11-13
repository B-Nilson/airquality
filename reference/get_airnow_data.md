# Download air quality station observations from the US EPA "AirNow" platform

AirNow is a US EPA nationwide voluntary program which hosts
non-validated air quality observation data from stations in the US and
many other countries globally.

The AirNow API provides access to hourly raw observation files which are
updated as data are received from the various monitoring agencies. Due
to the real-time, non-validated nature of these data great care must be
taken if using these data to support regulation, trends, guidance, or
any other government or public decision making. It is highly recommended
to seek out quality assured data where possible.

\[get_airnow_data\] provides an easy way to retrieve these observations
using AirNow's station IDs (see \[get_airnow_stations\]) and a specified
date or date range.

Due to the API's file structure, data retrieval time is proportional to
the number of hours of data desired, regardless of the number of
stations or variables requested.

## Usage

``` r
get_airnow_data(
  stations = "all",
  date_range = "now",
  variables = "all",
  raw = FALSE,
  fast = FALSE,
  quiet = FALSE
)
```

## Arguments

- stations:

  (Optional). Either "all" or a character vector specifying AQS IDs for
  stations to filter data to. If "all" not provided, data for all
  stations for each hour in \`date_range\` are still downloaded, but
  only data for desired stations is returned. Default is "all".

- date_range:

  (Optional). A datetime vector (or a character vector with dates in
  "YYYY-MM-DD HH:MM:SS" format, or "now" for current hour) with either 1
  or 2 values. Providing a single value will return data for that hour
  only, whereas two values will return data between (and including)
  those times. Dates are "backward-looking", so a value of "2019-01-01
  01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00". Default is
  "now" (the current hour).

- variables:

  (Optional). A character vector of one or more variables to try and get
  data for. All variables are downloaded regardless of this parameter,
  but only data for desired variables is returned. Default is "all",
  i.e. all available variables.

- raw:

  (Optional). A single logical (TRUE or FALSE) value indicating if raw
  data files are desired (i.e. without a standardized format). Default
  is FALSE.

- fast:

  (Optional). A single logical (TRUE or FALSE) value indicating if,
  where possible, time-intensive code should be skipped and parallel
  processing should be used. Default is FALSE.

- quiet:

  (Optional). A single logical (TRUE or FALSE) value indicating if
  non-critical messages/warnings should be silenced. Default is FALSE.

## Value

A tibble of hourly observation data for desired station(s) and date
range where available. Columns returned will vary depending on available
data from station(s). If \`raw = FALSE\` (default), the returned data
will be in a standardized format and units will be attached to each
variable.

Dates are UTC time and "backward-looking", so a value of "2019-01-01
01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00".

## See also

Other Data Collection:
[`get_abgov_data()`](https://b-nilson.github.io/airquality/reference/get_abgov_data.md),
[`get_abgov_stations()`](https://b-nilson.github.io/airquality/reference/get_abgov_stations.md),
[`get_airnow_stations()`](https://b-nilson.github.io/airquality/reference/get_airnow_stations.md),
[`get_bcgov_data()`](https://b-nilson.github.io/airquality/reference/get_bcgov_data.md),
[`get_bcgov_stations()`](https://b-nilson.github.io/airquality/reference/get_bcgov_stations.md),
[`purpleair_api()`](https://b-nilson.github.io/airquality/reference/purpleair_api.md)

## Examples

``` r
# \donttest{
# Get data for all stations for first 3 hours (UTC) of Jan 2019
get_airnow_data("all", c("2019-01-01 01:00:00", "2019-01-01 03:00:00"))
#> Data from the 'AirNow' repository are collected from the US Environmental Protection Agency and are NOT to be used commercially. Recent observations are not quality assured, and are intended for research and/or situational awareness (**NOT for regulatory decision making**). See `https://www.airnow.gov` for more information.
#> 2025-11-13 21:31:53: Getting hourly files
#> 2025-11-13 21:31:55: Getting station metadata
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `date_local = format(...)`.
#> ℹ In group 36: `tz_local = NA`.
#> Caused by warning in `with_tz.default()`:
#> ! Unrecognized time zone 'NA'
#> # A tibble: 10,129 × 22
#>    date_utc            date_local      site_id quality_assured pm25_1hr pm10_1hr
#>    <dttm>              <chr>           <chr>   <lgl>           [ug/m^3] [ug/m^3]
#>  1 2019-01-01 01:00:00 2018-12-31 21:… 000010… FALSE                 NA       NA
#>  2 2019-01-01 02:00:00 2018-12-31 22:… 000010… FALSE                 NA       NA
#>  3 2019-01-01 03:00:00 2018-12-31 23:… 000010… FALSE                 NA       NA
#>  4 2019-01-01 01:00:00 2018-12-31 21:… 000010… FALSE                  9       NA
#>  5 2019-01-01 02:00:00 2018-12-31 22:… 000010… FALSE                  9       NA
#>  6 2019-01-01 03:00:00 2018-12-31 23:… 000010… FALSE                  7       NA
#>  7 2019-01-01 01:00:00 2018-12-31 21:… 000010… FALSE                 NA       NA
#>  8 2019-01-01 02:00:00 2018-12-31 22:… 000010… FALSE                 NA       NA
#>  9 2019-01-01 03:00:00 2018-12-31 23:… 000010… FALSE                 NA       NA
#> 10 2019-01-01 01:00:00 2018-12-31 21:… 000010… FALSE                 NA       NA
#> # ℹ 10,119 more rows
#> # ℹ 16 more variables: bc_1hr [ug/m^3], o3_1hr [ppb], no_1hr [ppb],
#> #   no2_1hr [ppb], no2y_1hr [ppb], nox_1hr [ppb], noy_1hr [ppb], so2_1hr [ppb],
#> #   co_1hr [ppb], rh_1hr [%], temp_1hr [°C], wd_1hr [°], ws_1hr [m/s],
#> #   precip_1hr [mm], pressure_1hr [kPa], solar_1hr [W/m^2]

# Get data for two specific stations for first 3 hours (UTC) of Jan 2019
get_airnow_data(c("000010102", "000010401"), c("2019-01-01 01:00:00", "2019-01-01 03:00:00"))
#> Data from the 'AirNow' repository are collected from the US Environmental Protection Agency and are NOT to be used commercially. Recent observations are not quality assured, and are intended for research and/or situational awareness (**NOT for regulatory decision making**). See `https://www.airnow.gov` for more information.
#> 2025-11-13 21:31:58: Getting hourly files
#> 2025-11-13 21:32:00: Getting station metadata
#> # A tibble: 6 × 6
#>   date_utc            date_local         site_id quality_assured pm25_1hr o3_1hr
#>   <dttm>              <chr>              <chr>   <lgl>           [ug/m^3]  [ppb]
#> 1 2019-01-01 01:00:00 2018-12-31 21:30 … 000010… FALSE                 NA     35
#> 2 2019-01-01 02:00:00 2018-12-31 22:30 … 000010… FALSE                 NA     35
#> 3 2019-01-01 03:00:00 2018-12-31 23:30 … 000010… FALSE                 NA     35
#> 4 2019-01-01 01:00:00 2018-12-31 21:30 … 000010… FALSE                  9     37
#> 5 2019-01-01 02:00:00 2018-12-31 22:30 … 000010… FALSE                  9     36
#> 6 2019-01-01 03:00:00 2018-12-31 23:30 … 000010… FALSE                  7     36

# Get non-standardized data for all stations for first 3 hours (PST) of Jan 2019
date_range <- lubridate::ymd_h(c("2019-01-01 01:00:00", "2019-01-01 03:00:00"), tz = "Etc/GMT+8")
#> Warning: All formats failed to parse. No formats found.
get_airnow_data("all", date_range, raw = TRUE)
#> Data from the 'AirNow' repository are collected from the US Environmental Protection Agency and are NOT to be used commercially. Recent observations are not quality assured, and are intended for research and/or situational awareness (**NOT for regulatory decision making**). See `https://www.airnow.gov` for more information.
#> Error in if (any(date_range < within[1])) {    if (all(date_range < within[1])) {        stop("At least one date_range value must be on or after ",             format(within[1], "%F %T %Z"))    }    warning("`date_range` values must be on or after ", format(within[1],         "%F %T %Z"), ".\n ", "Set the `date_range` to a period from this date onwards to stop this warning.")    date_range[date_range < within[1]] <- within[1]}: missing value where TRUE/FALSE needed
# }
```
