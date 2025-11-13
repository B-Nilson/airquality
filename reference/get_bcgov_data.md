# Download air quality station observations from the British Columbia (Canada) Government

Air pollution monitoring in Canada is done by individual
Provinces/Territories, primarily as a part of the federal National Air
Pollution Surveillance (NAPS) program. The Province of British Columbia
hosts it's hourly air quality observations through a public FTP site,
providing both historic QA/QC'ed and real-time raw data.

Annual QA/QC'ed files are available on the BC FTP site for each
monitoring station, however these are usually 1-2 years out of date due
to the QA/QC process. A single file is available for each station for
all non-QA/QC'ed years, which has potentially 0-2+ years of data
depending on the time since the last QA/QC'ed dataset was created).

\[get_bcgov_data()\] provides an easy way to retrieve these observations
using BC's station "EMS IDs" (see \[get_bcgov_stations()\]) and a
specified date or date range.

Due to the FTP site's file structure, data retrieval time is
proportional to the number of stations requested as well as the number
of years of data (PST timezone) desired. There can be potentially longer
retrieval times for non-QA/QC'ed years depending on the current time
since the last QA/QC'ed year due to larger file sizes.

## Usage

``` r
get_bcgov_data(
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

  (Optional). A character vector of one or more station IDs (BC EMS IDs)
  to try and get data desired for (see \[get_bcgov_stations()\]).
  Default is "all", i.e. all available stations.

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
data from station(s).

Dates UTC time and are "backward-looking", so a value of "2019-01-01
01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00".

## See also

\[get_bcgov_stations()\]

Other Data Collection:
[`get_abgov_data()`](https://b-nilson.github.io/airquality/reference/get_abgov_data.md),
[`get_abgov_stations()`](https://b-nilson.github.io/airquality/reference/get_abgov_stations.md),
[`get_airnow_data()`](https://b-nilson.github.io/airquality/reference/get_airnow_data.md),
[`get_airnow_stations()`](https://b-nilson.github.io/airquality/reference/get_airnow_stations.md),
[`get_bcgov_stations()`](https://b-nilson.github.io/airquality/reference/get_bcgov_stations.md),
[`purpleair_api()`](https://b-nilson.github.io/airquality/reference/purpleair_api.md)

## Examples

``` r
# \donttest{
# For a single station
station <- "0450307" # EMS IDs - see get_bcgov_stations()
# For the years 2019 and 2020
date_range <- lubridate::ymd_h(c("2019-01-01 00", "2020-12-31 23"), tz = "Etc/GMT+8")
get_bcgov_data(station, date_range)
#> Data from the 'BCgov' repository are collected from the British Columbia Ministry of Environment and Climate Change Strategy and are NOT to be used commercially. Recent observations are not quality assured, and are intended for research and/or situational awareness (**NOT for regulatory decision making**). See `https://www2.gov.bc.ca/gov/content/environment/air-land-water/air` for more information.
#> 2025-11-13 21:22:01: Getting archived data
#> # A tibble: 17,541 × 26
#>    date_utc            date_local      site_id quality_assured pm25_1hr pm10_1hr
#>    <dttm>              <chr>           <chr>   <lgl>           [ug/m^3] [ug/m^3]
#>  1 2019-01-01 09:00:00 2019-01-01 01:… 0450307 TRUE                 2.7      9.6
#>  2 2019-01-01 10:00:00 2019-01-01 02:… 0450307 TRUE                 2.4     19.4
#>  3 2019-01-01 11:00:00 2019-01-01 03:… 0450307 TRUE                 2.6      6.9
#>  4 2019-01-01 12:00:00 2019-01-01 04:… 0450307 TRUE                 2.6      6.1
#>  5 2019-01-01 13:00:00 2019-01-01 05:… 0450307 TRUE                 2.2      6  
#>  6 2019-01-01 14:00:00 2019-01-01 06:… 0450307 TRUE                 2.1      4.8
#>  7 2019-01-01 15:00:00 2019-01-01 07:… 0450307 TRUE                 3.1      6.4
#>  8 2019-01-01 16:00:00 2019-01-01 08:… 0450307 TRUE                 4        6.1
#>  9 2019-01-01 17:00:00 2019-01-01 09:… 0450307 TRUE                 3.9      4.2
#> 10 2019-01-01 18:00:00 2019-01-01 10:… 0450307 TRUE                 3.6      6.6
#> # ℹ 17,531 more rows
#> # ℹ 20 more variables: o3_1hr [ppb], no_1hr [ppb], no2_1hr [ppb],
#> #   nox_1hr [ppb], so2_1hr [ppb], trs_1hr [ppb], rh_1hr [%], temp_1hr [°C],
#> #   wd_1hr [°], wd_unitvector_1hr [°], ws_1hr [m/s], ws_vector_1hr [m/s],
#> #   pm25_1hr_instrument <fct>, pm10_1hr_instrument <fct>,
#> #   o3_1hr_instrument <fct>, no_1hr_instrument <fct>, no2_1hr_instrument <fct>,
#> #   nox_1hr_instrument <fct>, so2_1hr_instrument <fct>, …

# For multiple stations
stations <- c("0450307", "E206898") # EMS IDs - see get_bcgov_stations()
# For first week of January 2019
date_range <- lubridate::ymd_h(c("2019-01-01 00", "2019-01-07 23"), tz = "Etc/GMT+8")
get_bcgov_data(stations, date_range)
#> Data from the 'BCgov' repository are collected from the British Columbia Ministry of Environment and Climate Change Strategy and are NOT to be used commercially. Recent observations are not quality assured, and are intended for research and/or situational awareness (**NOT for regulatory decision making**). See `https://www2.gov.bc.ca/gov/content/environment/air-land-water/air` for more information.
#> 2025-11-13 21:25:58: Getting archived data
#> # A tibble: 167 × 23
#>    date_utc            date_local      site_id quality_assured pm25_1hr pm10_1hr
#>    <dttm>              <chr>           <chr>   <lgl>           [ug/m^3] [ug/m^3]
#>  1 2019-01-01 09:00:00 2019-01-01 01:… 0450307 TRUE                 2.7      9.6
#>  2 2019-01-01 10:00:00 2019-01-01 02:… 0450307 TRUE                 2.4     19.4
#>  3 2019-01-01 11:00:00 2019-01-01 03:… 0450307 TRUE                 2.6      6.9
#>  4 2019-01-01 12:00:00 2019-01-01 04:… 0450307 TRUE                 2.6      6.1
#>  5 2019-01-01 13:00:00 2019-01-01 05:… 0450307 TRUE                 2.2      6  
#>  6 2019-01-01 14:00:00 2019-01-01 06:… 0450307 TRUE                 2.1      4.8
#>  7 2019-01-01 15:00:00 2019-01-01 07:… 0450307 TRUE                 3.1      6.4
#>  8 2019-01-01 16:00:00 2019-01-01 08:… 0450307 TRUE                 4        6.1
#>  9 2019-01-01 17:00:00 2019-01-01 09:… 0450307 TRUE                 3.9      4.2
#> 10 2019-01-01 18:00:00 2019-01-01 10:… 0450307 TRUE                 3.6      6.6
#> # ℹ 157 more rows
#> # ℹ 17 more variables: o3_1hr [ppb], no_1hr [ppb], no2_1hr [ppb],
#> #   nox_1hr [ppb], so2_1hr [ppb], trs_1hr [ppb], rh_1hr [%], temp_1hr [°C],
#> #   ws_1hr [m/s], pm25_1hr_instrument <fct>, pm10_1hr_instrument <fct>,
#> #   o3_1hr_instrument <fct>, no_1hr_instrument <fct>, no2_1hr_instrument <fct>,
#> #   nox_1hr_instrument <fct>, so2_1hr_instrument <fct>,
#> #   trs_1hr_instrument <fct>
# }
```
