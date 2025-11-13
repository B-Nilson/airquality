# Download air quality station observations from the Alberta (Canada) Government

Air pollution monitoring in Canada is done by individual
Provinces/Territories, primarily as a part of the federal National Air
Pollution Surveillance (NAPS) program. The Province of Alberta hosts
it's hourly air quality observations through a public API, providing
both historic and real-time raw data.

\[get_abgov_data\] provides an easy way to retrieve these observations
using station name(s) (see \[get_abgov_stations\]) and a specified date
or date range.

## Usage

``` r
get_abgov_data(
  stations = "all",
  date_range = "now",
  variables = "all",
  raw = FALSE,
  fast = FALSE,
  quiet = FALSE,
  stations_per_call = 2,
  days_per_call = 10
)
```

## Arguments

- stations:

  (Optional) A character vector of one or more station names to try and
  get data desired for (see \[get_abgov_stations()\]). Default is "all",
  i.e. all available stations.

- date_range:

  (Optional). A datetime vector (or a character vector with dates in
  "YYYY-MM-DD HH:MM:SS" format, or "now" for current hour) with either 1
  or 2 values. Providing a single value will return data for that hour
  only, whereas two values will return data between (and including)
  those times. Dates are "backward-looking", so a value of "2019-01-01
  01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00". Default is
  "now" (the current hour).

- variables:

  (Optional) A character vector of one or more variables to try and get
  data desired.

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

- stations_per_call:

  (Optional) A single numeric value indicating the maximum number of
  stations to request per API call. The API header requires station
  names to be passed as a comma-separated list, too manyu stations may
  cause an eror depending on station name length. Default is 1.

- days_per_call:

  (Optional) A single numeric value indicating the maximum number of
  days (per station) to request per API call. This is a safety measure
  to prevent the API from timing out by requesting too many days at
  once. Default is 90.

## Value

A tibble of hourly observation data for desired station(s) and date
range where available. Columns returned will vary depending on available
data from station(s).

Dates UTC time and are "backward-looking", so a value of "2019-01-01
01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00".

## See also

\[get_abgov_stations\]

Other Data Collection:
[`get_abgov_stations()`](https://b-nilson.github.io/airquality/reference/get_abgov_stations.md),
[`get_airnow_data()`](https://b-nilson.github.io/airquality/reference/get_airnow_data.md),
[`get_airnow_stations()`](https://b-nilson.github.io/airquality/reference/get_airnow_stations.md),
[`get_bcgov_data()`](https://b-nilson.github.io/airquality/reference/get_bcgov_data.md),
[`get_bcgov_stations()`](https://b-nilson.github.io/airquality/reference/get_bcgov_stations.md),
[`purpleair_api()`](https://b-nilson.github.io/airquality/reference/purpleair_api.md)

## Examples

``` r
# \donttest{
get_abgov_data(quiet = TRUE)
#> Warning: Failed to open 'https://data.environment.alberta.ca/Services/AirQualityV2/AQHI.svc/StationMeasurements?$filter=indexof%28%27t%27,%20StationName%29%20ge%20-1%20and%20ReadingDate%20ge%20datetime%272025-11-13T14%3A00%3A00%27%20and%20ReadingDate%20le%20datetime%272025-11-13T14%3A00%3A00%27%20and%20%28indexof%28%27Fine%20Particulate%20Matter%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Ozone%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Nitric%20Oxide%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Nitrogen%20Dioxide%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Total%20Oxides%20of%20Nitrogen%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Ammonia%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Sulphur%20Dioxide%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Total%20Reduced%20Sulphur%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Hydrogen%20Sulphide%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Carbon%20Monoxide%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Methane%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Total%20Hydrocarbons%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Non-methane%20Hydrocarbons%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Relative%20Humidity%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Outdoor%20Air%20Temperature%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Wind%20Direction%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Std.%20Dev.%20of%20Wind%20Direction%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Wind%20Speed%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Solar%20Radiation%27,%20ParameterName%29%20ge%200%20or%20indexof%28%27Barometric%20Pressure%20%28non-adjusted%29%27,%20ParameterName%29%20ge%200%29%20and%20Value%20ne%20null&$select=StationName,ParameterName,ReadingDate,Value&Connection%20Timeout=36000': The requested URL returned error: 400
#> Error in standardize_data_format(dplyr::bind_rows(qaqc_data, raw_data),     date_range = date_range, known_stations = known_stations,     desired_cols = names(unlist(unname(.abgov_columns))), id_cols = "site_name",     fast = fast, raw = raw): No data available before reformatting.
# }
```
