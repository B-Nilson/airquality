# Download air quality station metadata from the British Columbia (Canada) Government

Air pollution monitoring in Canada is done by individual
Provinces/Territories, primarily as a part of the federal National Air
Pollution Surveillance (NAPS) program. The Province of British Columbia
hosts it's air quality metadata through a public FTP site.

\[get_bcgov_stations\] provides an easy way to retrieve this metadata
(typically to determine station id's to pass to \[get_bcgov_data\])

## Usage

``` r
get_bcgov_stations(date_range = "now", use_sf = FALSE, quiet = FALSE)
```

## Arguments

- date_range:

  (Optional). A datetime vector (or a character vector with dates in
  "YYYY-MM-DD HH:MM:SS" format, or "now" for current hour) with either 1
  or 2 values. Providing a single value will return data for that hour
  only, whereas two values will return data between (and including)
  those times. Dates are "backward-looking", so a value of "2019-01-01
  01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00". Default is
  "now" (the current hour).

- use_sf:

  (Optional) a single logical (TRUE/FALSE) value indicating whether or
  not to return a spatial object. using the \`sf\` package

- quiet:

  (Optional). A single logical (TRUE or FALSE) value indicating if
  non-critical messages/warnings should be silenced. Default is FALSE.

## Value

A tibble of metadata for British Columbia air quality monitoring
stations.

## See also

\[get_bcgov_data\]

Other Data Collection:
[`get_abgov_data()`](https://b-nilson.github.io/airquality/reference/get_abgov_data.md),
[`get_abgov_stations()`](https://b-nilson.github.io/airquality/reference/get_abgov_stations.md),
[`get_airnow_data()`](https://b-nilson.github.io/airquality/reference/get_airnow_data.md),
[`get_airnow_stations()`](https://b-nilson.github.io/airquality/reference/get_airnow_stations.md),
[`get_bcgov_data()`](https://b-nilson.github.io/airquality/reference/get_bcgov_data.md),
[`purpleair_api()`](https://b-nilson.github.io/airquality/reference/purpleair_api.md)

## Examples

``` r
# \donttest{
# Normal usage
get_bcgov_stations()
#> Warning: URL 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR//AnnualSummary/': Timeout of 60 seconds was reached
#> Error in file(con, "r"): cannot open the connection to 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR//AnnualSummary/'
# if spatial object required
get_bcgov_stations(use_sf = TRUE)
#> Warning: URL 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR//AnnualSummary/': Timeout of 60 seconds was reached
#> Error in file(con, "r"): cannot open the connection to 'ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR//AnnualSummary/'
# if data for past/specific years required
get_bcgov_stations(years = 1998:2000)
#> Error in get_bcgov_stations(years = 1998:2000): unused argument (years = 1998:2000)
# }
```
