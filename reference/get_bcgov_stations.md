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
#> # A tibble: 404 × 9
#>    site_id naps_id site_name           lat   lng  elev date_created date_removed
#>    <chr>     <int> <chr>             <dbl> <dbl> <int> <date>       <date>      
#>  1 0110000      NA Victoria Air 1     48.4 -123.     0 1976-01-01   NA          
#>  2 0110030      NA Victoria PAPS      48.4 -123.    22 1983-01-04   NA          
#>  3 0110031      NA Victoria Royal R…  48.4 -123.     1 1998-03-03   NA          
#>  4 0110063      NA Powell River Law…  49.9 -125.    20 1978-07-12   NA          
#>  5 0110101      NA Port Alice Hospi…  50.4 -127.     0 1990-01-05   NA          
#>  6 0110203      NA Gold River Pumph…  49.7 -126.     3 1977-01-02   NA          
#>  7 0110258      NA Port Alberni       49.3 -125.     0 1978-01-04   NA          
#>  8 0110501      NA Port Alice Trail…  50.4 -127.    10 1980-07-22   NA          
#>  9 0220204  102301 Powell River Cra…  49.9 -125.   209 1990-07-01   2018-02-13  
#> 10 0220205  102302 Powell River Wil…  49.9 -125.   136 1990-01-07   2019-10-16  
#> # ℹ 394 more rows
#> # ℹ 1 more variable: tz_local <chr>
# if spatial object required
get_bcgov_stations(use_sf = TRUE)
#> Simple feature collection with 404 features and 7 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -132.2875 ymin: 48.3094 xmax: -114.8877 ymax: 58.8103
#> Geodetic CRS:  WGS 84
#> # A tibble: 404 × 8
#>    site_id naps_id site_name             elev date_created date_removed tz_local
#>  * <chr>     <int> <chr>                <int> <date>       <date>       <chr>   
#>  1 0110000      NA Victoria Air 1           0 1976-01-01   NA           America…
#>  2 0110030      NA Victoria PAPS           22 1983-01-04   NA           America…
#>  3 0110031      NA Victoria Royal Roads     1 1998-03-03   NA           America…
#>  4 0110063      NA Powell River Lawn B…    20 1978-07-12   NA           America…
#>  5 0110101      NA Port Alice Hospital      0 1990-01-05   NA           America…
#>  6 0110203      NA Gold River Pumphouse     3 1977-01-02   NA           America…
#>  7 0110258      NA Port Alberni             0 1978-01-04   NA           America…
#>  8 0110501      NA Port Alice Trailer      10 1980-07-22   NA           America…
#>  9 0220204  102301 Powell River Cranbe…   209 1990-07-01   2018-02-13   America…
#> 10 0220205  102302 Powell River Wildwo…   136 1990-01-07   2019-10-16   America…
#> # ℹ 394 more rows
#> # ℹ 1 more variable: geometry <POINT [°]>
# if data for past/specific years required
get_bcgov_stations(years = 1998:2000)
#> Error in get_bcgov_stations(years = 1998:2000): unused argument (years = 1998:2000)
# }
```
