# Download air quality station metadata from the Alberta (Canada) Government

Air pollution monitoring in Canada is done by individual
Provinces/Territories, primarily as a part of the federal National Air
Pollution Surveillance (NAPS) program. The Province of Alberta hosts
it's hourly air quality metadata through a public API.

\[get_abgov_stations\] provides an easy way to retrieve this metadata
(typically to determine station names to pass to \[get_abgov_data\])

## Usage

``` r
get_abgov_stations(..., use_sf = FALSE, quiet = FALSE)
```

## Arguments

- ...:

  Not used. For compatibility with other metadata functions and future
  expansion.

- use_sf:

  (Optional) a single logical (TRUE/FALSE) value indicating whether or
  not to return a spatial object. using the \`sf\` package

- quiet:

  (Optional). A single logical (TRUE or FALSE) value indicating if
  non-critical messages/warnings should be silenced. Default is FALSE.

## Value

A tibble of metadata for Alberta air quality monitoring stations.

## See also

\[get_abgov_data\]

Other Data Collection:
[`get_abgov_data()`](https://b-nilson.github.io/airquality/reference/get_abgov_data.md),
[`get_airnow_data()`](https://b-nilson.github.io/airquality/reference/get_airnow_data.md),
[`get_airnow_stations()`](https://b-nilson.github.io/airquality/reference/get_airnow_stations.md),
[`get_bcgov_data()`](https://b-nilson.github.io/airquality/reference/get_bcgov_data.md),
[`get_bcgov_stations()`](https://b-nilson.github.io/airquality/reference/get_bcgov_stations.md),
[`purpleair_api()`](https://b-nilson.github.io/airquality/reference/purpleair_api.md)

## Examples

``` r
# \donttest{
# Normal usage
get_abgov_stations()
#> # A tibble: 93 × 11
#>    site_id site_name   type  description operated_by address airshed   lat   lng
#>    <chr>   <chr>       <chr> <chr>       <chr>       <chr>   <chr>   <dbl> <dbl>
#>  1 01AQY   Leduc Sens… Pilo…  NA         NA          NA      Albert…  53.2 -114.
#>  2 01CAP   Woodcroft   Oper… "Station o… http://cap… NA      Capita…  53.6 -114.
#>  3 01CRAZ  Okotoks Wy… Port…  NA         https://cr… NA      Calgar…  50.7 -114.
#>  4 01LICA  Cold Lake … Oper…  NA         http://www… NA      Lakela…  54.4 -110.
#>  5 01PAML  Jasper      Port… "EPA porta… NA          NA      West C…  52.9 -118.
#>  6 01PASZA Grande Pra… Oper…  NA         http://www… NA      Peace …  55.2 -119.
#>  7 01SIA   Sherwood P… Oper…  NA         http://cap… NA      Strath…  53.5 -113.
#>  8 01WBEA  Fort McKay  Oper… "Fort McKa… http://www… Near t… Wood B…  57.2 -112.
#>  9 02AQM   Calgary So… Oper…  NA         http://www… 49th A… Calgar…  51.0 -114.
#> 10 02CAP   O’Morrow S… Oper… "Not an AQ… https://ca… NA      Capita…  54.9 -113.
#> # ℹ 83 more rows
#> # ℹ 2 more variables: elev <dbl>, tz_local <chr>
# if spatial object required
get_abgov_stations(use_sf = TRUE)
#> Simple feature collection with 93 features and 9 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -119.3968 ymin: 49.46218 xmax: -110.2331 ymax: 58.7084
#> Geodetic CRS:  WGS 84
#> # A tibble: 93 × 10
#>    site_id site_name         type  description operated_by address airshed  elev
#>  * <chr>   <chr>             <chr> <chr>       <chr>       <chr>   <chr>   <dbl>
#>  1 01AQY   Leduc Sensor      Pilo…  NA         NA          NA      Albert…    NA
#>  2 01CAP   Woodcroft         Oper… "Station o… http://cap… NA      Capita…   673
#>  3 01CRAZ  Okotoks Wylie     Port…  NA         https://cr… NA      Calgar…    NA
#>  4 01LICA  Cold Lake South   Oper…  NA         http://www… NA      Lakela…    NA
#>  5 01PAML  Jasper            Port… "EPA porta… NA          NA      West C…    NA
#>  6 01PASZA Grande Prairie -… Oper…  NA         http://www… NA      Peace …    NA
#>  7 01SIA   Sherwood Park     Oper…  NA         http://cap… NA      Strath…   709
#>  8 01WBEA  Fort McKay        Oper… "Fort McKa… http://www… Near t… Wood B…    NA
#>  9 02AQM   Calgary Southeast Oper…  NA         http://www… 49th A… Calgar…  1030
#> 10 02CAP   O’Morrow Station… Oper… "Not an AQ… https://ca… NA      Capita…   571
#> # ℹ 83 more rows
#> # ℹ 2 more variables: tz_local <chr>, geometry <POINT [°]>
# }
```
