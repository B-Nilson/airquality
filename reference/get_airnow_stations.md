# Download air quality station metadata from the US EPA "AirNow" platform

AirNow is a US EPA nationwide voluntary program which hosts
non-validated air quality observation data from stations in the US and
many other countries globally.

The AirNow API provides access to daily metadata files for the available
stations at that time.

\[get_airnow_stations\] provides an easy way to retrieve this metadata
(typically to determine station id's to pass to \[get_airnow_data\])

## Usage

``` r
get_airnow_stations(
  date_range = "now",
  time_step = "1 days",
  use_sf = FALSE,
  quiet = FALSE
)
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

- time_step:

  (Optional). A character string specifying the time step to check data
  for within \`date_range\`. Default is "1 days" (i.e. check every day
  within \`date_range\` - may be slow for long date ranges).

- use_sf:

  (Optional) a single logical (TRUE/FALSE) value indicating whether or
  not to return a spatial object. using the \`sf\` package

- quiet:

  (Optional). A single logical (TRUE or FALSE) value indicating if
  non-critical messages/warnings should be silenced. Default is FALSE.

## Value

A tibble of metadata for the air quality monitoring stations on AirNow.

## See also

\[get_airnow_data\]

Other Data Collection:
[`get_abgov_data()`](https://b-nilson.github.io/airquality/reference/get_abgov_data.md),
[`get_abgov_stations()`](https://b-nilson.github.io/airquality/reference/get_abgov_stations.md),
[`get_airnow_data()`](https://b-nilson.github.io/airquality/reference/get_airnow_data.md),
[`get_bcgov_data()`](https://b-nilson.github.io/airquality/reference/get_bcgov_data.md),
[`get_bcgov_stations()`](https://b-nilson.github.io/airquality/reference/get_bcgov_stations.md),
[`purpleair_api()`](https://b-nilson.github.io/airquality/reference/purpleair_api.md)

## Examples

``` r
# \donttest{
# Normal usage
get_airnow_stations()
#> # A tibble: 5,167 × 11
#>    site_id    site_name city    lat    lng  elev status operator tz_offset as_of
#>    <chr>      <chr>     <chr> <dbl>  <dbl> <dbl> <chr>  <chr>        <dbl> <chr>
#>  1 560350700  Big Piney SUBL…  42.5 -110.  2089. Active Wyoming…        -7 2026…
#>  2 360130006  Dunkirk   CHAU…  42.5  -79.3    0  Active New Yor…        -5 2026…
#>  3 550590019  Chiwaukee KENO…  42.5  -87.8    0  Active Wiscons…        -6 2026…
#>  4 160291009  Soda Spr… CARI…  42.7 -112.     0  Inact… Idaho D…        -7 2026…
#>  5 001581010  JPN Site… NA     42.7  142.     0  Active Japan M…         9 2026…
#>  6 840311079… Santee S… KNOX   42.7  -97.9  386. Active EPA Off…        -6 2026…
#>  7 260650018  Lansing   INGH…  42.8  -84.6    0  Active Michiga…        -5 2026…
#>  8 560250100  Casper G… NATR…  42.8 -106.  1592. Active Wyoming…        -7 2026…
#>  9 330150018  Londonde… ROCK…  42.9  -71.4  124. Active New Ham…        -5 2026…
#> 10 840271153… Lake Lena PINE   46.0  -92.5    0  Active Minneso…        -6 2026…
#> # ℹ 5,157 more rows
#> # ℹ 1 more variable: tz_local <chr>
# if spatial object required
get_airnow_stations(use_sf = TRUE)
#> Simple feature collection with 5167 features and 9 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -161.767 ymin: -34.5766 xmax: 144.36 ymax: 70.1319
#> Geodetic CRS:  WGS 84
#> # A tibble: 5,167 × 10
#>    site_id      site_name   city   elev status operator tz_offset as_of tz_local
#>  * <chr>        <chr>       <chr> <dbl> <chr>  <chr>        <dbl> <chr> <chr>   
#>  1 560350700    Big Piney   SUBL… 2089. Active Wyoming…        -7 2026… America…
#>  2 360130006    Dunkirk     CHAU…    0  Active New Yor…        -5 2026… America…
#>  3 550590019    Chiwaukee   KENO…    0  Active Wiscons…        -6 2026… America…
#>  4 160291009    Soda Sprin… CARI…    0  Inact… Idaho D…        -7 2026… America…
#>  5 001581010    JPN Site 0… NA       0  Active Japan M…         9 2026… Asia/To…
#>  6 840311079992 Santee Sio… KNOX   386. Active EPA Off…        -6 2026… America…
#>  7 260650018    Lansing     INGH…    0  Active Michiga…        -5 2026… America…
#>  8 560250100    Casper Gas… NATR… 1592. Active Wyoming…        -7 2026… America…
#>  9 330150018    Londonderr… ROCK…  124. Active New Ham…        -5 2026… America…
#> 10 840271153061 Lake Lena   PINE     0  Active Minneso…        -6 2026… America…
#> # ℹ 5,157 more rows
#> # ℹ 1 more variable: geometry <POINT [°]>
# if data for some time in the past required
get_airnow_stations(date_range = lubridate::ymd("2022-01-01"))
#> # A tibble: 4,746 × 11
#>    site_id   site_name   city    lat   lng  elev status operator tz_offset as_of
#>    <chr>     <chr>       <chr> <dbl> <dbl> <dbl> <chr>  <chr>        <dbl> <chr>
#>  1 017203140 JPN Site 0… NA     36.4  136.     0 Active Japan M…         9 2022…
#>  2 019321030 JPN Site 0… NA     35.6  139.     0 Active Japan M…         9 2022…
#>  3 020204510 JPN Site 0… NA     36.1  138.     0 Active Japan M…         9 2022…
#>  4 022202150 JPN Site 0… NA     34.7  138.     0 Active Japan M…         9 2022…
#>  5 022447010 JPN Site 0… NA     34.7  138.     0 Active Japan M…         9 2022…
#>  6 028202530 JPN Site 0… NA     34.8  135.     0 Active Japan M…         9 2022…
#>  7 028214510 JPN Site 0… NA     34.8  135.     0 Active Japan M…         9 2022…
#>  8 030205020 JPN Site 0… NA     33.9  135.     0 Active Japan M…         9 2022…
#>  9 033202070 JPN Site 0… NA     34.5  134.     0 Active Japan M…         9 2022…
#> 10 035205030 JPN Site 0… NA     34.0  132.     0 Active Japan M…         9 2022…
#> # ℹ 4,736 more rows
#> # ℹ 1 more variable: tz_local <chr>
# Or a range of time
get_airnow_stations(date_range = lubridate::ymd(c("2022-01-01","2022-01-05")))
#> # A tibble: 4,751 × 11
#>    site_id    site_name city    lat    lng  elev status operator tz_offset as_of
#>    <chr>      <chr>     <chr> <dbl>  <dbl> <dbl> <chr>  <chr>        <dbl> <chr>
#>  1 80011G002  Estación… GUAN…  20.5 -101.   477. Active Secreta…        -6 2022…
#>  2 800140009  Loma Dor… JALI…  20.6 -103.   502. Active Sistema…        -6 2022…
#>  3 80011G018  Estación… GUAN…  21.0 -101.   610  Active Secreta…        -6 2022…
#>  4 150031001  Honolulu  HONO…  21.3 -158.     0  Active Hawaii …       -10 2022…
#>  5 682RIY010… Riyadh    NA     24.7   46.6    0  Active U.S. De…         3 2022…
#>  6 484391062  Kennedal… TARR…  32.7  -97.2  195. Active Texas C…         0 2022…
#>  7 482030002  Karnack … HARR…  32.7  -94.2    0  Active Texas C…        -6 2022…
#>  8 060732000  Manzanit… SAN …  32.7 -116.     0  Active Manzani…        -8 2022…
#>  9 061000000  NA        NA     32.7 -116.     0  Active Manzani…        -8 2022…
#> 10 043201520  JPN Site… NA     32.8  131.     0  Active Japan M…         9 2022…
#> # ℹ 4,741 more rows
#> # ℹ 1 more variable: tz_local <chr>
# }
```
