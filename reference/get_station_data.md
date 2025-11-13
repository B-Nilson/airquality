# Gather air quality observations from multiple networks and data sources

This is the general use function for gathering air quality observation
data in a standardized format for all desired monitoring networks and
data sources available in this package.

It either uses OpenStreetMap to look up a polygon for (a) specified
location(s) or user-provided polygons to search for monitoring stations
within a buffer distance of the polygon(s) from the specified
networks/sources. The data from those stations for the desired period
are then downloaded, standardized, combined, and returned.

Currently the following monitoring networks and data sources are
available:

FEM (Regulatory-grade "Federal Equivalent Method") monitors:

1.  AirNow (US/Global, non-validated, real-time)

2.  BCgov (B.C. (Canada), validated and non-validated, real-time)

## Usage

``` r
get_station_data(
  locations,
  date_range,
  buffer_dist = 10,
  networks = "all",
  sources = "all",
  quiet = FALSE
)
```

## Arguments

- locations:

  A character vector with at least one value that indicates a location
  on Open Street Map that data is desired for (ie. "Prince George, BC,
  Canada", or "Canada"), OR an sf object with polygon(s) indicating area
  of interest.

- date_range:

  (Optional). A datetime vector (or a character vector with dates in
  "YYYY-MM-DD HH:MM:SS" format, or "now" for current hour) with either 1
  or 2 values. Providing a single value will return data for that hour
  only, whereas two values will return data between (and including)
  those times. Dates are "backward-looking", so a value of "2019-01-01
  01:00" covers from "2019-01-01 00:01"- "2019-01-01 01:00". Default is
  "now" (the current hour).

- buffer_dist:

  (Optional) A single numeric value indicating the distance to buffer
  the station search location by (typically units of km). Default is 10.

- networks:

  (Optional) A character vector indicating which monitoring networks to
  get data for. Default is "all".

- sources:

  (Optional) A character vector indicating which data sources to get
  data from. Default is "all".

- quiet:

  (Optional). A single logical (TRUE or FALSE) value indicating if
  non-critical messages/warnings should be silenced. Default is FALSE.

## Value

A list with two elements, the first called "stations" is an sf POINT
object with all stations from the specified monitoring networks and data
sources within a specified location + buffer. The second is a tibble of
hourly observation data over the date range for those stations IDs /
networks / sources.

The columns date_utc, date_local, site_id, site_named, and
quality_assured will always be returned Observation columns will be
named "pollutant_averagingTime_unit", and will be present depending on
available data from station(s) and data sources. Some sources may have
additional columns included not found in others.

## Examples

``` r
# \donttest{
# Get data for all stations within 10 km of Fort St. John, BC
#  for the first hour of Feb 2019
get_station_data(locations = "Fort St. John, BC, Canada", date_range = "2019-02-01 01:00:00")
#> Warning: Adding a search buffer of 10 km to each location (see arg `buffer_dist`)
#> FEM - BCgov : 4 station(s) to check for data
#> Data from the 'BCgov' repository are collected from the British Columbia Ministry of Environment and Climate Change Strategy and are NOT to be used commercially. Recent observations are not quality assured, and are intended for research and/or situational awareness (**NOT for regulatory decision making**). See `https://www2.gov.bc.ca/gov/content/environment/air-land-water/air` for more information.
#> Error in standardize_data_format(dplyr::bind_rows(archived_data, realtime_data), : No data available before reformatting.
#> FEM - AirNow : 1 station(s) to check for data
#> Data from the 'AirNow' repository are collected from the US Environmental Protection Agency and are NOT to be used commercially. Recent observations are not quality assured, and are intended for research and/or situational awareness (**NOT for regulatory decision making**). See `https://www.airnow.gov` for more information.
#> 2025-11-13 21:28:09: Getting hourly files
#> 2025-11-13 21:28:09: Getting station metadata
#> Error in data_fun(stations = site_ids, date_range, quiet = quiet): No data available for desired stations during specified date range.
#> $stations
#> Simple feature collection with 5 features and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -120.8561 ymin: 56.23179 xmax: -120.8094 ymax: 56.25772
#> Geodetic CRS:  WGS 84
#> # A tibble: 5 × 5
#>   site_id       site_name               network source             geometry
#>   <chr>         <chr>                   <chr>   <chr>           <POINT [°]>
#> 1 E243516       Fort St John NP Cultur… FEM     BCgov   (-120.8489 56.2458)
#> 2 E299830       Fort St John Key Learn… FEM     BCgov  (-120.8561 56.24472)
#> 3 E304550       Fort St John 85th Aven… FEM     BCgov  (-120.8539 56.23179)
#> 4 FSJ OGC CAMEL Fort St John Hospital   FEM     BCgov  (-120.8094 56.25772)
#> 5 000105501     Fort St John NP Cultur… FEM     AirNow  (-120.8489 56.2458)
#> 
#> $data
#> # A tibble: 0 × 0
#> 

# Get data for all FEM stations within 25 km of 2 BC cities from AirNow only
#  for the first hour of Feb 2019
get_station_data(c("Vanderhoof BC, Canada", "Kamloops, BC, Canada"),
  "2019-02-01 01:00:00",
  buffer_dist = 25,
  networks = "FEM", sources = "AirNow"
)
#> No polygonal boundary for Vanderhoof BC, Canada. Returning the bounding boxes.
#> Warning: Adding a search buffer of 25 km to each location (see arg `buffer_dist`)
#> FEM - AirNow : 2 station(s) to check for data
#> Data from the 'AirNow' repository are collected from the US Environmental Protection Agency and are NOT to be used commercially. Recent observations are not quality assured, and are intended for research and/or situational awareness (**NOT for regulatory decision making**). See `https://www.airnow.gov` for more information.
#> 2025-11-13 21:28:19: Getting hourly files
#> 2025-11-13 21:28:19: Getting station metadata
#> $stations
#> Simple feature collection with 2 features and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -120.3975 ymin: 50.6747 xmax: -120.3339 ymax: 50.6978
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 × 5
#>   site_id   site_name                 network source            geometry
#>   <chr>     <chr>                     <chr>   <chr>          <POINT [°]>
#> 1 000100401 Kamloops Federal Building FEM     AirNow (-120.3339 50.6747)
#> 2 000100402 Kamloops Brocklehurst     FEM     AirNow (-120.3975 50.6978)
#> 
#> $data
#> # A tibble: 1 × 7
#>   date_utc            date_local site_id quality_assured pm25_1hr source network
#>   <dttm>              <chr>      <chr>   <lgl>           [ug/m^3] <chr>  <chr>  
#> 1 2019-02-01 01:00:00 2019-01-3… 000100… FALSE                6.8 AirNow FEM    
#> 
# }
```
