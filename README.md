
# airquality

<!-- badges: start -->
<!-- badges: end -->

The goal of airquality is to reduce the burden of gathering, standardizing, and analyzing air quality data. This package is very much a work in progress, significant changes will occur and there is no gaurentee things will work as expected. 

## Installation

You can install the development version of airquality from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("B-Nilson/airquality")
```

## Utilities 

A few useful functions for working with air quality data.

``` r 
library(airquality)

# Convert between units [under development]
convert_units(c(-20, 0, 20), in_unit = "C", out_unit = "F")
convert_units(c(0, 50, 100), c(5, 10, 25), "%", "DEW POINT")

# Error handler wrapper [under development]
#   Converts errors to messages (optional) and allows specification
#   of a return value on error instead of stopping the script 
#   useful for loading data where there is potential for missing files, corruption, etc 
data_file_paths = c(...)
lapply(data_file_paths, 
  \(fp) on_error(read.csv(fp), return = NULL))

# lookup function for location time zones [under development]
get_station_timezone(lat = 51.9357, lng = -170.3222)

```

## Data Collection

### Federal Equivalent Method (FEM) Data
Here's how you can get regulatory FEM airquality data. This data is considered the "gold standard" for realtime observations. Due to upfront and maintenance costs these stations tend to be prioritized for areas with more people.

``` r
library(airquality)

# All FEM data sources for (a) location(s)
get_station_data("Prince George, BC, Canada",
  date_range = c("2023-01-01 00", "2023-01-03 23"),
  networks = "FEM")

# AirNow (US + Partner Agencies) only for (a) specific station(s)
stations = get_airnow_stations()
get_airnow_data(stations$site_id[1],
  date_range = c("2023-01-01 00", "2023-01-03 23"))

# BC Gov. (Canada) only for (a) specific station(s)
stations = get_bcgov_stations()
get_bcgov_data(stations$site_id[1],
  date_range = c("2023-01-01 00", "2023-01-03 23"))

# AB Gov. (Canada) only for (a) specific station(s) 
stations = get_abgov_stations()
get_abgov_data(stations$site_id[1],
  date_range = c("2023-01-01 00", "2023-01-03 23"))

```

### Low-cost Monitor (LCM) Data
Here's how you can get LCM airquality data. These monitors are not as accurate as their FEM counterparts, but due to their significantly lower cost they can be installed in more locations. Caution is recommended to ensure unrealistic observations are properly flagged and removed before analysis.

``` r
library(airquality)

# All LCM networks for (a) location(s)
get_station_data(
  "Prince George, BC, Canada",
  date_range = c("2023-01-01 00", "2023-01-03 23"),
  networks = "LCM")

# PurpleAir (Global) [under development]
read_key = "YOUR-API-KEY" # see develop.purpleair.com
stations = get_purpleair_stations("Prince George, BC, Canada", api_key = read_key)
get_purpleair_data(
  stations$site_id[1],
  date_range = c("2023-01-01 00", "2023-01-03 23"),
  api_key = read_key)

# AQEgg (Global?) [to come]

# Clarity (Global?) [to come]

```

## Air Quality Standards [under development]

Air quality managers and organizations set standards that are used to assess air quality impacts in their region. Here are ways you can calculate these standards with observation data:

```r
library(airquality)

# Canadian AQHI
AQHI(pm25 = 25, o3 = 70, no2 = 20)
AQHI_plus(pm25 = 25)

# Canadian Ambient Air Quality Standards 
CAAQS(...)

# United States AQI
AQI(pm25 = 20, o3 = 70, no2 = 20)
AQI(pm25 = 25)

```

## Analysis Plots [under development]

Here are ways you can make useful analysis plots built using ggplot2:

```r
library(airquality)

# Taylor Diagrams
taylor_diagram(...)

# Wind Roses
wind_rose(...)

```

