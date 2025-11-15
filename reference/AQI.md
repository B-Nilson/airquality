# Calculate the US AQI from pollutant observations

The Air Quality Index (AQI) is used for reporting on air quality in the
United States, and focuses on short term health effects as a result of
breathing polluted air.

The AQI is calculated separately for 5 pollutants: ozone (O3),
particulate matter (PM2.5 and PM10), carbon monoxide (CO), sulfur
dioxide (SO2), and nitrogen dioxide (NO2) based on maximum values (of
various averaging periods) for a particular day. The highest AQI value
among each pollutants value for that day is recorded as the AQI and the
corresponding pollutant is reported as the principal pollutant.

The US EPA has established risk categories and associated health
messaging for AQI ranges including: "Good" (0-50), "Moderate" (51-100),
"Unhealthy for Sensitive Groups" (101-150), "Unhealthy" (151-200), "Very
Unhealthy" (200-300), and "Hazardous" (301-500). AQI values above 500
are considered "Beyond the AQI", but AQI values will still be calculated
for relative comparisons.

## Usage

``` r
AQI(
  dates = Sys.time(),
  o3_8hr_ppm = NA,
  o3_1hr_ppm = NA,
  pm25_24hr_ugm3 = NA,
  pm25_1hr_ugm3 = NA,
  pm10_24hr_ugm3 = NA,
  pm10_1hr_ugm3 = NA,
  co_8hr_ppm = NA,
  co_1hr_ppm = NA,
  so2_1hr_ppb = NA,
  no2_1hr_ppb = NA
)
```

## Arguments

- dates:

  Vector of hourly dates corresponding to observations. Date gaps will
  be filled automatically.

- o3_8hr_ppm:

  (Optional) Numeric vector of hourly 8-hour mean ozone (O3)
  concentrations (ppm). Will be calculated from o3_1hr_ppm if provided
  and o3_8hr_ppm not provided.

- o3_1hr_ppm:

  (Optional) Numeric vector of hourly 1-hour mean ozone (O3)
  concentrations (ppm).

- pm25_24hr_ugm3:

  (Optional) Numeric vector of hourly 24-hour mean fine particulate
  matter (PM2.5) concentrations (ug/m^3). Will be calculated from
  pm25_1hr_ugm3 if provided and pm25_24hr_ugm3 not provided.

- pm25_1hr_ugm3:

  (Optional) Numeric vector of hourly 1-hour mean fine particulate
  matter (PM2.5) concentrations (ug/m^3).

- pm10_24hr_ugm3:

  (Optional) Numeric vector of hourly 24-hour mean coarse particulate
  matter (PM10) concentrations (ug/m^3). Will be calculated from
  pm10_1hr_ugm3 if provided and pm10_24hr_ugm3 not provided.

- pm10_1hr_ugm3:

  (Optional) Numeric vector of hourly 1-hour mean coarse particulate
  matter (PM10) concentrations (ug/m^3).

- co_8hr_ppm:

  (Optional) Numeric vector of hourly 8-hour mean carbon monoxide (CO)
  concentrations (ppm). Will be calculated from co_1hr_ppm if provided
  and co_8hr_ppm not provided.

- co_1hr_ppm:

  (Optional) Numeric vector of hourly 1-hour mean carbon monoxide (CO)
  concentrations (ppm).

- so2_1hr_ppb:

  (Optional) Numeric vector of hourly 1-hour mean sulfur dioxide (SO2)
  concentrations (ppb). 24-hour averages will be calculated
  automatically.

- no2_1hr_ppb:

  (Optional) Numeric vector of hourly 1-hour mean nitrogen dioxide (NO2)
  concentrations (ppb).

## Value

A tibble (data.frame) with columns: date, AQI, risk_category,
principal_pol and 1 row for each day between the min and max values of
the provided dates

## See also

Other Air Quality Standards:
[`CAAQS()`](https://b-nilson.github.io/airquality/reference/CAAQS.md)

## Examples

``` r
AQI(o3_8hr_ppm = 0.078, o3_1hr_ppm = 0.104, pm25_24hr_ugm3 = 35.9)
#> # A tibble: 1 × 4
#>   date                  AQI risk_category                  principal_pol
#>   <dttm>              <dbl> <fct>                          <fct>        
#> 1 2025-11-15 00:00:00   126 Unhealthy for Sensitive Groups o3           
AQI(o3_1hr_ppm = 0.104, pm25_24hr_ugm3 = 35.9)
#> # A tibble: 1 × 4
#>   date                  AQI risk_category                  principal_pol
#>   <dttm>              <dbl> <fct>                          <fct>        
#> 1 2025-11-15 00:00:00   102 Unhealthy for Sensitive Groups pm25         
```
