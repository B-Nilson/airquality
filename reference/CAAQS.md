# Assess the attainment of the Canadian Ambient Air Quality Standards (CAAQS)

The Canadian Ambient Air Quality Standards (CAAQS) are part of a
collaborative national Air Quality Management System (AQMS), to better
protect human health and the environment. Standards at various averaging
periods are defined for fine particulate matter (PM2.5), ozone (O3),
nitrogen dioxide (NO2), and sulphur dioxide (SO2), and are typically
updated ever 5 years.

Management levels (Green -\> Yellow -\> Orange -\> Red) are defined for
each pollutant standard. A "Red" level indicates exceedance of the CAAQS
and management plans are typically developed for regions at "Orange" or
worse levels.

## Usage

``` r
CAAQS(
  dates,
  pm25_1hr_ugm3 = NULL,
  o3_1hr_ppb = NULL,
  no2_1hr_ppb = NULL,
  so2_1hr_ppb = NULL,
  min_completeness = 0.5
)
```

## Arguments

- dates:

  Vector of hourly datetime values corresponding to observations. Date
  gaps will be filled automatically.

- pm25_1hr_ugm3:

  (Optional). Vector of hourly mean fine particulate matter (PM2.5)
  concentrations (ug/m^3).

- o3_1hr_ppb:

  (Optional). Vector of hourly mean ozone (O3) concentrations (ppb).

- no2_1hr_ppb:

  (Optional). Vector of hourly mean nitrogen dioxide (NO2)
  concentrations (ppb).

- so2_1hr_ppb:

  (Optional). Vector of hourly mean sulphur dioxide (SO2) concentrations
  (ppb).

- min_completeness:

  A single value from 0 to 1 indicating the required annual data
  completeness for a pollutant. Default is 0.5 (50 percent).

## Value

a list of tibbles (data.frames), one tibble per pollutant provided with
annual CAAQS metrics and management levels

## References

<https://ccme.ca/en/air-quality-report>

## See also

Other Canadian Air Quality:
[`purpleair_api()`](https://b-nilson.github.io/airquality/reference/purpleair_api.md)

Other Air Quality Standards:
[`AQI()`](https://b-nilson.github.io/airquality/reference/AQI.md)

## Examples

``` r
obs <- data.frame(
  date = seq(
    lubridate::ymd_h("2020-01-01 00"),
    lubridate::ymd_h("2023-12-31 23"), "1 hours"
  ),
  pm25 = sample(1:150, 35064, TRUE), o3 = sample(1:150, 35064, TRUE),
  no2 = sample(1:150, 35064, TRUE), so2 = sample(1:150, 35064, TRUE)
)
CAAQS(
  dates = obs$date, pm25_1hr_ugm3 = obs$pm25,
  o3_1hr_ppb = obs$o3, no2_1hr_ppb = obs$no2, so2_1hr_ppb = obs$so2
)
#> $pm25
#> # A tibble: 4 × 7
#>    year perc_98_of_daily_means `3yr_mean_of_perc_98` management_level_daily
#>   <dbl>                  <dbl>                 <dbl> <chr>                 
#> 1  2020                   95.4                  NA   NA                    
#> 2  2021                   94.9                  NA   NA                    
#> 3  2022                   94.1                  94.8 Red                   
#> 4  2023                   95.6                  94.9 Red                   
#> # ℹ 3 more variables: mean_of_daily_means <dbl>, `3yr_mean_of_means` <dbl>,
#> #   management_level_annual <chr>
#> 
#> $o3
#> # A tibble: 4 × 4
#>    year fourth_highest_daily_max_8hr_mean_o3 `3yr_mean` management_level_8hr
#>   <dbl>                                <dbl>      <dbl> <chr>               
#> 1  2020                                 118.        NA  NA                  
#> 2  2021                                 116.        NA  NA                  
#> 3  2022                                 114.       116. Red                 
#> 4  2023                                 116.       115. Red                 
#> 
#> $no2
#> # A tibble: 4 × 6
#>    year annual_mean_of_hourly management_level_hourly perc_98_of_daily_maxima
#>   <dbl>                 <dbl> <chr>                                     <dbl>
#> 1  2020                  75.6 Red                                         150
#> 2  2021                  75.8 Red                                         150
#> 3  2022                  75.9 Red                                         150
#> 4  2023                  76.1 Red                                         150
#> # ℹ 2 more variables: `3yr_mean_of_perc_98` <dbl>,
#> #   management_level_annual <chr>
#> 
#> $so2
#> # A tibble: 4 × 6
#>    year annual_mean_of_hourly management_level_hourly perc_99_of_daily_maxima
#>   <dbl>                 <dbl> <chr>                                     <dbl>
#> 1  2020                  75.3 Red                                         150
#> 2  2021                  74.8 Red                                         150
#> 3  2022                  75.7 Red                                         150
#> 4  2023                  75.2 Red                                         150
#> # ℹ 2 more variables: `3yr_mean_of_perc_99` <dbl>,
#> #   management_level_annual <chr>
#> 
```
