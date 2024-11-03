# CAAQS returns expected output

    Code
      output
    Output
      $pm25
      # A tibble: 4 x 7
         year perc_98_of_daily_means `3yr_mean_of_perc_98` management_level_daily
        <dbl>                  <dbl>                 <dbl> <chr>                 
      1  2020                    100                    NA <NA>                  
      2  2021                    100                    NA <NA>                  
      3  2022                    100                   100 Red                   
      4  2023                    100                   100 Red                   
      # i 3 more variables: mean_of_daily_means <dbl>, `3yr_mean_of_means` <dbl>,
      #   management_level_annual <chr>
      
      $o3
      # A tibble: 4 x 4
         year fourth_highest_daily_max_8hr_mean_o3 `3yr_mean` management_level_8hr
        <dbl>                                <dbl>      <dbl> <chr>               
      1  2020                                   15         NA <NA>                
      2  2021                                   15         NA <NA>                
      3  2022                                   15         15 Green               
      4  2023                                   15         15 Green               
      
      $no2
      # A tibble: 4 x 6
         year annual_mean_of_hourly management_level_hourly perc_98_of_daily_maxima
        <dbl>                 <dbl> <chr>                                     <dbl>
      1  2020                     1 Green                                         1
      2  2021                     1 Green                                         1
      3  2022                     1 Green                                         1
      4  2023                     1 Green                                         1
      # i 2 more variables: `3yr_mean_of_perc_98` <dbl>,
      #   management_level_annual <chr>
      
      $so2
      # A tibble: 4 x 6
         year annual_mean_of_hourly management_level_hourly perc_99_of_daily_maxima
        <dbl>                 <dbl> <chr>                                     <dbl>
      1  2020                    30 Green                                        30
      2  2021                    30 Green                                        30
      3  2022                    30 Green                                        30
      4  2023                    30 Green                                        30
      # i 2 more variables: `3yr_mean_of_perc_99` <dbl>,
      #   management_level_annual <chr>
      

