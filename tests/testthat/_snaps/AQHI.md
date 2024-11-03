# AQHI returns expected output

    Code
      AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25, o3_1hr_ppb = obs$o3,
      no2_1hr_ppb = obs$no2, verbose = FALSE)
    Output
      # A tibble: 24 x 13
         date                 pm25    o3   no2 pm25_rolling_3hr no2_rolling_3hr
         <dttm>              <int> <int> <int>            <dbl>           <dbl>
       1 2024-01-01 00:00:00     1     1     1               NA              NA
       2 2024-01-01 01:00:00     2     2     2               NA              NA
       3 2024-01-01 02:00:00     3     3     3                2               2
       4 2024-01-01 03:00:00     4     4     4                3               3
       5 2024-01-01 04:00:00     5     5     5                4               4
       6 2024-01-01 05:00:00     6     6     6                5               5
       7 2024-01-01 06:00:00     7     7     7                6               6
       8 2024-01-01 07:00:00     8     8     8                7               7
       9 2024-01-01 08:00:00     9     9     9                8               8
      10 2024-01-01 09:00:00    10    10    10                9               9
      # i 14 more rows
      # i 7 more variables: o3_rolling_3hr <dbl>, AQHI <int>, AQHI_plus <fct>,
      #   risk <int>, high_risk_pop_message <chr>, general_pop_message <chr>,
      #   AQHI_plus_exceeds_AQHI <lgl>

---

    Code
      AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25, verbose = FALSE)
    Output
      # A tibble: 24 x 7
         pm25_1hr_ugm3 AQHI  AQHI_plus risk  high_risk_pop_message general_pop_message
                 <int> <fct> <fct>     <fct> <chr>                 <chr>              
       1             1 1     1         Low   Enjoy your usual act~ Ideal air quality ~
       2             2 1     1         Low   Enjoy your usual act~ Ideal air quality ~
       3             3 1     1         Low   Enjoy your usual act~ Ideal air quality ~
       4             4 1     1         Low   Enjoy your usual act~ Ideal air quality ~
       5             5 1     1         Low   Enjoy your usual act~ Ideal air quality ~
       6             6 1     1         Low   Enjoy your usual act~ Ideal air quality ~
       7             7 1     1         Low   Enjoy your usual act~ Ideal air quality ~
       8             8 1     1         Low   Enjoy your usual act~ Ideal air quality ~
       9             9 1     1         Low   Enjoy your usual act~ Ideal air quality ~
      10            10 1     1         Low   Enjoy your usual act~ Ideal air quality ~
      # i 14 more rows
      # i 1 more variable: AQHI_plus_exceeds_AQHI <lgl>

