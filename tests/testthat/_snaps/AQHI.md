# AQHI returns expected output

    Code
      AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25, o3_1hr_ppb = obs$o3,
      no2_1hr_ppb = obs$no2, verbose = FALSE)
    Output
      # A tibble: 24 x 13
         date                 pm25    o3   no2 pm25_rolling_3hr no2_rolling_3hr
         <dttm>              <int> <int> <int>            <dbl>           <dbl>
       1 2024-01-01 00:00:00    91    12     2             NA              NA  
       2 2024-01-01 01:00:00    27   115   108             NA              NA  
       3 2024-01-01 02:00:00    51   139    95             56.3            68.3
       4 2024-01-01 03:00:00    59    71   102             45.7           102. 
       5 2024-01-01 04:00:00   103    89   145             71             114  
       6 2024-01-01 05:00:00    61   112    94             74.3           114. 
       7 2024-01-01 06:00:00   135   117   143             99.7           127. 
       8 2024-01-01 07:00:00     1    32    83             65.7           107. 
       9 2024-01-01 08:00:00   120   125    79             85.3           102. 
      10 2024-01-01 09:00:00    49    27    25             56.7            62.3
      # i 14 more rows
      # i 7 more variables: o3_rolling_3hr <dbl>, AQHI <int>, AQHI_plus <fct>,
      #   risk <int>, high_risk_pop_message <chr>, general_pop_message <chr>,
      #   AQHI_plus_exceeds_AQHI <lgl>

---

    Code
      AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25, verbose = FALSE)
    Output
         pm25_1hr_ugm3 AQHI AQHI_plus      risk
      1             91   10        10      High
      2             27    3         3       Low
      3             51    6         6  Moderate
      4             59    6         6  Moderate
      5            103    +         + Very High
      6             61    7         7      High
      7            135    +         + Very High
      8              1    1         1       Low
      9            120    +         + Very High
      10            49    5         5  Moderate
      11            95   10        10      High
      12            86    9         9      High
      13           125    +         + Very High
      14           101    +         + Very High
      15            74    8         8      High
      16            13    2         2       Low
      17             9    1         1       Low
      18           119    +         + Very High
      19           106    +         + Very High
      20            55    6         6  Moderate
      21           128    +         + Very High
      22            63    7         7      High
      23            46    5         5  Moderate
      24             4    1         1       Low
                                                                     high_risk_pop_message
      1                                          Reduce or reschedule activities outdoors.
      2                                                       Enjoy your usual activities.
      3  Consider reducing or rescheduling activities outdoors if you experience symptoms.
      4  Consider reducing or rescheduling activities outdoors if you experience symptoms.
      5                                                 Avoid strenuous activity outdoors.
      6                                          Reduce or reschedule activities outdoors.
      7                                                 Avoid strenuous activity outdoors.
      8                                                       Enjoy your usual activities.
      9                                                 Avoid strenuous activity outdoors.
      10 Consider reducing or rescheduling activities outdoors if you experience symptoms.
      11                                         Reduce or reschedule activities outdoors.
      12                                         Reduce or reschedule activities outdoors.
      13                                                Avoid strenuous activity outdoors.
      14                                                Avoid strenuous activity outdoors.
      15                                         Reduce or reschedule activities outdoors.
      16                                                      Enjoy your usual activities.
      17                                                      Enjoy your usual activities.
      18                                                Avoid strenuous activity outdoors.
      19                                                Avoid strenuous activity outdoors.
      20 Consider reducing or rescheduling activities outdoors if you experience symptoms.
      21                                                Avoid strenuous activity outdoors.
      22                                         Reduce or reschedule activities outdoors.
      23 Consider reducing or rescheduling activities outdoors if you experience symptoms.
      24                                                      Enjoy your usual activities.
                                                                       general_pop_message
      1  Consider reducing or rescheduling activities outdoors if you experience symptoms.
      2                                          Ideal air quality for outdoor activities.
      3            No need to modify your usual activities unless you experience symptoms.
      4            No need to modify your usual activities unless you experience symptoms.
      5   Reduce or reschedule activities outdoors, especially if you experience symptoms.
      6  Consider reducing or rescheduling activities outdoors if you experience symptoms.
      7   Reduce or reschedule activities outdoors, especially if you experience symptoms.
      8                                          Ideal air quality for outdoor activities.
      9   Reduce or reschedule activities outdoors, especially if you experience symptoms.
      10           No need to modify your usual activities unless you experience symptoms.
      11 Consider reducing or rescheduling activities outdoors if you experience symptoms.
      12 Consider reducing or rescheduling activities outdoors if you experience symptoms.
      13  Reduce or reschedule activities outdoors, especially if you experience symptoms.
      14  Reduce or reschedule activities outdoors, especially if you experience symptoms.
      15 Consider reducing or rescheduling activities outdoors if you experience symptoms.
      16                                         Ideal air quality for outdoor activities.
      17                                         Ideal air quality for outdoor activities.
      18  Reduce or reschedule activities outdoors, especially if you experience symptoms.
      19  Reduce or reschedule activities outdoors, especially if you experience symptoms.
      20           No need to modify your usual activities unless you experience symptoms.
      21  Reduce or reschedule activities outdoors, especially if you experience symptoms.
      22 Consider reducing or rescheduling activities outdoors if you experience symptoms.
      23           No need to modify your usual activities unless you experience symptoms.
      24                                         Ideal air quality for outdoor activities.
         AQHI_plus_exceeds_AQHI
      1                      NA
      2                      NA
      3                      NA
      4                      NA
      5                      NA
      6                      NA
      7                      NA
      8                      NA
      9                      NA
      10                     NA
      11                     NA
      12                     NA
      13                     NA
      14                     NA
      15                     NA
      16                     NA
      17                     NA
      18                     NA
      19                     NA
      20                     NA
      21                     NA
      22                     NA
      23                     NA
      24                     NA

