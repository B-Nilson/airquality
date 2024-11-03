# AQI returns expected output

    Code
      AQI(dates = lubridate::ymd_h("2024-01-01 00"), o3_8hr_ppm = 0.078,
      pm25_24hr_ugm3 = 35.9, co_8hr_ppm = 8.4)
    Output
      # A tibble: 1 x 4
        date                  AQI risk_category                  principal_pol
        <dttm>              <dbl> <fct>                          <fct>        
      1 2024-01-01 00:00:00   126 Unhealthy for Sensitive Groups o3           

