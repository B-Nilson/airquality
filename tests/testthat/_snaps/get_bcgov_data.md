# expected data returned

    Code
      obs
    Output
      # A tibble: 50 x 29
         date_utc            date_local             site_id site_name  quality_assured
         <dttm>              <chr>                  <chr>   <chr>      <lgl>          
       1 2019-02-01 00:00:00 2019-01-31 16:00 -0800 0450307 Prince Ge~ TRUE           
       2 2019-02-01 00:00:00 2019-01-31 16:00 -0800 0450307 Prince Ge~ TRUE           
       3 2019-02-01 01:00:00 2019-01-31 17:00 -0800 0450307 Prince Ge~ TRUE           
       4 2019-02-01 01:00:00 2019-01-31 17:00 -0800 0450307 Prince Ge~ TRUE           
       5 2019-02-01 02:00:00 2019-01-31 18:00 -0800 0450307 Prince Ge~ TRUE           
       6 2019-02-01 02:00:00 2019-01-31 18:00 -0800 0450307 Prince Ge~ TRUE           
       7 2019-02-01 03:00:00 2019-01-31 19:00 -0800 0450307 Prince Ge~ TRUE           
       8 2019-02-01 03:00:00 2019-01-31 19:00 -0800 0450307 Prince Ge~ TRUE           
       9 2019-02-01 04:00:00 2019-01-31 20:00 -0800 0450307 Prince Ge~ TRUE           
      10 2019-02-01 04:00:00 2019-01-31 20:00 -0800 0450307 Prince Ge~ TRUE           
      # i 40 more rows
      # i 24 more variables: pm25_1hr_ugm3 <dbl>, pm25_1hr_ugm3_instrument <chr>,
      #   pm10_1hr_ugm3 <dbl>, pm10_1hr_ugm3_instrument <chr>, o3_1hr_ppb <dbl>,
      #   o3_1hr_ppb_instrument <chr>, no_1hr_ppb <dbl>, no_1hr_ppb_instrument <chr>,
      #   no2_1hr_ppb <dbl>, no2_1hr_ppb_instrument <chr>, nox_1hr_ppb <dbl>,
      #   nox_1hr_ppb_instrument <chr>, so2_1hr_ppb <dbl>,
      #   so2_1hr_ppb_instrument <chr>, trs_1hr_ppb <dbl>, ...

---

    Code
      obs_raw
    Output
      # A tibble: 25 x 47
         date_utc            DATE_PST            STATION_NAME    EMS_ID PM25_RAW  PM25
         <dttm>              <chr>               <chr>           <chr>     <dbl> <dbl>
       1 2019-02-01 00:00:00 2019-01-31 16:00 -8 Prince George ~ 04503~     5.43   5.4
       2 2019-02-01 01:00:00 2019-01-31 17:00 -8 Prince George ~ 04503~     9.39   9.4
       3 2019-02-01 02:00:00 2019-01-31 18:00 -8 Prince George ~ 04503~    11.7   11.7
       4 2019-02-01 03:00:00 2019-01-31 19:00 -8 Prince George ~ 04503~    10.7   10.7
       5 2019-02-01 04:00:00 2019-01-31 20:00 -8 Prince George ~ 04503~     9.42   9.4
       6 2019-02-01 05:00:00 2019-01-31 21:00 -8 Prince George ~ 04503~     9.86   9.9
       7 2019-02-01 06:00:00 2019-01-31 22:00 -8 Prince George ~ 04503~     4.92   4.9
       8 2019-02-01 07:00:00 2019-01-31 23:00 -8 Prince George ~ 04503~     3.01   3  
       9 2019-02-01 08:00:00 2019-02-01 00:00 -8 Prince George ~ 04503~     3.39   3.4
      10 2019-02-01 09:00:00 2019-02-01 01:00 -8 Prince George ~ 04503~     3.54   3.5
      # i 15 more rows
      # i 41 more variables: PM25_INSTRUMENT <chr>, O3_RAW <dbl>, O3 <dbl>,
      #   O3_INSTRUMENT <chr>, NO_RAW <dbl>, NO <dbl>, NO_INSTRUMENT <chr>,
      #   NO2_RAW <dbl>, NO2 <dbl>, NO2_INSTRUMENT <chr>, SO2_RAW <dbl>, SO2 <dbl>,
      #   SO2_INSTRUMENT <chr>, PM10_RAW <dbl>, PM10 <dbl>, PM10_INSTRUMENT <chr>,
      #   WSPD_VECT_RAW <lgl>, WSPD_VECT <lgl>, WSPD_VECT_INSTRUMENT <lgl>,
      #   WDIR_VECT_RAW <lgl>, WDIR_VECT <lgl>, WDIR_VECT_INSTRUMENT <lgl>, ...

