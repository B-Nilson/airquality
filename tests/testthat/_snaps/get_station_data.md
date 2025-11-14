# expected data returned

    Code
      obs
    Output
      # A tibble: 50 x 14
         date_utc            date_local        site_id quality_assured pm25_1hr rh_1hr
         <dttm>              <chr>             <chr>   <lgl>           [ug/m^3]    [%]
       1 2019-02-01 00:00:00 2019-01-31 16:00~ E269223 TRUE                 2.8   95  
       2 2019-02-01 01:00:00 2019-01-31 17:00~ E269223 TRUE                 5     96.4
       3 2019-02-01 02:00:00 2019-01-31 18:00~ E269223 TRUE                 7     96.7
       4 2019-02-01 03:00:00 2019-01-31 19:00~ E269223 TRUE                 7.2   96.9
       5 2019-02-01 04:00:00 2019-01-31 20:00~ E269223 TRUE                 3.8   97.1
       6 2019-02-01 05:00:00 2019-01-31 21:00~ E269223 TRUE                 3.3   97.1
       7 2019-02-01 06:00:00 2019-01-31 22:00~ E269223 TRUE                 2.6   90.8
       8 2019-02-01 07:00:00 2019-01-31 23:00~ E269223 TRUE                 2.3   85.5
       9 2019-02-01 08:00:00 2019-02-01 00:00~ E269223 TRUE                 1.6   84.5
      10 2019-02-01 09:00:00 2019-02-01 01:00~ E269223 TRUE                 1.4   86.3
      # i 40 more rows
      # i 8 more variables: temp_1hr [°C], wd_1hr [°], wd_unitvector_1hr [°],
      #   ws_1hr [m/s], ws_vector_1hr [m/s], pm25_1hr_instrument <fct>, source <chr>,
      #   network <chr>

