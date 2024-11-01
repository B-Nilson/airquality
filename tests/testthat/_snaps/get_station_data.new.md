# expected data returned

    Code
      obs
    Output
      # A tibble: 50 x 17
         date_utc            date_local             site_id site_name  quality_assured
         <dttm>              <chr>                  <chr>   <chr>      <lgl>          
       1 2019-02-01 00:00:00 2019-01-31 16:00 -0800 E269223 Vanderhoo~ TRUE           
       2 2019-02-01 01:00:00 2019-01-31 17:00 -0800 E269223 Vanderhoo~ TRUE           
       3 2019-02-01 02:00:00 2019-01-31 18:00 -0800 E269223 Vanderhoo~ TRUE           
       4 2019-02-01 03:00:00 2019-01-31 19:00 -0800 E269223 Vanderhoo~ TRUE           
       5 2019-02-01 04:00:00 2019-01-31 20:00 -0800 E269223 Vanderhoo~ TRUE           
       6 2019-02-01 05:00:00 2019-01-31 21:00 -0800 E269223 Vanderhoo~ TRUE           
       7 2019-02-01 06:00:00 2019-01-31 22:00 -0800 E269223 Vanderhoo~ TRUE           
       8 2019-02-01 07:00:00 2019-01-31 23:00 -0800 E269223 Vanderhoo~ TRUE           
       9 2019-02-01 08:00:00 2019-02-01 00:00 -0800 E269223 Vanderhoo~ TRUE           
      10 2019-02-01 09:00:00 2019-02-01 01:00 -0800 E269223 Vanderhoo~ TRUE           
      # i 40 more rows
      # i 12 more variables: pm25_1hr_ugm3 <dbl>, pm25_1hr_ugm3_instrument <chr>,
      #   rh_1hr_percent <dbl>, rh_1hr_percent_instrument <chr>, t_1hr_celcius <dbl>,
      #   t_1hr_celcius_instrument <chr>, wd_1hr_degrees <dbl>,
      #   wd_1hr_degrees_instrument <chr>, ws_1hr_ms <dbl>,
      #   ws_1hr_ms_instrument <chr>, source <chr>, network <chr>

