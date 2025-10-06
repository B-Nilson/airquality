# expected data returned

    Code
      obs
    Output
      # A tibble: 1 x 5
        date_utc            date_local             site_id   quality_assured o3_1hr
        <dttm>              <chr>                  <chr>     <lgl>            [ppb]
      1 2019-02-01 00:00:00 2019-01-31 20:30 -0330 000010102 FALSE               37

---

    Code
      obs_raw
    Output
      # A tibble: 1 x 9
        date     time  siteID    site       tz_offset param unit  value operator      
        <chr>    <chr> <chr>     <chr>          <int> <chr> <chr> <dbl> <chr>         
      1 01/31/18 23:00 000010102 St. John's        -4 OZONE PPB      33 Newfoundland ~

