# expected data returned

    Code
      obs
    Output
      # A tibble: 1 x 6
        date_utc            date_local    site_id site_name quality_assured o3_1hr_ppb
        <dttm>              <chr>         <chr>   <chr>     <lgl>                <dbl>
      1 2019-02-01 00:00:00 2019-01-31 2~ 000010~ St. John~ FALSE                   37

---

    Code
      obs_raw
    Output
              date  time    siteID       site tz_offset param unit value
      1 2018-02-01 23:00 000010102 St. John's        -4 OZONE  PPB    33
                           operator
      1 Newfoundland & Labrador DEC

