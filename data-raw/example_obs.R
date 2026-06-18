date_range <- lubridate::ymd_h(
  c("2014-01-01 00", "2014-12-31 23"),
  tz = "America/Vancouver"
)
example_obs <- get_bcgov_data(
  stations = "0450307",
  date_range = date_range,
  quiet = FALSE
)

example_obs <- example_obs |>
  dplyr::select(date_utc, date_local, dplyr::ends_with("_1hr")) |>
  dplyr::mutate(
    date_local = lubridate::parse_date_time(
      date_local,
      "%Y-%m-%d %H:%M %z",
      tz = "America/Vancouver"
    )
  )

usethis::use_data(example_obs, overwrite = TRUE)
