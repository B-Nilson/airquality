#  TODO: test and finish this, need to evaluate time correctness (may not actually be LST for every site/year ...)
get_NAPS_data = function(years, pollutants = "PM25") {
  base_url <- "https://data-donnees.az.ec.gc.ca/api/file"
  archive_dir <- "/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/%YEAR%/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/"
  file_paths <- years |>
    lapply(\(year) {
      archive_dir |>
        stringr::str_replace("%YEAR%", as.character(year)) |>
        paste0(pollutants, "_", year, ".csv")
    }) |>
    unlist()
  request_urls <- base_url |>
    paste0("?path=", file_paths |> stringr::str_replace_all("/", "%2F"))
  naps_data <- request_urls |>
    handyr::for_each(
      \(url) {
        handyr::on_error(
          .return = NULL,
          withr::with_options(
            list(timeout = 6000),
            data.table::fread(url, skip = 7)
          )
        )
      },
      .bind = TRUE
    )
  naps_data_long <- naps_data |>
    tidyr::pivot_longer(dplyr::starts_with("H"), names_to = "hour_local") |>
    dplyr::mutate(value = value |> handyr::swap(-999, NA)) |>
    dplyr::select(
      site_id = "NAPS ID//Identifiant SNPA",
      pollutant = "Pollutant//Polluant",
      method_code = "Method Code//Code Méthode",
      lng = "Longitude//Longitude",
      lat = "Latitude//Latitude",
      city = "City//Ville",
      date = "Date//Date",
      hour_local,
      value
    ) |>
    tidyr::pivot_wider(names_from = "pollutant", values_from = "value") |>
    dplyr::mutate(
      site_id = stringr::str_pad(site_id, pad = "0", width = 6, side = "left"),
      hour_local = stringr::str_split(hour_local, "//", simplify = TRUE)[, 1] |>
        stringr::str_remove("H") |>
        as.integer()
    )

  site_timezones <- naps_data_long |>
    dplyr::select(site_id, lat, lng) |>
    dplyr::distinct(lat, lng, .keep_all = TRUE) |>
    dplyr::mutate(tz_local = handyr::get_timezone(lng, lat)) |>
    dplyr::distinct(site_id, tz_local) |>
    dplyr::mutate(
      lst_offset = "2025-06-01 00" |>
        lubridate::ymd_h(tz = tz_local[1]) |>
        format("%z") |>
        tz_offset_to_hours(),
      .by = tz_local
    )

  naps_data_long |>
    dplyr::left_join(site_timezones, by = "site_id") |>
    dplyr::mutate(
      date_utc = date |>
        format("%F") |>
        paste(hour_local - 1) |> # hours are 1 - 24, backwards local ST
        lubridate::ymd_h(tz = "UTC") - # set to UTC date (actually LST, fix next line)
        lubridate::minutes(round(lst_offset, digits = 1) * 60) + # LST -> UTC
        lubridate::hours(1), # fix 1-24 -> 0-23 conversion earlier
      is_qced = TRUE,
      source = "naps"
    ) |>
    dplyr::select(site_id, date = date_utc, pm25 = PM2.5, is_qced, source)
}

tz_offset_to_hours <- function(tz_offsets = "+0000") {
  hours <- tz_offsets |>
    stringr::str_sub(end = -3) |>
    as.numeric()
  mins <- tz_offsets |>
    stringr::str_sub(start = -2) |>
    as.numeric() *
    (hours / abs(hours))
  hours + mins / 60
}