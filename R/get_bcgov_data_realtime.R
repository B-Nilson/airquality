bcgov_get_realtime_data <- function(stations, quiet = FALSE) {
  realtime_directory <- bcgov_ftp_site |>
    paste0("/Hourly_Raw_Air_Data/Station/")

  force_col_class <- c(
    DATE_PST = "character",
    EMS_ID = "character",
    STATION = "character"
  )

  all_stations <- bcgov_get_raw_stations(realtime = TRUE)
  if (any(stations == "all")) {
    stations <- all_stations
  }

  if(all(!stations %in% all_stations)) {
    stop("All provided stations not available for realtime data")
  }
  if(any(!stations %in% all_stations)) {
    if (!quiet) warning(
      "Some stations not available for realtime data: ", 
      paste0(stations[!stations %in% all_stations], collapse = ", ")
    )
  }
  stations <- stations[stations %in% all_stations]

  variables[variables == "pm2.5"] <- "pm25"
  
  if (any(variables == "all")) {
    variables_to_drop = character(0)
  } else {
    variables_to_drop <- bcgov_col_names[
      !(names(bcgov_col_names) |>
        stringr::str_starts(variables |> paste0(collapse = "|"))) &
        !(names(bcgov_col_names) %in%
          c('date_utc', 'site_id', 'quality_assured'))
    ] |>
      unname()
  }

  # Download each stations file and bind together
  realtime_directory |>
    paste0(stations, ".csv") |>
    handyr::for_each(
      .as_list = TRUE, # TODO: remove once handyr updated (should be default when .bind = TRUE)
      .bind = TRUE,
      \(path) {
        withr::with_options(
          list(timeout = 3600),
          data.table::fread(
            file = path,
            colClasses = force_col_class,
            showProgress = !quiet
          ) |>
            bcgov_format_raw_data(mode = "realtime") |>
            dplyr::select(-dplyr::any_of(variables_to_drop)) |>
            handyr::on_error(.return = NULL, .message = TRUE)
        )
      }
    ) |> 
    dplyr::mutate(quality_assured = FALSE)
}