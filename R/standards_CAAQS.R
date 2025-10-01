#' Assess the attainment of the Canadian Ambient Air Quality Standards (CAAQS)
#'
#' @param dates Vector of hourly datetime values corresponding to observations. Date gaps will be filled automatically.
#' @param pm25_1hr_ugm3 (Optional). Vector of hourly mean fine particulate matter (PM2.5) concentrations (ug/m^3).
#' @param o3_1hr_ppb (Optional). Vector of hourly mean ozone (O3) concentrations (ppb).
#' @param no2_1hr_ppb (Optional). Vector of hourly mean nitrogen dioxide (NO2) concentrations (ppb).
#' @param so2_1hr_ppb (Optional). Vector of hourly mean sulphur dioxide (SO2) concentrations (ppb).
#' @param min_completeness A single value from 0 to 1 indicating the required annual data completeness for a pollutant. Default is 0.5 (50 percent).
#'
#' @description
#' The Canadian Ambient Air Quality Standards (CAAQS) are part of a collaborative national Air Quality Management System (AQMS), to better protect human health and the environment.
#' Standards at various averaging periods are defined for fine particulate matter (PM2.5), ozone (O3), nitrogen dioxide (NO2), and sulphur dioxide (SO2), and are typically updated ever 5 years.
#'
#' Management levels (Green -> Yellow -> Orange -> Red) are defined for each pollutant standard.
#' A "Red" level indicates exceedance of the CAAQS and management plans are typically developed for regions at "Orange" or worse levels.
#'
#' @references \url{https://ccme.ca/en/air-quality-report}
#' @family Canadian Air Quality
#' @family Air Quality Standards
#'
#' @return a list of tibbles (data.frames), one tibble per pollutant provided with annual CAAQS metrics and management levels
#' @export
#'
#' @examples
#' obs <- data.frame(
#'   date = seq(
#'     lubridate::ymd_h("2020-01-01 00"),
#'     lubridate::ymd_h("2023-12-31 23"), "1 hours"
#'   ),
#'   pm25 = sample(1:150, 35064, TRUE), o3 = sample(1:150, 35064, TRUE),
#'   no2 = sample(1:150, 35064, TRUE), so2 = sample(1:150, 35064, TRUE)
#' )
#' CAAQS(
#'   dates = obs$date, pm25_1hr_ugm3 = obs$pm25,
#'   o3_1hr_ppb = obs$o3, no2_1hr_ppb = obs$no2, so2_1hr_ppb = obs$so2
#' )
CAAQS <- function(
  dates,
  pm25_1hr_ugm3 = NULL,
  o3_1hr_ppb = NULL,
  no2_1hr_ppb = NULL,
  so2_1hr_ppb = NULL,
  min_completeness = 0.5
) {
  # Join inputs
  obs <- dplyr::bind_cols(
    date = dates,
    pm25 = pm25_1hr_ugm3,
    o3 = o3_1hr_ppb,
    no2 = no2_1hr_ppb,
    so2 = so2_1hr_ppb
  ) |>
    dplyr::mutate(year = lubridate::year(.data$date))

  # Assess hours of data for each pollutant annually
  has_enough_obs <- obs |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      .groups = "drop",
      dplyr::across(-"date", \(x) sum(!is.na(x)))
    ) |>
    dplyr::mutate(dplyr::across(-"year", \(x) {
      total_hours <- ifelse(is_leap_year(.data$year), 8784, 8760)
      (x / total_hours > min_completeness) |>
        handyr::swap(NA, with = FALSE)
    })) |>
    tidyr::complete(year = min(.data$year):max(.data$year))

  # Check for 3 consecutive years for any pollutant
  has_3_consecutive_years <- has_enough_obs |>
    dplyr::summarise(dplyr::across(-"year", \(x) {
      any((x + dplyr::lag(x) + dplyr::lag(x, 2)) >= 3)
    }))
  if (all(!has_3_consecutive_years)) {
    stop(paste(
      "Cannot calculate CAAQS without at least one pollutant with at least 3 years",
      "with `min_completeness`x100% of hourly observations."
    ))
  }

  # Drop data for years lacking enough data
  pols <- names(has_enough_obs)[-1]
  for (pol in pols) {
    insufficient_years <- has_enough_obs$year[unlist(!has_enough_obs[pol])]
    if (length(insufficient_years)) {
      warning(paste(
        "Insufficient data collected for pol:",
        pol,
        "for year(s):",
        paste(insufficient_years, collapse = ", "),
        "see argument `min_completeness`"
      ))
      is_insufficient_year <- obs$year %in% insufficient_years
      obs[is_insufficient_year, pol] <- NA
    }
  }

  # Fill in missing hours with NAs
  obs <- obs |>
    tidyr::complete(
      date = seq(
        min(dates) |> lubridate::floor_date("years"),
        (max(dates) |> lubridate::ceiling_date("years")) - lubridate::hours(1),
        "1 hours"
      )
    ) |>
    dplyr::arrange(.data$date)

  # Calculate CAAQS attainment where data provided
  thresholds <- CAAQS_thesholds()
  list(
    pm25 = if (!is.null(pm25_1hr_ugm3)) CAAQS_pm25(obs, thresholds),
    o3 = if (!is.null(o3_1hr_ppb)) CAAQS_o3(obs, thresholds),
    no2 = if (!is.null(no2_1hr_ppb)) CAAQS_no2(obs, thresholds),
    so2 = if (!is.null(so2_1hr_ppb)) CAAQS_so2(obs, thresholds)
  )
}

## CAAQS Helpers ----------------------------------------------------------
CAAQS_pm25 <- function(obs, thresholds) {
  obs |>
    # Hourly mean -> daily mean
    dplyr::group_by(
      date = .data$date |> lubridate::floor_date("days")
    ) |>
    dplyr::summarise(dplyr::across(
      dplyr::everything(),
      c(mean = mean_no_na)
    )) |>
    # Daily mean -> annual 98th percentile and annual mean
    dplyr::group_by(year = lubridate::year(.data$date)) |>
    dplyr::summarise(
      .groups = "drop",
      perc_98_of_daily_means = .data$pm25_mean |>
        stats::quantile(0.98, na.rm = T) |>
        unname(),
      mean_of_daily_means = mean_no_na(.data$pm25_mean)
    ) |>
    # +3 year averages, +whether standard is met
    dplyr::mutate(
      `3yr_mean_of_perc_98` = .data$perc_98_of_daily_means |>
        get_lag_n_mean(n = 3),
      management_level_daily = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`3yr_mean_of_perc_98`[.data$year == y],
              thresholds = thresholds$pm25$daily
            )
          }
        ),
      `3yr_mean_of_means` = .data$mean_of_daily_means |>
        get_lag_n_mean(n = 3),
      management_level_annual = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`3yr_mean_of_means`[.data$year == y],
              thresholds = thresholds$pm25$annual
            )
          }
        )
    ) |>
    dplyr::relocate(
      "management_level_daily",
      "mean_of_daily_means",
      .after = "3yr_mean_of_perc_98"
    )
}

CAAQS_o3 <- function(obs, thresholds) {
  obs |>
    # hourly mean -> 8 hourly mean
    dplyr::group_by(
      date = .data$date |>
        lubridate::floor_date("8 hours")
    ) |>
    dplyr::summarise(`8hr_mean_o3` = .data$o3 |> mean_no_na()) |>
    # 8 hourly mean -> daily max
    dplyr::group_by(date = .data$date |> lubridate::floor_date("days")) |>
    dplyr::summarise(
      daily_max_8hr_mean_o3 = .data$`8hr_mean_o3` |> handyr::max(na.rm = TRUE)
    ) |>
    # daily max -> annual 4th highest
    dplyr::group_by(year = .data$date |> lubridate::year()) |>
    dplyr::arrange(dplyr::desc(.data$daily_max_8hr_mean_o3)) |>
    dplyr::summarise(
      .groups = "drop",
      fourth_highest_daily_max_8hr_mean_o3 = .data$daily_max_8hr_mean_o3[4]
    ) |>
    # +3 year averages, +whether standard is met
    dplyr::mutate(
      `3yr_mean` = .data$fourth_highest_daily_max_8hr_mean_o3 |>
        get_lag_n_mean(n = 3),
      management_level_8hr = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`3yr_mean`[.data$year == y],
              thresholds = thresholds$o3$`8hr`
            )
          }
        )
    )
}

CAAQS_no2 <- function(obs, thresholds) {
  obs |>
    # + annual mean
    dplyr::group_by(year = .data$date |> lubridate::year()) |>
    dplyr::mutate(annual_mean_of_hourly = .data$no2 |> mean_no_na()) |>
    # hourly mean -> daily maxima
    dplyr::group_by(
      date = .data$date |> lubridate::floor_date("1 days"),
      .data$annual_mean_of_hourly
    ) |>
    dplyr::summarise(
      .groups = "drop",
      daily_max_hourly_no2 = .data$no2 |> handyr::max(na.rm = TRUE)
    ) |>
    # daily maxima -> annual 98th percentile
    dplyr::group_by(
      year = date |> lubridate::year(),
      .data$annual_mean_of_hourly
    ) |>
    dplyr::summarise(
      .groups = "drop",
      perc_98_of_daily_maxima = .data$daily_max_hourly_no2 |>
        stats::quantile(0.98, na.rm = T) |>
        unname()
    ) |>
    # +3 year averages, +standard for that year, +whether standard is met
    dplyr::mutate(
      `3yr_mean_of_perc_98` = .data$perc_98_of_daily_maxima |>
        get_lag_n_mean(n = 3),
      management_level_hourly = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`annual_mean_of_hourly`[.data$year == y],
              thresholds = thresholds$no2$hourly
            )
          }
        ),
      management_level_annual = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`3yr_mean_of_perc_98`[.data$year == y],
              thresholds = thresholds$no2$annual
            )
          }
        )
    ) |>
    dplyr::relocate(
      "management_level_hourly",
      .after = "annual_mean_of_hourly"
    )
}

CAAQS_so2 <- function(obs, thresholds) {
  obs |>
    # + annual mean
    dplyr::group_by(year = date |> lubridate::year()) |>
    dplyr::mutate(annual_mean_of_hourly = .data$so2 |> mean_no_na()) |>
    # hourly mean -> daily maxima
    dplyr::group_by(
      date = date |> lubridate::floor_date("1 days"),
      .data$annual_mean_of_hourly
    ) |>
    dplyr::summarise(
      .groups = "drop",
      daily_max_hourly_so2 = .data$so2 |> handyr::max(na.rm = TRUE)
    ) |>
    # daily maxima -> annual 98th percentile
    dplyr::group_by(
      year = date |> lubridate::year(),
      .data$annual_mean_of_hourly
    ) |>
    dplyr::summarise(
      .groups = "drop",
      perc_99_of_daily_maxima = .data$daily_max_hourly_so2 |>
        stats::quantile(0.99, na.rm = T) |>
        unname()
    ) |>
    # +3 year averages, +standard for that year, +whether standard is met
    dplyr::mutate(
      `3yr_mean_of_perc_99` = .data$perc_99_of_daily_maxima |>
        get_lag_n_mean(n = 3),
      management_level_hourly = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`annual_mean_of_hourly`[.data$year == y],
              thresholds = thresholds$so2$hourly
            )
          }
        ),
      management_level_annual = .data$year |>
        sapply(
          \(y) {
            CAAQS_meets_standard(
              year = y,
              metric = .data$`3yr_mean_of_perc_99`[.data$year == y],
              thresholds = thresholds$so2$annual
            )
          }
        )
    ) |>
    dplyr::relocate(
      "management_level_hourly",
      .after = "annual_mean_of_hourly"
    )
}

CAAQS_meets_standard <- function(year, metric, thresholds) {
  mgmt_levels <- thresholds[as.numeric(names(thresholds)) <= year] |>
    dplyr::last()
  if (length(mgmt_levels) == 0) {
    return(NA)
  }
  # Calculate CAAQS attainment
  attainment <- mgmt_levels |>
    handyr::for_each(
      .as_list = TRUE,
      .bind = TRUE,
      .show_progress = FALSE,
      \(lvl) metric > lvl
    )
  attainment <- attainment |>
    apply(1, \(x) handyr::min(which(x), na.rm = TRUE))
  attainment[!is.na(attainment)] <- names(mgmt_levels)[
    attainment[!is.na(attainment)]
  ]
  return(attainment)
}

CAAQS_thesholds <- function() {
  list(
    pm25 = list(
      daily = list(
        "2015" = c(Red = 28, Orange = 19, Yellow = 10.01, Green = 0),
        "2020" = c(Red = 27, Orange = 19, Yellow = 10.01, Green = 0)
      ),
      annual = list(
        "2015" = c(Red = 10, Orange = 6.41, Yellow = 4.01, Green = 0),
        "2020" = c(Red = 8.8, Orange = 6.41, Yellow = 4.01, Green = 0)
      )
    ),
    o3 = list(
      `8hr` = list(
        "2015" = c(Red = 63, Orange = 56.01, Yellow = 50.01, Green = 0),
        "2020" = c(Red = 62, Orange = 56.01, Yellow = 50.01, Green = 0),
        "2025" = c(Red = 60, Orange = 56.01, Yellow = 50.01, Green = 0)
      )
    ),
    no2 = list(
      hourly = list(
        "2020" = c(Red = 60, Orange = 31.01, Yellow = 20.01, Green = 0),
        "2025" = c(Red = 42, Orange = 31.01, Yellow = 20.01, Green = 0)
      ),
      annual = list(
        "2020" = c(Red = 17, Orange = 7.01, Yellow = 2.01, Green = 0),
        "2025" = c(Red = 12, Orange = 7.01, Yellow = 2.01, Green = 0)
      )
    ),
    so2 = list(
      hourly = list(
        "2020" = c(Red = 70, Orange = 50.01, Yellow = 30.01, Green = 0),
        "2025" = c(Red = 65, Orange = 50.01, Yellow = 30.01, Green = 0)
      ),
      annual = list(
        "2020" = c(Red = 5, Orange = 3.01, Yellow = 2.01, Green = 0),
        "2025" = c(Red = 4, Orange = 3.01, Yellow = 2.01, Green = 0)
      )
    )
  )
}

# TODO: implement
CAAQS_objectives <- function(mgmt_levels) {
  c(
    Green = "To maintain good air quality through proactive air management measures to keep clean areas clean.",
    Yellow = "To improve air quality using early and ongoing actions for continuous improvement.",
    Orange = "To improve air quality through active air management and prevent exceedance of the CAAQS.",
    Red = "To reduce pollutant levels below the CAAQS through advanced air management actions."
  )
}
