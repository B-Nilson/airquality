
# Canadian AQHI -----------------------------------------------------------

#' Calculate the Canadian AQHI from hourly PM2.5, NO2, and O3 observations
#'
#' @param dates Vector of hourly datetimes corresponding to observations. Date gaps will be filled automatically.
#' @param pm25_1hr_ugm3 Numeric vector of hourly mean fine particulate matter (PM2.5) concentrations (ug/m^3).
#' @param no2_1hr_ppb (Optional). Numeric vector of hourly mean nitrogen dioxide (NO2) concentrations (ppb). If not provided AQHI+ will be calculated from PM2.5 only.
#' @param o3_1hr_ppb (Optional). Numeric vector of hourly mean ozone (O3) concentrations (ppb). If not provided AQHI+ will be calculated from PM2.5 only.
#' @param quiet (Optional). A single logical (TRUE/FALSE) value indicating if AQHI+ warning (if o3 and no2 not provided) should be hidden. Default is FALSE
#'
#' @description
#' The Canadian Air Quality Health Index (AQHI) combines the health risk of
#' fine particulate matter (PM2.5), ozone (O3), and nitrogen dioxide (NO2) on a scale from 1-10 (+).
#' The AQHI is "the sum of excess mortality risk associated with individual pollutants
#' from a time-series analysis of air pollution and mortality in Canadian cities,
#' adjusted to a 0–10 scale, and calculated hourly on the basis of trailing 3-hr average pollutant concentrations."
#'
#' The AQHI is overriden by the Canadian AQHI+ if the AQHI+ exceeds the AQHI for a particular hour.
#' The AQHI+ is a modification of the Canadian Air Quality Health Index (AQHI).
#' AQHI+ only uses fine particulate matter (PM2.5) instead of the combination of PM2.5, ozone (O3), and nitrogen dioxide (NO2).
#' Unlike the AQHI which uses 3-hour mean averages, AQHI+ is calculated using hourly means.
#'
#' The AQHI was originally published by Steib et. al in 2008,
#' and has been adopted by all Canadian provinces/territories
#' (except Quebec where they use the AQI instead of the AQHI/AQHI+).
#'
#' @references \url{https://doi.org/10.3155/1047-3289.58.3.435}
#'
#' @family Canadian Air Quality
#' @family Air Quality Standards
#'
#' @return A tibble (data.frame) with columns (*if all 3 pollutants provided):
#' date, pm25, o3*, no2*, pm25_rolling_3hr*, o3_rolling_3hr*, o3_rolling_3hr*,
#'  AQHI, AQHI_plus, risk, high_risk_pop_message, general_pop_message, AQHI_plus_exceeds_AQHI*
#'  and potentially more rows than `length(dates)` (due to any missing hours being filled with NA values).
#' @export
#'
#' @examples
#' obs = data.frame(
#'   date = seq(lubridate::ymd_h("2024-01-01 00"),
#'              lubridate::ymd_h("2024-01-01 23"), "1 hours"),
#'   pm25 = sample(1:150, 24), o3 = sample(1:150, 24), no2 = sample(1:150, 24))
#' AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25,
#'      o3_1hr_ppb = obs$o3, no2_1hr_ppb = obs$no2)
#'
#' AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25) # Returns AQHI+
AQHI = function(dates, pm25_1hr_ugm3, no2_1hr_ppb = NA, o3_1hr_ppb = NA, quiet = FALSE){
  # See: https://www.tandfonline.com/doi/abs/10.3155/1047-3289.58.3.435
  . = NULL # so build check doesn't yell at me
  aqhi_breakpoints = stats::setNames(
    c(-Inf, 1:10*10, Inf),
    c(NA, 1:10, "+")
  )
  # Join inputs
  obs = dplyr::bind_cols(
    date = dates, pm25 = pm25_1hr_ugm3,
    o3 = o3_1hr_ppb, no2 = no2_1hr_ppb
  ) %>%
    # Fill in missing hours with NAs
    tidyr::complete(date = seq(min(date), max(date), "1 hours")) %>%
    dplyr::arrange(date)

  # Calculate AQHI+ (pm25 only)
  aqhi_plus = AQHI_plus(obs$pm25) %>%
    # Add columns in case only PM2.5 provided
    dplyr::mutate(AQHI = .data$AQHI_plus, AQHI_plus_exceeds_AQHI = NA) %>%
    dplyr::relocate('AQHI', .before = "AQHI_plus")

  # Need all 3 pollutants to calculate AQHI
  have_all_3_pol = !all(is.na(pm25_1hr_ugm3)) &
    !all(is.na(no2_1hr_ppb)) & !all(is.na(o3_1hr_ppb))
  if (have_all_3_pol) {
    obs = obs %>%
      # +3hr rolling means, + AQHI, +risk levels
      dplyr::mutate(
        # Calculate rolling 3 hour averages (at least 2 hours per average)
        pm25_rolling_3hr = roll_mean_3hr_min_2(.data$pm25),
        no2_rolling_3hr = roll_mean_3hr_min_2(.data$no2),
        o3_rolling_3hr = roll_mean_3hr_min_2(.data$o3),
        # Calculate AQHI
        AQHI = cut(AQHI_formula(
          .data$pm25_rolling_3hr, .data$no2_rolling_3hr, .data$o3_rolling_3hr),
          breaks = aqhi_breakpoints/10,
          labels = names(aqhi_breakpoints[-1])),
        AQHI_plus = aqhi_plus$AQHI_plus,
        # Get risk levels
        risk = AQHI_risk_category(.data$AQHI)
      ) %>%
      # + health messaging
      dplyr::bind_cols(., AQHI_health_messaging(.$risk)) %>%
      # Use AQHI+ (levels, risk, and messaging) if AQHI+ exceeds AQHI
      AQHI_replace_w_AQHI_plus(aqhi_plus)
  }else{
    if(!quiet) warning("Returning AQHI+ (PM2.5 only) as no non-missing NO2 / O3 provided.")
    obs = aqhi_plus
  }
  return(obs)
}

#' Calculate the Canadian AQHI+ from hourly PM2.5 observations
#'
#' @param pm25_1hr_ugm3 A numeric/integer vector with hourly mean PM2.5 concentrations (ug/m^3).
#' @param min_allowed_pm25 A single numeric value indicating the minimum allowed concentration (Defaults to 0 ug/m^3). All values in `pm25_1hr_ugm3` less than this will be replaced with NA.
#'
#' @description
#' The Canadian AQHI+ is a modification of the Canadian Air Quality Health Index (AQHI).
#' AQHI+ is meant for augmenting the AQHI to provide a more responsive health index during wildfire smoke events.
#' AQHI+ only uses fine particulate matter (PM2.5) instead of the combination of PM2.5, ozone (O3), and nitrogen dioxide (NO2).
#' In addition, AQHI+ is calculated using hourly mean averages instead of 3-hourly mean averages used by the AQHI.
#' The AQHI+ overrides the AQHI if it exceeds the AQHI for a particular hour.
#'
#' AQHI+ splits hourly PM2.5 concentrations into bins of 10 ug/m^3 from 0 to 100 ug/m^3 (AQHI+ 1 - 10),
#' and assigns "+" to values greater than 100 ug/m^3.
#' The risk categories match the AQHI (Low [1-3, or 0-30 ug/m^3], Moderate [4-6, or 30.1-60 ug/m^3],
#' High [7-10, or 60.1-100 ug/m^3], and Very High [+, or >100 ug/m^3]) and share the same health messaging.
#'
#' The AQHI+ was originally published by Yao et. al in 2019,
#' and has been adopted by all Canadian provinces/territories as of 2024
#' (except Quebec where they use the AQI instead of the AQHI and AQHI+).
#'
#' @references \url{https://doi.org/10.17269/s41997-019-00237-w}
#'
#' @family Canadian Air Quality
#' @family Air Quality Standards
#'
#' @return A tibble (data.frame) with columns:
#' hourly_pm25, AQHI_plus, risk, high_risk_pop_message, general_pop_message
#' and `length(pm25_1hr_ugm3)` rows
#' @export
#'
#' @examples
#' # Hourly pm2.5 concentrations
#' obs = data.frame(
#'   date = seq(lubridate::ymd_h("2024-01-01 00"),
#'              lubridate::ymd_h("2024-01-01 23"), "1 hours"),
#'   pm25 = sample(1:150, 24))
#' # Calculate the AQHI+
#' AQHI_plus(obs$pm25)
#'
#' # Hourly pm2.5 concentrations (with negative values)
#' obs = data.frame(
#'   date = seq(lubridate::ymd_h("2024-01-01 00"),
#'              lubridate::ymd_h("2024-01-01 23"), "1 hours"),
#'   pm25 = c(-2, -0.1, sample(1:150, 22)))
#' # Calculate the AQHI+ for each hour, except for hours where pm2.5 is < -0.5
#' AQHI_plus(obs$pm25, min_allowed_pm25 = -0.5)
#' @importFrom rlang .data
AQHI_plus = function(pm25_1hr_ugm3, min_allowed_pm25 = 0){

  # Define breakpoint for AQHI levels
  aqhi_breakpoints = stats::setNames(
    c(-Inf, 1:10*10, Inf),
    c(NA, 1:10, "+")
  )

  # Remove values < min_allowed_pm25 (normally 0)
  pm25_1hr_ugm3[pm25_1hr_ugm3 < min_allowed_pm25] = NA

  # Get AQHI+
  aqhi_p = cut(pm25_1hr_ugm3,
      breaks = aqhi_breakpoints,
      labels = names(aqhi_breakpoints)[-1])

  # Get risk levels
  risk = AQHI_risk_category(aqhi_p)

  # Get health messages
  health_messages = AQHI_health_messaging(risk)

  # Combine and return
  data.frame(
    pm25_1hr_ugm3 = pm25_1hr_ugm3,
    AQHI_plus = aqhi_p,
    risk = risk,
    health_messages
  )

}

## AQHI Helpers -----------------------------------------------------------
AQHI_formula = function(pm25_rolling_3hr, no2_rolling_3hr, o3_rolling_3hr){
  round(10/10.4 * 100 * (
    (exp(0.000537 * o3_rolling_3hr) - 1) +
      (exp(0.000871 * no2_rolling_3hr) - 1) +
      (exp(0.000487 * pm25_rolling_3hr) - 1)
  ))
}

AQHI_risk_category = function(AQHI){
  aqhi_levels = list(
    Low = 1:3,
    Moderate = 4:6,
    High = 7:10,
    "Very High" = "+"
  )
  risk = factor(
    AQHI, unlist(aqhi_levels),
    unlist(
      sapply(seq_along(aqhi_levels), \(i){
        rep(names(aqhi_levels)[i], length(aqhi_levels[[i]]))
      })
    )
  )
  return(risk)
}

AQHI_health_messaging = function(risk_categories){
  aqhi_messaging = list(
    Low = data.frame(
      high_risk_pop_message = "Enjoy your usual activities.",
      general_pop_message = "Ideal air quality for outdoor activities."
    ),
    Moderate = data.frame(
      high_risk_pop_message = "Consider reducing or rescheduling activities outdoors if you experience symptoms.",
      general_pop_message = "No need to modify your usual activities unless you experience symptoms."
    ),
    High = data.frame(
      high_risk_pop_message = "Reduce or reschedule activities outdoors.",
      general_pop_message = "Consider reducing or rescheduling activities outdoors if you experience symptoms."
    ),
    "Very High" = data.frame(
      high_risk_pop_message = "Avoid strenuous activity outdoors.",
      general_pop_message = "Reduce or reschedule activities outdoors, especially if you experience symptoms."
    )
  )

  lapply(aqhi_messaging[risk_categories], \(x){
    if (is.null(x)) {
      data.frame(high_risk_pop_message = NA, general_pop_message = NA)
    }else return(x)
  }) %>% dplyr::bind_rows()
}

# TODO: make sure AQHI is a column in obs
AQHI_replace_w_AQHI_plus = function(obs, aqhi_plus){
  obs %>%
    # Use AQHI+ (levels, risk, and messaging) if AQHI+ exceeds AQHI
    dplyr::mutate(
      # Check in AQHI+ > AQHI
      AQHI_plus_exceeds_AQHI = swap_na(
        as.numeric(aqhi_plus$AQHI_plus) > as.numeric(.data$AQHI), TRUE),
      # Replace AQHI levels if so
      AQHI = dplyr::case_when(
        AQHI_plus_exceeds_AQHI ~ aqhi_plus$AQHI_plus, TRUE ~ .data$AQHI),
      # And risk levels
      risk = dplyr::case_when(
        AQHI_plus_exceeds_AQHI ~ aqhi_plus$risk, TRUE ~ .data$risk),
      # And health messaging
      high_risk_pop_message = dplyr::case_when(
        AQHI_plus_exceeds_AQHI ~ aqhi_plus$high_risk_pop_message,
        TRUE ~ .data$high_risk_pop_message),
      general_pop_message = dplyr::case_when(
        AQHI_plus_exceeds_AQHI ~ aqhi_plus$general_pop_message,
        TRUE ~ .data$general_pop_message)
    )
}


# CAAQS -------------------------------------------------------------------

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
#' obs = data.frame(
#'   date = seq(lubridate::ymd_h("2020-01-01 00"),
#'              lubridate::ymd_h("2023-12-31 23"), "1 hours"),
#'   pm25 = sample(1:150, 35064, TRUE), o3 = sample(1:150, 35064, TRUE),
#'   no2 = sample(1:150, 35064, TRUE), so2 = sample(1:150, 35064, TRUE)
#' )
#' CAAQS(dates = obs$date, pm25_1hr_ugm3 = obs$pm25,
#'      o3_1hr_ppb = obs$o3, no2_1hr_ppb = obs$no2, so2_1hr_ppb = obs$so2)
CAAQS = function(dates, pm25_1hr_ugm3 = NULL, o3_1hr_ppb = NULL,
                 no2_1hr_ppb = NULL, so2_1hr_ppb = NULL,
                 min_completeness = 0.5){
  # Define thresholds for each pollutant / avg / year
  thresholds = CAAQS_thesholds()

  # Get full record of dates for years provided
  complete_dates = seq(
    lubridate::floor_date(min(dates), "years"),
    lubridate::ceiling_date(max(dates), "years") - lubridate::hours(1),
    "1 hours")

  # Join inputs
  obs = dplyr::bind_cols(
    date = dates, pm25 = pm25_1hr_ugm3,
    o3 = o3_1hr_ppb, no2 = no2_1hr_ppb,
    so2 = so2_1hr_ppb)

  # Assess hours of data for each pollutant annually
  has_enough_obs = obs %>%
    dplyr::group_by(year = lubridate::year(.data$date)) %>%
    dplyr::summarise(dplyr::across(-1, \(x) sum(!is.na(x)))) %>%
    dplyr::mutate(dplyr::across(-1, \(x) x/ifelse(.data$year%%4==0, 8784, 8760))) %>%
    dplyr::mutate(dplyr::across(-1, \(x) swap_na(x > min_completeness, F))) %>%
    tidyr::complete(year = min(.data$year):max(.data$year))
  # Check for 3 consecutive years for any pollutant
  has_3_consecutive_years = has_enough_obs %>%
    dplyr::summarise(
      dplyr::across(-1, \(x) any((x + dplyr::lag(x) + dplyr::lag(x, 2)) >= 3)))
  # Stop if not enough data provided
  if(all(!has_3_consecutive_years))
    stop("CAAQS requires at least 3 years with `min_completeness`x100% of hourly observations for at least one pollutant.")
  # Drop data for years lacking enough data
  for(pol in names(has_enough_obs)[-1]){
    insufficient_years = has_enough_obs$year[unlist(!has_enough_obs[pol])]
    if(length(insufficient_years)){
      warning(paste("Insufficient data collected for pol:", pol,
                    "for year(s):", paste(insufficient_years, collapse = ", "),
                    "see argument `min_completeness`"))
      obs[lubridate::year(obs$date) %in% insufficient_years, pol] = NA
    }
  }

  # Fill in missing hours with NAs
  obs = obs %>%
    tidyr::complete(date = complete_dates) %>%
    dplyr::arrange(.data$date)

  # Calculate CAAQS attainment where data provided
  attainment = list(
    pm25 = if (!is.null(pm25_1hr_ugm3)) CAAQS_pm25(obs, thresholds),
    o3   = if (!is.null(  o3_1hr_ppb )) CAAQS_o3(  obs, thresholds),
    no2  = if (!is.null( no2_1hr_ppb )) CAAQS_no2( obs, thresholds),
    so2  = if (!is.null( so2_1hr_ppb )) CAAQS_so2( obs, thresholds)
  )
  return(attainment)
}

## CAAQS Helpers ----------------------------------------------------------
CAAQS_pm25 = function(obs, thresholds){
  obs %>%
    # Hourly mean -> daily mean
    dplyr::group_by(date = lubridate::floor_date(.data$date, "days")) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), c(mean = mean_no_na))) %>%
    # Daily mean -> annual 98th percentile and annual mean
    dplyr::group_by(year = lubridate::year(.data$date)) %>%
    dplyr::summarise(perc_98_of_daily_means = unname(stats::quantile(.data$pm25_mean, 0.98, na.rm = T)),
                     mean_of_daily_means = mean(.data$pm25_mean, na.rm = T)) %>%
    dplyr::ungroup() |>
    # +3 year averages, +whether standard is met
    dplyr::mutate(
      # +3 year averages,
      `3yr_mean_of_perc_98` = get_lag_n_mean(.data$perc_98_of_daily_means, n = 3),
      `3yr_mean_of_means` = get_lag_n_mean(.data$mean_of_daily_means, n = 3),
      # +whether standards are met
      management_level_daily = sapply(
        .data$year,
        \(y) CAAQS_meets_standard(year = y,
                                  metric = .data$`3yr_mean_of_perc_98`[.data$year == y],
                                  thresholds = thresholds$pm25$daily)),
      management_level_annual = sapply(
        .data$year,
        \(y) CAAQS_meets_standard(year = y,
                                  metric = .data$`3yr_mean_of_means`[.data$year == y],
                                  thresholds = thresholds$pm25$annual))
    ) %>%
    dplyr::relocate("management_level_daily",
                    .after = "3yr_mean_of_perc_98")%>%
    dplyr::relocate("mean_of_daily_means",
                    .after = "management_level_daily")
}

CAAQS_o3 = function(obs, thresholds){
  obs %>%
    # hourly mean -> 8 hourly mean
    dplyr::group_by(date = lubridate::floor_date(.data$date, "8 hours")) %>%
    dplyr::summarise(`8hr_mean_o3` = mean(.data$o3, na.rm = T)) %>%
    # 8 hourly mean -> daily max
    dplyr::group_by(date = lubridate::floor_date(.data$date, "days")) %>%
    dplyr::summarise(daily_max_8hr_mean_o3 = max_no_na(.data$`8hr_mean_o3`)) %>%
    # daily max -> annual 4th highest
    dplyr::group_by(year = lubridate::year(.data$date)) %>%
    dplyr::arrange(dplyr::desc(.data$daily_max_8hr_mean_o3)) %>%
    dplyr::summarise(fourth_highest_daily_max_8hr_mean_o3 = .data$daily_max_8hr_mean_o3[4]) %>%
    dplyr::ungroup() |>
    # +3 year averages, +standard for that year, +whether standard is met
    dplyr::mutate(
      # +3 year averages
      `3yr_mean` = get_lag_n_mean(.data$fourth_highest_daily_max_8hr_mean_o3, n = 3),
      # +whether standard is met
      management_level_8hr = sapply(
        .data$year,
        \(y) CAAQS_meets_standard(year = y,
                                  metric = .data$`3yr_mean`[.data$year == y],
                                  thresholds = thresholds$o3$`8hr`))
    )
}

CAAQS_no2 = function(obs, thresholds){
  obs %>%
    # + annual mean
    dplyr::group_by(year = lubridate::year(.data$date)) %>%
    dplyr::mutate(annual_mean_of_hourly = mean(.data$no2, na.rm = T)) %>%
    # hourly mean -> daily maxima
    dplyr::group_by(date = lubridate::floor_date(.data$date, "1 days"),
                    .data$annual_mean_of_hourly) %>%
    dplyr::summarise(daily_max_hourly_no2 = max_no_na(.data$no2), .groups = "drop") %>%
    # daily maxima -> annual 98th percentile
    dplyr::group_by(year = lubridate::year(date),
                    .data$annual_mean_of_hourly) %>%
    dplyr::summarise(
      perc_98_of_daily_maxima = unname(stats::quantile(
        .data$daily_max_hourly_no2, 0.98, na.rm = T)), .groups = "drop") %>%\
    dplyr::ungroup() |>
    # +3 year averages, +standard for that year, +whether standard is met
    dplyr::mutate(
      # +3 year averages
      `3yr_mean_of_perc_98` = get_lag_n_mean(.data$perc_98_of_daily_maxima, n = 3),
      # +whether standard is met
      management_level_hourly = sapply(
        .data$year,
        \(y) CAAQS_meets_standard(year = y,
                                  metric = .data$`annual_mean_of_hourly`[.data$year == y],
                                  thresholds = thresholds$no2$hourly)),
      management_level_annual = sapply(
        .data$year,
        \(y) CAAQS_meets_standard(year = y,
                                  metric = .data$`3yr_mean_of_perc_98`[.data$year == y],
                                  thresholds = thresholds$no2$annual))
    ) %>%
    dplyr::relocate("management_level_hourly",
                    .after = "annual_mean_of_hourly")
}

CAAQS_so2 = function(obs, thresholds){
  obs %>%
    # + annual mean
    dplyr::group_by(year = lubridate::year(date)) %>%
    dplyr::mutate(annual_mean_of_hourly = mean(.data$so2, na.rm = T)) %>%
    # hourly mean -> daily maxima
    dplyr::group_by(date = lubridate::floor_date(date, "1 days"), .data$annual_mean_of_hourly) %>%
    dplyr::summarise(daily_max_hourly_so2 = max_no_na(.data$so2), .groups = "drop") %>%
    # daily maxima -> annual 98th percentile
    dplyr::group_by(year = lubridate::year(date), .data$annual_mean_of_hourly) %>%
    dplyr::summarise(
      perc_99_of_daily_maxima = unname(stats::quantile(
        .data$daily_max_hourly_so2, 0.99, na.rm = T)), .groups = "drop") %>%
    dplyr::ungroup() |>
    # +3 year averages, +standard for that year, +whether standard is met
    dplyr::mutate(
      # +3 year averages
      `3yr_mean_of_perc_99` = get_lag_n_mean(.data$perc_99_of_daily_maxima, n = 3),
      # +whether standard is met
      management_level_hourly = sapply(
        .data$year,
        \(y) CAAQS_meets_standard(year = y,
                                  metric = .data$`annual_mean_of_hourly`[.data$year == y],
                                  thresholds = thresholds$so2$hourly)),
      management_level_annual = sapply(
        .data$year,
        \(y) CAAQS_meets_standard(year = y,
                                  metric = .data$`3yr_mean_of_perc_99`[.data$year == y],
                                  thresholds = thresholds$so2$annual))
    ) %>%
    dplyr::relocate("management_level_hourly",
                    .after = "annual_mean_of_hourly")
}

CAAQS_meets_standard = function(metric, thresholds, year){
  . = NULL # so build check doesn't yell at me
  mgmt_levels = thresholds %>%
    .[as.numeric(names(.)) <= year] %>%
    dplyr::last()
  if(length(mgmt_levels) == 0) return(NA)

  attainment = dplyr::bind_cols(lapply(mgmt_levels, \(lvl) metric > lvl))
  attainment = apply(attainment, 1, \(x) min_no_na(which(x)))
  attainment[!is.na(attainment)] = names(mgmt_levels)[
    attainment[!is.na(attainment)]]

  return(attainment)
}

CAAQS_thesholds = function(){
  list(
    pm25 = list(
      daily  = list('2015' = c(Red = 28,  Orange = 19,   Yellow = 10.01, Green = 0),
                    '2020' = c(Red = 27,  Orange = 19,   Yellow = 10.01, Green = 0)),
      annual = list('2015' = c(Red = 10,  Orange = 6.41, Yellow = 4.01,  Green = 0),
                    '2020' = c(Red = 8.8, Orange = 6.41, Yellow = 4.01,  Green = 0))
    ),
    o3 = list(
      `8hr`  = list('2015' = c(Red = 63,  Orange = 56.01,   Yellow = 50.01, Green = 0),
                    '2020' = c(Red = 62,  Orange = 56.01,   Yellow = 50.01, Green = 0),
                    '2025' = c(Red = 60,  Orange = 56.01,   Yellow = 50.01, Green = 0))
    ),
    no2 = list(
      hourly = list('2020' = c(Red = 60,  Orange = 31.01,   Yellow = 20.01, Green = 0),
                    '2025' = c(Red = 42,  Orange = 31.01,   Yellow = 20.01, Green = 0)),
      annual = list('2020' = c(Red = 17,  Orange = 7.01,   Yellow = 2.01, Green = 0),
                    '2025' = c(Red = 12,  Orange = 7.01,   Yellow = 2.01, Green = 0))
    ),
    so2 = list(
      hourly = list('2020' = c(Red = 70,  Orange = 50.01,   Yellow = 30.01, Green = 0),
                    '2025' = c(Red = 65,  Orange = 50.01,   Yellow = 30.01, Green = 0)),
      annual = list('2020' = c(Red = 5,  Orange = 3.01,   Yellow = 2.01, Green = 0),
                    '2025' = c(Red = 4,  Orange = 3.01,   Yellow = 2.01, Green = 0))
    )
  )
}

# TODO: implement
CAAQS_objectives = function(mgmt_levels){
  objectives = c(
    Green = "To maintain good air quality through proactive air management measures to keep clean areas clean.",
    Yellow = "To improve air quality using early and ongoing actions for continuous improvement.",
    Orange = "To improve air quality through active air management and prevent exceedance of the CAAQS.",
    Red = "To reduce pollutant levels below the CAAQS through advanced air management actions."
  )
}

# US AQI ------------------------------------------------------------------

# TODO: Include AQI health messaging
# TODO: add @description, @family
# TODO: Add reference to https://www.airnow.gov/sites/default/files/2020-05/aqi-technical-assistance-document-sept2018.pdf
# Example:
# AQI(o3_8hr_ppm = 0.078, o3_1hr_ppm = 0.104, pm25_24hr_ugm3 = 35.9)

#' Calculate the US AQI from pollutant observations
#'
#' @param dates Vector of hourly dates corresponding to observations. Date gaps will be filled automatically.
#' @param o3_8hr_ppm (Optional) Numeric vector of hourly 8-hour mean ozone (O3) concentrations (ppm).
#' Will be calculated from o3_1hr_ppm if provided and o3_8hr_ppm not provided.
#' @param o3_1hr_ppm (Optional) Numeric vector of hourly 1-hour mean ozone (O3) concentrations (ppm).
#' @param pm25_24hr_ugm3 (Optional) Numeric vector of hourly 24-hour mean fine particulate matter (PM2.5) concentrations (ug/m^3).
#' Will be calculated from pm25_1hr_ugm3 if provided and pm25_24hr_ugm3 not provided.
#' @param pm25_1hr_ugm3 (Optional) Numeric vector of hourly 1-hour mean fine particulate matter (PM2.5) concentrations (ug/m^3).
#' @param pm10_24hr_ugm3 (Optional) Numeric vector of hourly 24-hour mean coarse particulate matter (PM10) concentrations (ug/m^3).
#' Will be calculated from pm10_1hr_ugm3 if provided and pm10_24hr_ugm3 not provided.
#' @param pm10_1hr_ugm3 (Optional) Numeric vector of hourly 1-hour mean coarse particulate matter (PM10) concentrations (ug/m^3).
#' @param co_8hr_ppm (Optional) Numeric vector of hourly 8-hour mean carbon monoxide (CO) concentrations (ppm).
#' Will be calculated from co_1hr_ppm if provided and co_8hr_ppm not provided.
#' @param co_1hr_ppm (Optional) Numeric vector of hourly 1-hour mean carbon monoxide (CO) concentrations (ppm).
#' @param so2_1hr_ppb (Optional) Numeric vector of hourly 1-hour mean sulfur dioxide (SO2) concentrations (ppb).
#' 24-hour averages will be calculated automatically.
#' @param no2_1hr_ppb (Optional) Numeric vector of hourly 1-hour mean nitrogen dioxide (NO2) concentrations (ppb).
#'
#' @description
#' The Air Quality Index (AQI) is used for reporting on air quality in the United States,
#' and focuses on short term health effects as a result of breathing polluted air.
#'
#' The AQI is calculated separately for 5 pollutants: ozone (O3), particulate matter (PM2.5 and PM10),
#' carbon monoxide (CO), sulfur dioxide (SO2), and nitrogen dioxide (NO2) based
#' on maximum values (of various averaging periods) for a particular day.
#' The highest AQI value among each pollutants value for that day is recorded
#' as the AQI and the corresponding pollutant is reported as the principal pollutant.
#'
#' The US EPA has established risk categories and associated health messaging for AQI ranges including:
#' "Good" (0-50), "Moderate" (51-100), "Unhealthy for Sensitive Groups" (101-150),
#' "Unhealthy" (151-200), "Very Unhealthy" (200-300), and "Hazardous" (301-500).
#' AQI values above 500 are considered "Beyond the AQI",
#' but AQI values will still be calculated for relative comparisons.
#'
#' @return A tibble (data.frame) with columns:
#' date, AQI, risk_category, principal_pol
#' and 1 row for each day between the min and max values of the provided dates
#' @export
#'
#' @family Air Quality Standards
#' @family USA Air Quality
#'
#' @examples
#' AQI(o3_8hr_ppm = 0.078, o3_1hr_ppm = 0.104, pm25_24hr_ugm3 = 35.9)
#' AQI(o3_1hr_ppm = 0.104, pm25_24hr_ugm3 = 35.9)
AQI = function(dates = Sys.time(),
               o3_8hr_ppm = NA, o3_1hr_ppm = NA,
               pm25_24hr_ugm3 = NA, pm25_1hr_ugm3 = NA,
               pm10_24hr_ugm3 = NA, pm10_1hr_ugm3 = NA,
               co_8hr_ppm = NA, co_1hr_ppm = NA,
               so2_1hr_ppb = NA, no2_1hr_ppb = NA){
  # TODO: Reference https://forum.airnowtech.org/t/the-aqi-equation-2024-valid-beginning-may-6th-2024/453

  # Determine which pollutants provided as input
  AQI_pols = c("o3_8hr_ppm", "o3_1hr_ppm",
           "pm25_24hr_ugm3", "pm25_1hr_ugm3",
           "pm10_24hr_ugm3", "pm10_1hr_ugm3",
           "co_8hr_ppm"    , "co_1hr_ppm",
           "so2_1hr_ppb"   , "no2_1hr_ppb")
  all_missing = lapply(AQI_pols, \(pol) all(is.na(get(pol)))) %>%
    stats::setNames(AQI_pols)

  # Ensure at least one pollutants data is provided
  if(all(unlist(all_missing))){
    stop(paste("At least one pollutant's concentrations must",
                "be provided and have at least 1 non-NA value."))
  }

  # Combine inputs
  dat = data.frame(date = dates,
                   o3_8hr_ppm, o3_1hr_ppm,
                   pm25_24hr_ugm3, pm25_1hr_ugm3,
                   pm10_24hr_ugm3, pm10_1hr_ugm3,
                   co_8hr_ppm, co_1hr_ppm,
                   so2_1hr_ppb, no2_1hr_ppb) %>%
    # Fill hourly date gaps with NA obs
    tidyr::complete(date = seq(min(dates), max(dates), "1 hours"))

  ## Make additional running averages if needed
  # If no non-NA o3_8hr_ppm values provided, but 1hr ozone is provided
  if(all_missing$o3_8hr_ppm & !all_missing$o3_1hr_ppm){
    # Calculate o3_8hr_ppm from o3_1hr_ppm
    dat$o3_8hr_ppm = roll_mean_8hr_min_5(dat$o3_1hr_ppm)
  }
  # If no non-NA pm25_24hr_ugm3 values provided, but 1hr pm25 is provided
  if(all_missing$pm25_24hr_ugm3 & !all_missing$pm25_1hr_ugm3){
    # Calculate pm25_24hr_ugm3 from pm25_1hr_ugm3
    dat$pm25_24hr_ugm3 = roll_mean_24hr_min_15(dat$pm25_1hr_ugm3)
  }
  # If no non-NA pm10_24hr_ugm3 values provided, but 1hr pm10 is provided
  if(all_missing$pm10_24hr_ugm3 & !all_missing$pm10_1hr_ugm3){
    # Calculate pm10_24hr_ugm3 from pm10_1hr_ugm3
    dat$pm10_24hr_ugm3 = roll_mean_24hr_min_15(dat$pm10_1hr_ugm3)
  }
  # If no non-NA co_8hr_ppm values provided, but 1hr CO is provided
  if(all_missing$co_8hr_ppm & !all_missing$co_1hr_ppm){
    # Calculate co_8hr_ppm from co_1hr_ppm
    dat$co_8hr_ppm = roll_mean_8hr_min_5(dat$co_1hr_ppm)
  }
  # If non-NA so2_1hr_ppb values provided
  if(!all_missing$so2_1hr_ppb){
    # Calculate so2_24hr_ppb from so2_1hr_ppb
    dat$so2_24hr_ppb = roll_mean_24hr_min_15(dat$so2_1hr_ppb)
    all_missing$so2_24hr_ppb = FALSE
  }else{
    # Otherwise use NA
    dat$so2_24hr_ppb = NA
    all_missing$so2_24hr_ppb = TRUE
  }

  # Get Daily mean values for all pollutants/averaging times
  dat = dplyr::group_by(dat, date = lubridate::floor_date(.data$date, "days")) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), mean_no_na)) %>%
    dplyr::ungroup() |>
    dplyr::arrange(.data$date) # Sort by date

  # Truncate daily means
  dat = dat %>%
    # Truncate ozone to 3 decimals
    dplyr::mutate(dplyr::across(dplyr::starts_with("o3"),
                                \(x) trunc_n(x, 3))) %>%
    # Truncate PM2.5 and CO to 1 decimal
    dplyr::mutate(dplyr::across(dplyr::starts_with("pm25|co"),
                                \(x) trunc_n(x, 1))) %>%
    # Truncate PM10, SO2, and NO2 to no decimals
    dplyr::mutate(dplyr::across(dplyr::starts_with("so2|no2|pm10"),
                                \(x) trunc_n(x, 0)))

  # Calculate AQI for each pollutant provided
  provided_pols = names(dat)[-1] # get pols that are provided
  for (pol in provided_pols) {
    # If provided
    if (!all_missing[[pol]]) {
      # Calculate AQI from pollutant concentrations
      dat = AQI_from_con(dat, pol)
    # Otherwise use NA
    }else dat[[paste0("AQI_", pol)]] = NA
  }
  AQI_cols = paste0("AQI_", provided_pols)
  names(AQI_cols) = stringr::str_split(AQI_cols, "_", simplify = T)[,2]

  # Set hourly AQI to the highest of the calculated values
  dat = dplyr::rowwise(dat) %>% # for each hour
    dplyr::mutate(
      # Take the max non-NA value as the AQI
      AQI = max(dplyr::across(dplyr::all_of(AQI_cols)), na.rm = T),
      # Get risk category
      risk_category = AQI_risk_category(.data$AQI),
      # Determine the principal pollutant for the AQI
      principal_pol = names(AQI_cols)[which.max( # get pol where max
        dplyr::across(dplyr::all_of(AQI_cols)))] %>% # across AQI columns
        factor(unique(names(AQI_cols)))) %>% # make it a factor
    dplyr::ungroup()

  # Return a tibble with datetimes and corresponding AQI
  return(dplyr::select(dat, "date", "AQI",
                       "risk_category", "principal_pol"))
}

## AQI Helpers ------------------------------------------------------------
aqi_levels = list(
  "Good" = 0:50,
  "Moderate" = 51:100,
  "Unhealthy for Sensitive Groups" = 101:150,
  "Unhealthy" = 151:200,
  "Very Unhealthy" = 201:300,
  "Hazardous" = 301:500,
  "Beyond the AQI" = 500:5000
)

# Returns Risk category when AQI value provided
AQI_risk_category = function(AQI){
  risk = factor(
    AQI, unlist(aqi_levels),
    unlist(
      sapply(seq_along(aqi_levels), \(i){
        level = names(aqi_levels)[i] %>%
          stringr::str_remove("2$")
        rep(level, length(aqi_levels[[i]]))
      })
    )
  )
  return(risk)
}

# Define breakpoints for AQI formulation
AQI_breakpoints = list(
  ## 8 hour mean ozone
  # 8-hour O3 values do not define higher AQI values (≥ 301).
  # AQI values of 301 or higher are calculated with 1-hour O3 concentrations.
  # The highest of the 1hr/8hr AQI for ozone is used
  # (1 hour is only really used for some areas)
  o3_8hr_ppm = data.frame(
    risk_category = names(aqi_levels)[1:5],
    bp_low   = c(0    , 0.055, 0.071, 0.086, 0.106),
    bp_high  = c(0.054, 0.07 , 0.085, 0.105, 0.2  ),
    aqi_low  = c(0    , 51   , 101  , 151  , 201  ),
    aqi_high = c(50   , 100  , 150  , 200  , 300  )
  ),
  ## 1 Hour Mean Ozone
  # 1-hour O3 values do not define Good-Moderate AQI values (< 101).
  o3_1hr_ppm = data.frame(
    risk_category = names(aqi_levels)[3:7],
    bp_low   = c(0.125, 0.165, 0.205, 0.405, 0.605),
    bp_high  = c(0.164, 0.204, 0.404, 0.504, Inf  ),
    aqi_low  = c(101  , 151  , 201  , 301  , 301  ),
    aqi_high = c(150  , 200  , 300  , 500  , 500  )
  ),
  ## 24 Hour Mean Fine Particulate Matter
  # If a different SHL for PM2.5 is promulgated, Unhealthy and above will change accordingly.
  pm25_24hr_ugm3 = data.frame(
    risk_category = names(aqi_levels)[1:7],
    bp_low   = c(0 , 9.1 , 35.5, 55.5 , 125.5, 225.5, 325.5),
    bp_high  = c(9 , 35.4, 55.4, 125.4, 225.4, 325.4, Inf  ),
    aqi_low  = c(0 , 51  , 101 , 151  , 201  , 301  , 301  ),
    aqi_high = c(50, 100 , 150 , 200  , 300  , 500  , 500  )
  ),
  ## 24 Hour Mean Fine-Coarse Particulate Matter
  pm10_24hr_ugm3 = data.frame(
    risk_category = names(aqi_levels)[1:7],
    bp_low   = c(0 , 55 , 155, 255, 355, 425, 605),
    bp_high  = c(54, 154, 254, 354, 424, 604, Inf),
    aqi_low  = c(0 , 51 , 101, 151, 201, 301, 301),
    aqi_high = c(50, 100, 150, 200, 300, 500, 500)
  ),
  ## 8 Hour Mean Carbon Monoxide
  co_8hr_ppm = data.frame(
    risk_category = names(aqi_levels)[1:7],
    bp_low   = c(0 , 4.5 , 9.5, 12.5, 15.5, 30.5, 50.5),
    bp_high  = c(4.4, 9.4, 12.4, 15.4, 30.4, 50.4, Inf),
    aqi_low  = c(0 , 51 , 101, 151, 201, 301, 301),
    aqi_high = c(50, 100, 150, 200, 300, 500, 500)
  ),
  ## 1 Hour Mean Sulfur Dioxide
  # 1-hour SO2 values do not define higher AQI values (≥ 200).
  so2_1hr_ppb = data.frame(
    risk_category = names(aqi_levels)[c(1:4)],
    bp_low   = c(0 , 36 , 76 , 186),
    bp_high  = c(35, 75 , 185, 304),
    aqi_low  = c(0 , 51 , 101, 151),
    aqi_high = c(50, 100, 150, 200)
  ),
  ## 24 Hour Mean Sulfur Dioxide
  # AQI values of 200 or greater are calculated with 24-hour SO2 concentrations.
  so2_24hr_ppb = data.frame(
    risk_category = names(aqi_levels)[5:7],
    bp_low   = c(305, 605, 1005),
    bp_high  = c(604, 1004, Inf ),
    aqi_low  = c(201, 301, 301),
    aqi_high = c(300, 500, 500)
  ),
  ## 1 Hour Mean Nitrogen Dioxide
  no2_1hr_ppb = data.frame(
    risk_category = names(aqi_levels)[1:7],
    bp_low   = c(0 , 54 , 101, 361, 650 , 1250, 2050),
    bp_high  = c(53, 100, 360, 649, 1249, 2049, Inf ),
    aqi_low  = c(0 , 51 , 101, 151, 201 , 301 , 301 ),
    aqi_high = c(50, 100, 150, 200, 300 , 500 , 500 )
  )
)

# Get risk category for breakpoint determination for AQI formulation
AQI_bp_cat = function(obs, bps){
  suppressMessages(
    bps$risk_category %>%
      lapply(\(cat) {
        bp = bps[bps$risk_category == cat, ]
        ifelse(obs >= bp$bp_low & obs <= bp$bp_high, rep(cat, length(obs)), NA)
      }) %>%
      dplyr::bind_cols() %>%
      apply(1, \(row){
        ifelse(all(is.na(row)), NA, row[!is.na(row)])
      }) %>%
      unlist())
}

# When provided concentrations and corresponding breakpoints, return AQI
AQI_formulation = function(obs, bp_low, bp_high, aqi_low, aqi_high){
  ceiling(
    (aqi_high - aqi_low) / (bp_high - bp_low) * (obs - bp_low) + aqi_low
  )
}

# Workhorse function to determine risk category, append breakponints, then calc AQI
AQI_from_con = function(dat, pol){
  . = NULL # so build check doesn't yell at me
  dat[paste0("cat_",pol)] = NA
  dat[paste0("AQI_",pol)] = NA
  dat = dat %>%
    # Determine the risk category based on the concentrations and break points
    dplyr::mutate(dplyr::across(paste0("cat_",pol),
                     \(x) AQI_bp_cat(dat[[pol]], AQI_breakpoints[[pol]]))) %>%
    # Append the corresponding break points and AQI breaks for each hour
    dplyr::left_join(AQI_breakpoints[[pol]] %>%
                       dplyr::rename_with(.cols = 2:5, \(x)paste0(x,"_", pol)),
                     by = dplyr::join_by(!!paste0("cat_",pol) == "risk_category")) 
  dat %>%
    # Calculate AQI for each hour based on those
    dplyr::mutate(across(paste0("AQI_",pol),
                     \(x) AQI_formulation(
                       dat[[pol]],
                       dat[[paste0("bp_low_", pol)]],
                       dat[[paste0("bp_high_", pol)]],
                       dat[[paste0("aqi_low_", pol)]],
                       dat[[paste0("aqi_high_", pol)]])))
}

