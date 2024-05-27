
# Canadian AQHI -----------------------------------------------------------

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

#' Calculate Canadian AQHI from hourly PM2.5, NO2, and O3 observations
#'
#' @param datetimes Vector of hourly datetime values corresponding to observations. Date gaps will be filled automatically.
#' @param pm25_hourly Vector of hourly mean fine particulate matter (PM2.5) concentrations (ug/m^3).
#' @param no2_hourly (Optional). Vector of hourly mean nitrogen dioxide (NO2) concentrations (ppb). If not provided AQHI+ will be calculated from PM2.5 only.
#' @param o3_hourly (Optional). Vector of hourly mean ozone (O3) concentrations (ppb). If not provided AQHI+ will be calculated from PM2.5 only.
#' @param quiet (Optional). A single logical (TRUE/FALSE) value indicating if AQHI+ warning (if o3 and no2 not provided) should be hidden. Default is FALSE
#'
#' @description
#' The Canadian Air Quality Health Index (AQHI) combines the health risk of
#' fine particulate matter (PM2.5), ozone (O3), and nitrogen dioxide (NO2) on a scale from 1-10 (+).
#' The AQHI is "the sum of excess mortality risk associated with individual pollutants
#' from a time-series analysis of air pollution and mortality in Canadian cities,
#' adjusted to a 0–10 scale, and calculated hourly on the basis of trailing 3-hr average pollutant concentrations."
#'
#' The Canadian AQHI+ is a modification of the Canadian Air Quality Health Index (AQHI).
#' AQHI+ only uses fine particulate matter (PM2.5) instead of the combination of PM2.5, ozone (O3), and nitrogen dioxide (NO2)
#' , and is calculated using hourly means instead of using 3 hour means.
#' The AQHI+ overrides the AQHI if it exceeds the AQHI, which typically occurs during wildfire smoke events.
#'
#' The AQHI was originally published by Steib et. al in 2008 (\link{https://doi.org/10.3155/1047-3289.58.3.435}),
#'  and has been adopted by all Canadian provinces/territories (except Quebec where they use the AQI instead of the AQHI/AQHI+).
#'
#' @family Canadian AQHI Functions
#' @return A tibble (data.frame) with columns (*if all 3 pollutants provided):
#' date, pm25, o3*, no2*, pm25_rolling_3hr*, o3_rolling_3hr*, o3_rolling_3hr*,
#'  AQHI, AQHI_plus, risk, high_risk_pop_message, general_pop_message, AQHI_plus_exceeds_AQHI*
#'  and potentially more rows than `length(datetimes)` (due to any missing hours being filled with NA values).
#' @export
#'
#' @examples
#' obs = data.frame(
#'   date = seq(lubridate::ymd_h("2024-01-01 00"),
#'              lubridate::ymd_h("2024-01-01 23"), "1 hours"),
#'   pm25 = sample(1:150, 24), o3 = sample(1:150, 24), no2 = sample(1:150, 24))
#' AQHI(datetimes = obs$date, pm25_hourly = obs$pm25,
#'      o3_hourly = obs$o3, no2_hourly = obs$no2)
#'
#' AQHI(datetimes = obs$date, pm25_hourly = obs$pm25) # Returns AQHI+
AQHI = function(datetimes, pm25_hourly, no2_hourly = NA, o3_hourly = NA, quiet = FALSE){
  # See: https://www.tandfonline.com/doi/abs/10.3155/1047-3289.58.3.435
  . = NULL # so build check doesn't yell at me
  aqhi_breakpoints = stats::setNames(
    c(-Inf, 1:10*10, Inf),
    c(NA, 1:10, "+")
  )
  # Join inputs
  obs = dplyr::bind_cols(
    date = datetimes, pm25 = pm25_hourly,
    o3 = o3_hourly, no2 = no2_hourly
  ) %>%
    # Fill in missing hours with NAs
    tidyr::complete(date = seq(min(date), max(date), "1 hours")) %>%
    dplyr::arrange(date)

  # Calculate AQHI+ (pm25 only)
  aqhi_plus = AQHI_plus(obs$pm25) %>%
    # Add columns in case only PM2.5 provided
    dplyr::mutate(AQHI = .data$AQHI_plus, AQHI_plus_exceeds_AQHI = NA) %>%
    dplyr::relocate(.data$AQHI, .before = "AQHI_plus")

  # Need all 3 pollutants to calculate AQHI
  have_all_3_pol = !all(is.na(pm25_hourly)) &
    !all(is.na(no2_hourly)) & !all(is.na(o3_hourly))
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
#' @param pm25_hourly A numeric/integer vector with hourly mean PM2.5 concentrations (ug/m^3).
#' @param min_allowed_pm25 A single numeric value indicating the minimum allowed concentration (Defaults to 0 ug/m^3). All values in `pm25_hourly` less than this will be replaced with NA.
#'
#' @description
#' The Canadian AQHI+ is a modification of the Canadian Air Quality Health Index (AQHI).
#' AQHI+ only uses fine particulate matter (PM2.5) instead of the combination of PM2.5, ozone (O3), and nitrogen dioxide (NO2)
#' , and is calculated using hourly means instead of using 3 hour means.
#' The AQHI+ overrides the AQHI if it exceeds the AQHI, which typically occurs during wildfire smoke events.
#'
#' Hourly PM2.5 is split into bins of 10 ug/m^3 up to 100 ug/m^3 (AQHI+ 1 - 10)
#' , and values greater than 100 ug/m^3 have an AQHI+ of "+".
#' The risk categories match the AQHI Low [1-3, or 0-30 ug/m^3], Moderate [4-6, or 30.1-60 ug/m^3]
#' , High [7-10, or 60.1-100 ug/m^3], and Very High [+, or >100 ug/m^3] and share the same health messaging.
#'
#' The AQHI+ was originally published by Yao et. al in 2019 (\link{https://doi.org/10.17269/s41997-019-00237-w}),
#'  and has been adopted by all Canadian provinces/territories as of 2024 (except Quebec where they use the AQI instead of the AQHI/AQHI+).
#'
#' @family Canadian AQHI Functions
#' @return A tibble (data.frame) with columns:
#' hourly_pm25, AQHI_plus, risk, high_risk_pop_message, general_pop_message
#' and `length(pm25_hourly)` rows
#' @export
#'
#' @examples
#' obs = data.frame(
#'   date = seq(lubridate::ymd_h("2024-01-01 00"),
#'              lubridate::ymd_h("2024-01-01 23"), "1 hours"),
#'   pm25 = sample(1:150, 24))
#' AQHI_plus(obs$pm25)
#'
#' obs = data.frame(
#'   date = seq(lubridate::ymd_h("2024-01-01 00"),
#'              lubridate::ymd_h("2024-01-01 23"), "1 hours"),
#'   pm25 = c(-2, -0.1, sample(1:150, 22)))
#' AQHI_plus(obs$pm25, min_allowed_pm25 = -0.5)
#' @importFrom rlang .data
AQHI_plus = function(pm25_hourly, min_allowed_pm25 = 0){

  # Define breakpoint for AQHI levels
  aqhi_breakpoints = stats::setNames(
    c(-Inf, 1:10*10, Inf),
    c(NA, 1:10, "+")
  )

  # Remove values < min_allowed_pm25 (normally 0)
  pm25_hourly[pm25_hourly < min_allowed_pm25] = NA

  # Get AQHI+
  aqhi_p = cut(pm25_hourly,
      breaks = aqhi_breakpoints,
      labels = names(aqhi_breakpoints)[-1])

  # Get risk levels
  risk = AQHI_risk_category(aqhi_p)

  # Get health messages
  health_messages = AQHI_health_messaging(risk)

  # Combine and return
  data.frame(
    pm25_hourly = pm25_hourly,
    AQHI_plus = aqhi_p,
    risk = risk,
    health_messages
  )

}

# CAAQS -------------------------------------------------------------------

CAAQS = function(datetimes, pm25_hourly = NULL, o3_hourly = NULL,
                 no2_hourly = NULL, so2_hourly = NULL){
  # TODO: Ensure 3 years of data provided
  . = NULL # so build check doesn't yell at me
  # Define thresholds for each pollutant / avg / year
  thresholds = list(
    pm25 = list(
      daily  = c('2015' = 28, '2020' = 27),
      annual = c('2015' = 10, '2020' = 8.8)
    ),
    o3 = list(
      `8hr`  = c('2015' = 63, '2020' = 62, '2025' = 60)
    ),
    no2 = list(
      hourly = c('2020' = 60, '2025' = 42),
      annual = c('2020' = 17, '2025' = 12)
    ),
    so2 = list(
      hourly = c('2020' = 60, '2025' = 42),
      annual = c('2020' = 17, '2025' = 12)
    )
  )
  # helper functions to remove NA by default
  mean_no_na = function(x, ...) mean(x, na.rm = T, ...)
  max_no_na = function(x, ...) swap_inf(suppressWarnings(max(x, na.rm = T, ...)), NA)
  # helper function to extract relevant thresholds
  get_threshold = function(thresholds, year){
    out = thresholds %>%
      .[as.numeric(names(.)) <= year] %>%
      dplyr::last()
    if(length(out) == 0) out = NA
    return(out)
  }
  # helper function to make 3 year means
  get_lag_n_mean = function(x, n = 3){
    out = x
    if(n <= 1) stop("`n` must be greater than one")
    for(i in 1:(n-1)){
      out = out + dplyr::lag(x, i)
    }
    return(out/n)
  }
  # Join inputs
  obs = dplyr::bind_cols(
    date = datetimes, pm25 = pm25_hourly,
    o3 = o3_hourly, no2 = no2_hourly,
    so2 = so2_hourly
  ) %>%
    # Fill in missing hours with NAs
    tidyr::complete(date = seq(lubridate::floor_date(min(.data$date), "years"),
                        lubridate::ceiling_date(max(.data$date), "years") - lubridate::hours(1),
                             "1 hours")) %>%
    dplyr::arrange(.data$date)

  # Get daily mean and max
  daily = obs %>%
    dplyr::group_by(date = lubridate::floor_date(.data$date, "days")) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   c(mean = mean_no_na, max = max_no_na)))

  met = list()
  # Test pm25 if provided
  if(!is.null(pm25_hourly)){
    met$pm25 = daily %>%
      # Daily mean -> annual 98th percentile and annual mean
      dplyr::group_by(year = lubridate::year(.data$date)) %>%
      dplyr::summarise(p_complete = round(sum(!is.na(.data$pm25_mean)) / dplyr::n(), 3)*100,
                perc_98_of_daily_means = unname(stats::quantile(.data$pm25_mean, 0.98, na.rm = T)),
                mean_of_daily_means = mean(.data$pm25_mean, na.rm = T)) %>%
      # +3 year averages, +standard for that year, +whether standard is met
      dplyr::mutate(
        # +3 year averages,
        `3yr_mean_of_perc_98` = get_lag_n_mean(.data$perc_98_of_daily_means, n = 3),
        `3yr_mean_of_means` = get_lag_n_mean(.data$mean_of_daily_means, n = 3),
        # +standards for that year
        daily_standard = sapply(.data$year, get_threshold,
                                thresholds = thresholds$pm25$daily),
        annual_standard = sapply(.data$year, get_threshold,
                                 thresholds = thresholds$pm25$annual),
        # +whether standards are met
        meets_standard_daily = .data$`3yr_mean_of_perc_98` <= .data$daily_standard,
        meets_standard_annual = .data$`3yr_mean_of_means` <= .data$annual_standard
      ) %>%
      dplyr::relocate(c(.data$daily_standard, .data$meets_standard_daily),
                      .after = "3yr_mean_of_perc_98")%>%
      dplyr::relocate(.data$mean_of_daily_means,
                      .after = "meets_standard_daily") %>%
      dplyr::relocate(.data$p_complete, .after = "year")
  }
  # Test o3 if provided
  if(!is.null(o3_hourly)){
    met$o3 = obs %>%
      # hourly mean -> 8 hourly mean
      dplyr::group_by(date = lubridate::floor_date(.data$date, "8 hours")) %>%
      dplyr::summarise(`8hr_mean_o3` = mean(.data$o3, na.rm = T)) %>%
      # 8 hourly mean -> daily max
      dplyr::group_by(date = lubridate::floor_date(.data$date, "days")) %>%
      dplyr::summarise(daily_max_8hr_mean_o3 = max_no_na(.data$`8hr_mean_o3`)) %>%
      # daily max -> annual 4th highest
      dplyr::group_by(year = lubridate::year(.data$date)) %>%
      dplyr::arrange(dplyr::desc(.data$daily_max_8hr_mean_o3)) %>%
      dplyr::summarise(
        p_complete = round(sum(!is.na(.data$daily_max_8hr_mean_o3)) / dplyr::n(), 3)*100,
        fourth_highest_daily_max_8hr_mean_o3 = .data$daily_max_8hr_mean_o3[4]) %>%
      # +3 year averages, +standard for that year, +whether standard is met
      dplyr::mutate(
        # +3 year averages
        `3yr_mean` = get_lag_n_mean(.data$fourth_highest_daily_max_8hr_mean_o3, n = 3),
        # +standard for that year
        standard = sapply(.data$year, get_threshold,
                                 thresholds = thresholds$o3$`8hr`),
        # +whether standard is met
        meets_standard = .data$`3yr_mean` <= .data$standard
      ) %>%
      dplyr::relocate(.data$p_complete, .after = "year")
  }
  # Test no2 if provided
  if(!is.null(no2_hourly)){
    met$no2 = obs %>%
      # + annual mean
      dplyr::group_by(year = lubridate::year(.data$date)) %>%
      dplyr::mutate(annual_mean_of_hourly = mean(.data$no2, na.rm = T)) %>%
      # hourly mean -> daily maxima
      dplyr::group_by(date = lubridate::floor_date(.data$date, "1 days"),
                      .data$annual_mean_of_hourly) %>%
      dplyr::summarise(daily_max_hourly_no2 = max(.data$no2, na.rm = T), .groups = "drop") %>%
      # daily maxima -> annual 98th percentile
      dplyr::group_by(year = lubridate::year(date),
                      .data$annual_mean_of_hourly) %>%
      dplyr::summarise(
        p_complete = round(sum(!is.na(.data$daily_max_hourly_no2)) / dplyr::n(), 3)*100,
        perc_98_of_daily_maxima = unname(stats::quantile(
          .data$daily_max_hourly_no2, 0.98, na.rm = T)), .groups = "drop") %>%
      # +3 year averages, +standard for that year, +whether standard is met
      dplyr::mutate(
        # +3 year averages
        `3yr_mean_of_perc_98` = get_lag_n_mean(.data$perc_98_of_daily_maxima, n = 3),
        # +standards for that year
        standard_hourly = sapply(.data$year, get_threshold,
                                 thresholds = thresholds$no2$hourly),
        standard_annual = sapply(.data$year, get_threshold,
                                 thresholds = thresholds$no2$annual),
        # +whether standard is met
        meets_standard_hourly = .data$annual_mean_of_hourly <= .data$standard_hourly,
        meets_standard_annual = .data$`3yr_mean_of_perc_98` <= .data$standard_annual,
      ) %>%
      dplyr::relocate(c(.data$standard_hourly, .data$meets_standard_hourly),
                      .after = "annual_mean_of_hourly") %>%
      dplyr::relocate(.data$p_complete, .after = "year")
  }
  # Test so2 if provided
  if(!is.null(so2_hourly)){
    met$so2 = obs %>%
      # + annual mean
      dplyr::group_by(year = lubridate::year(date)) %>%
      dplyr::mutate(annual_mean_of_hourly = mean(.data$so2, na.rm = T)) %>%
      # hourly mean -> daily maxima
      dplyr::group_by(date = lubridate::floor_date(date, "1 days"), .data$annual_mean_of_hourly) %>%
      dplyr::summarise(daily_max_hourly_so2 = max(.data$so2, na.rm = T), .groups = "drop") %>%
      # daily maxima -> annual 98th percentile
      dplyr::group_by(year = lubridate::year(date), .data$annual_mean_of_hourly) %>%
      dplyr::summarise(
        p_complete = round(sum(!is.na(.data$daily_max_hourly_so2)) / dplyr::n(), 3)*100,
        perc_99_of_daily_maxima = unname(stats::quantile(
          .data$daily_max_hourly_so2, 0.99, na.rm = T)), .groups = "drop") %>%
      # +3 year averages, +standard for that year, +whether standard is met
      dplyr::mutate(
        # +3 year averages
        `3yr_mean_of_perc_99` = get_lag_n_mean(.data$perc_99_of_daily_maxima, n = 3),
        # +standards for that year
        standard_hourly = sapply(.data$year, get_threshold,
                                 thresholds = thresholds$so2$hourly),
        standard_annual = sapply(.data$year, get_threshold,
                                 thresholds = thresholds$so2$annual),
        # +whether standard is met
        meets_standard_hourly = .data$annual_mean_of_hourly <= .data$standard_hourly,
        meets_standard_annual = .data$`3yr_mean_of_perc_99` <= .data$standard_annual,
      ) %>%
      dplyr::relocate(c(.data$standard_hourly, .data$meets_standard_hourly),
                      .after = "annual_mean_of_hourly") %>%
      dplyr::relocate(.data$p_complete, .after = "year")
  }
  return(met)
}

