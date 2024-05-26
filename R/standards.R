
## DEFINITIONS --------------

# 10, 20, ..., 90, 100, >100 (ug/m3)
aqhi_breakpoints = setNames(
  c(-Inf, 1:10*10, Inf),
  c(NA, 1:10, "+")
)

# Low [1-3], Mod. [4-6], High [7-10], V. High [+]
aqhi_levels = list(
  Low = 1:3,
  Moderate = 4:6,
  High = 7:10,
  "Very High" = "+"
)

# Health messaging
aqhi_messaging = list(
  Low = data.frame(
    high_risk_pop = "Enjoy your usual activities.",
    general_pop = "Ideal air quality for outdoor activities."
  ),
  Moderate = data.frame(
    high_risk_pop = "Consider reducing or rescheduling activities outdoors if you experience symptoms.",
    general_pop = "No need to modify your usual activities unless you experience symptoms."
  ),
  High = data.frame(
    high_risk_pop = "Reduce or reschedule activities outdoors.",
    general_pop = "Consider reducing or rescheduling activities outdoors if you experience symptoms."
  ),
  "Very High" = data.frame(
    high_risk_pop = "Avoid strenuous activity outdoors.",
    general_pop = "Reduce or reschedule activities outdoors, especially if you experience symptoms."
  )
)

# FUNCTIONS --------

# Canadian AQHI -----------------------------------------------------------

# Calculates Canadian AQHI (overriden by AQHI+ if higher)
AQHI = function(datetimes, pm25_hourly, no2_hourly = NA, o3_hourly = NA, quiet = FALSE){
  # Join inputs
  obs = bind_cols(
    date = datetimes, pm25 = pm25_hourly,
    o3 = o3_hourly, no2 = no2_hourly
  ) %>%
    # Fill in missing hours with NAs
    tidyr::complete(date = seq(min(date), max(date), "1 hours")) %>%
    arrange(date)

  # Calculate AQHI+ (pm25 only)
  aqhi_plus = AQHI_plus(obs$pm25)

  # Need all 3 pollutants to calculate AQHI
  if(!all(is.na(no2_hourly)) & !all(is.na(o3_hourly))){
    # Rolling 3 hour average PM2.5 (at least 2 hours per average)
    obs$pm25_rolling_3hr = zoo::rollapply(obs$pm25, width = 3,
                                     align = "right", fill = NA,
                                     FUN = mean_if_enough, min_n = 2)%>%
      round(1)
    # Rolling 3 hour average NO2 (at least 2 hours per average)
    obs$no2_rolling_3hr = zoo::rollapply(obs$no2, width = 3,
                                     align = "right", fill = NA,
                                     FUN = mean_if_enough, min_n = 2)%>%
      round(1)
    # Rolling 3 hour average O3(at least 2 hours per average)
    obs$o3_rolling_3hr = zoo::rollapply(obs$o3, width = 3,
                                     align = "right", fill = NA,
                                     FUN = mean_if_enough, min_n = 2) %>%
      round(1)
    # Calculate AQHI
    obs$AQHI = cut(round(10/10.4 * 100 * (
      (exp(0.000537 * o3_rolling_3hr) - 1) +
        (exp(0.000871 * no2_rolling_3hr) - 1) +
          (exp(0.000487 * pm25_rolling_3hr) - 1)
    )), aqhi_breakpoints/10, unlist(aqhi_levels))

    # Get risk levels
    obs$risk = factor(
      obs$AQHI, unlist(aqhi_levels),
      unlist(
        sapply(seq_along(aqhi_levels), function(i){
          rep(names(aqhi_levels)[i], length(aqhi_levels[[i]]))
        })
      )
    )

    # Get health messages
    health_messages = lapply(aqhi_messaging[obs$risk], function(x){
      if(is.null(x)){
        data.frame(high_risk_pop = NA, general_pop = NA)
      }else return(x)
    }) %>% dplyr::bind_rows()

    # Combine
    obs = data.frame(
      obs,
      health_messages
    )

    # Use AQHI+ if it exceeds AQHI
    obs = obs %>%
      mutate(
        AQHI_plus_exceeds_AQHI = as.numeric(aqhi_plus$AQHI_plus) > as.numeric(AQHI),
        AQHI_plus_exceeds_AQHI = swap_na(AQHI_plus_exceeds_AQHI, TRUE),
        AQHI = case_when(AQHI_plus_exceeds_AQHI ~ aqhi_plus$AQHI_plus,
                         TRUE ~ AQHI),
        risk = case_when(AQHI_plus_exceeds_AQHI ~ aqhi_plus$risk,
                         TRUE ~ risk),
        high_risk_pop = case_when(AQHI_plus_exceeds_AQHI ~ aqhi_plus$high_risk_pop,
                                  TRUE ~ obs$high_risk_pop),
        general_pop = case_when(AQHI_plus_exceeds_AQHI ~ aqhi_plus$general_pop,
                             TRUE ~ obs$general_pop)
      )

  }else{
    if(!quiet) warning("Returning AQHI+ (PM2.5 only) as no non-missing NO2 / O3 provided.")
    obs = aqhi_plus
  }
  return(obs)
}

# Calculates Canadian AQHI+ (PM2.5 only)
AQHI_plus = function(
    pm25_hourly,
    min_allowed_pm25 = 0)
{
  # Remove values < min_allowed_pm25 (normally 0)
  pm25_hourly[pm25_hourly < min_allowed_pm25] = NA

  # Get AQHI+
  aqhi_p = cut(pm25_hourly,
      breaks = aqhi_breakpoints,
      labels = unlist(aqhi_levels))

  # Get risk levels
  risk = factor(
      aqhi_p, unlist(aqhi_levels),
      unlist(
        sapply(seq_along(aqhi_levels), function(i){
          rep(names(aqhi_levels)[i], length(aqhi_levels[[i]]))
        })
      )
    )

  # Get health messages
  health_messages = lapply(aqhi_messaging[risk], function(x){
    if(is.null(x)){
      data.frame(high_risk_pop = NA, general_pop = NA)
    }else return(x)
  })
  health_messages = dplyr::bind_rows(health_messages)

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
  require(dplyr)
  require(lubridate)
  require(tidyr)

  # TODO: Ensure 3 years of data provided

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
      last()
    if(length(out) == 0) out = NA
    return(out)
  }
  # helper function to make 3 year means
  get_lag_n_mean = function(x, n = 3){
    out = x
    if(n <= 1) stop("`n` must be greater than one")
    for(i in 1:(n-1)){
      out = out + lag(x, i)
    }
    return(out/n)
  }
  # Join inputs
  obs = bind_cols(
    date = datetimes, pm25 = pm25_hourly,
    o3 = o3_hourly, no2 = no2_hourly,
    so2 = so2_hourly
  ) %>%
    # Fill in missing hours with NAs
    complete(date = seq(lubridate::floor_date(min(date), "years"),
                        lubridate::ceiling_date(max(date), "years") - lubridate::hours(1),
                             "1 hours")) %>%
    arrange(date)

  # Get daily mean and max
  daily = obs %>%
    group_by(date = lubridate::floor_date(date, "days")) %>%
    summarise(across(everything(), c(mean = mean_no_na, max = max_no_na)))

  met = list()
  # Test pm25 if provided
  if(!is.null(pm25_hourly)){
    met$pm25 = daily %>%
      # Daily mean -> annual 98th percentile and annual mean
      group_by(year = year(date)) %>%
      summarise(p_complete = round(sum(!is.na(pm25_mean)) / n(), 3)*100,
                perc_98_of_daily_means = quantile(pm25_mean, 0.98, na.rm = T),
                mean_of_daily_means = mean(pm25_mean, na.rm = T)) %>%
      # +3 year averages, +standard for that year, +whether standard is met
      mutate(
        # +3 year averages,
        `3yr_mean_of_perc_98` = get_lag_n_mean(perc_98_of_daily_means, n = 3),
        `3yr_mean_of_means` = get_lag_n_mean(mean_of_daily_means, n = 3),
        # +standards for that year
        daily_standard = sapply(year, get_threshold,
                                thresholds = thresholds$pm25$daily),
        annual_standard = sapply(year, get_threshold,
                                 thresholds = thresholds$pm25$annual),
        # +whether standards are met
        meets_standard_daily = `3yr_mean_of_perc_98` <= daily_standard,
        meets_standard_annual = `3yr_mean_of_means` <= annual_standard
      ) %>%
      dplyr::relocate(c(daily_standard, meets_standard_daily),
                      .after = "3yr_mean_of_perc_98")%>%
      dplyr::relocate(mean_of_daily_means,
                      .after = "meets_standard_daily") %>%
      dplyr::relocate(p_complete, .after = "year")
  }
  # Test o3 if provided
  if(!is.null(o3_hourly)){
    met$o3 = obs %>%
      # hourly mean -> 8 hourly mean
      group_by(date = floor_date(date, "8 hours")) %>%
      summarise(`8hr_mean_o3` = mean(o3, na.rm = T)) %>%
      # 8 hourly mean -> daily max
      group_by(date = floor_date(date, "days")) %>%
      summarise(daily_max_8hr_mean_o3 = max_no_na(`8hr_mean_o3`)) %>%
      # daily max -> annual 4th highest
      group_by(year = year(date)) %>%
      arrange(desc(daily_max_8hr_mean_o3)) %>%
      summarise(
        p_complete = round(sum(!is.na(daily_max_8hr_mean_o3)) / n(), 3)*100,
        fourth_highest_daily_max_8hr_mean_o3 = daily_max_8hr_mean_o3[4]) %>%
      # +3 year averages, +standard for that year, +whether standard is met
      mutate(
        # +3 year averages
        `3yr_mean` = get_lag_n_mean(fourth_highest_daily_max_8hr_mean_o3, n = 3),
        # +standard for that year
        standard = sapply(year, get_threshold,
                                 thresholds = thresholds$o3$`8hr`),
        # +whether standard is met
        meets_standard = `3yr_mean` <= standard
      ) %>%
      dplyr::relocate(p_complete, .after = "year")
  }
  # Test no2 if provided
  if(!is.null(no2_hourly)){
    met$no2 = obs %>%
      # + annual mean
      group_by(year = year(date)) %>%
      mutate(annual_mean_of_hourly = mean(no2, na.rm = T)) %>%
      # hourly mean -> daily maxima
      group_by(date = floor_date(date, "1 days"), annual_mean_of_hourly) %>%
      summarise(daily_max_hourly_no2 = max(no2, na.rm = T), .groups = "drop") %>%
      # daily maxima -> annual 98th percentile
      group_by(year = year(date), annual_mean_of_hourly) %>%
      summarise(
        p_complete = round(sum(!is.na(daily_max_hourly_no2)) / n(), 3)*100,
        perc_98_of_daily_maxima = quantile(
          daily_max_hourly_no2, 0.98, na.rm = T), .groups = "drop") %>%
      # +3 year averages, +standard for that year, +whether standard is met
      mutate(
        # +3 year averages
        `3yr_mean_of_perc_98` = get_lag_n_mean(perc_98_of_daily_maxima, n = 3),
        # +standards for that year
        standard_hourly = sapply(year, get_threshold,
                                 thresholds = thresholds$no2$hourly),
        standard_annual = sapply(year, get_threshold,
                                 thresholds = thresholds$no2$annual),
        # +whether standard is met
        meets_standard_hourly = annual_mean_of_hourly <= standard_hourly,
        meets_standard_annual = `3yr_mean_of_perc_98` <= standard_annual,
      ) %>%
      dplyr::relocate(c(standard_hourly, meets_standard_hourly),
                      .after = "annual_mean_of_hourly") %>%
      dplyr::relocate(p_complete, .after = "year")
  }
  # Test so2 if provided
  if(!is.null(so2_hourly)){
    met$so2 = obs %>%
      # + annual mean
      group_by(year = year(date)) %>%
      mutate(annual_mean_of_hourly = mean(so2, na.rm = T)) %>%
      # hourly mean -> daily maxima
      group_by(date = floor_date(date, "1 days"), annual_mean_of_hourly) %>%
      summarise(daily_max_hourly_so2 = max(no2, na.rm = T), .groups = "drop") %>%
      # daily maxima -> annual 98th percentile
      group_by(year = year(date), annual_mean_of_hourly) %>%
      summarise(
        p_complete = round(sum(!is.na(daily_max_hourly_so2)) / n(), 3)*100,
        perc_99_of_daily_maxima = quantile(
          daily_max_hourly_so2, 0.99, na.rm = T), .groups = "drop") %>%
      # +3 year averages, +standard for that year, +whether standard is met
      mutate(
        # +3 year averages
        `3yr_mean_of_perc_99` = get_lag_n_mean(perc_99_of_daily_maxima, n = 3),
        # +standards for that year
        standard_hourly = sapply(year, get_threshold,
                                 thresholds = thresholds$so2$hourly),
        standard_annual = sapply(year, get_threshold,
                                 thresholds = thresholds$so2$annual),
        # +whether standard is met
        meets_standard_hourly = annual_mean_of_hourly <= standard_hourly,
        meets_standard_annual = `3yr_mean_of_perc_99` <= standard_annual,
      ) %>%
      dplyr::relocate(c(standard_hourly, meets_standard_hourly),
                      .after = "annual_mean_of_hourly") %>%
      dplyr::relocate(p_complete, .after = "year")
  }
  return(met)
}

