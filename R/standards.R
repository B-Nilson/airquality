
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

# Calculates Canadian AQHI (overriden by AQHI+ if higher)
AQHI = function(pm25_hourly, no2_hourly = NULL, o3_hourly = NULL, quiet = FALSE){
  # Calculate AQHI+ (pm25 only)
  aqhi_plus = AQHI_plus(pm25_hourly)

  # Need all 3 pollutants to calculate AQHI
  if(!is.null(no2_hourly) & !is.null(o3_hourly)){
    # Rolling 3 hour average PM2.5 (at least 2 hours per average)
    pm25_rolling_3hr = zoo::rollapply(pm25_hourly, width = 3,
                                     align = "right", fill = NA,
                                     FUN = mean_if_enough, min_n = 2)
    # Rolling 3 hour average NO2 (at least 2 hours per average)
    no2_rolling_3hr = zoo::rollapply(no2_hourly, width = 3,
                                     align = "right", fill = NA,
                                     FUN = mean_if_enough, min_n = 2)
    # Rolling 3 hour average O3(at least 2 hours per average)
    o3_rolling_3hr = zoo::rollapply(o3_hourly, width = 3,
                                     align = "right", fill = NA,
                                     FUN = mean_if_enough, min_n = 2)
    # Calculate AQHI
    aqhi = cut(round(10/10.4 * 100 * (
      (exp(0.000537 * o3_rolling_3hr) - 1) +
        (exp(0.000871 * no2_rolling_3hr) - 1) +
          (exp(0.000487 * pm25_rolling_3hr) - 1)
    )), aqhi_breakpoints/10, unlist(aqhi_levels))

    # Get risk levels
    risk = factor(
      aqhi, unlist(aqhi_levels),
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

    # Combine
    out = data.frame(
      pm25_hourly = pm25_hourly,
      no2_hourly = no2_hourly,
      o3_hourly = o3_hourly,
      AQHI = aqhi,
      risk = risk,
      health_messages
    )

    # TODO: clean this up
    # Use AQHI+ if it exceeds AQHI
    out$AQHI_plus_exceeds_AQHI = as.numeric(aqhi_plus$AQHI_plus) > as.numeric(out$AQHI)
    out$AQHI_plus_exceeds_AQHI[is.na(out$AQHI_plus_exceeds_AQHI)] = T
    out$AQHI = ifelse(out$AQHI_plus_exceeds_AQHI, aqhi_plus$AQHI_plus, out$AQHI)
    out$risk = ifelse(out$AQHI_plus_exceeds_AQHI, aqhi_plus$risk, out$risk)
    out$high_risk_pop = ifelse(out$AQHI_plus_exceeds_AQHI, aqhi_plus$high_risk_pop, out$high_risk_pop)
    out$general_pop = ifelse(out$AQHI_plus_exceeds_AQHI, aqhi_plus$general_pop, out$general_pop)

  }else{
    if(!quiet) warning("Returning AQHI+ (PM2.5 only) as NO2 and O3 were not provided.")
    out = aqhi_plus
  }
  return(out)
}

# Calculates Canadian AQHI+ (PM2.5 only)
AQHI_plus = function(
    pm25_hourly,
    min_allowed_pm25 = 0)
{
  ## FORMATTING -------------
  # Remove values < min_allowed_pm25 (normally 0)
  pm25_hourly[pm25_hourly < min_allowed_pm25] = NA

  # MAIN ----------------
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
  health_messages = dplyr::bind_rows(aqhi_messaging[risk])

  # RETURN ---------------
  data.frame(
    pm25_hourly = pm25_hourly,
    AQHI_plus = aqhi_p,
    risk = risk,
    health_messages
  )

}
