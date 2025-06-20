#' Calculate the Canadian AQHI from hourly PM2.5, NO2, and O3 observations
#'
#' @param dates Vector of hourly datetimes corresponding to observations. Date gaps will be filled automatically.
#' @param pm25_1hr_ugm3 Numeric vector of hourly mean fine particulate matter (PM2.5) concentrations (ug/m^3).
#' @param no2_1hr_ppb (Optional). Numeric vector of hourly mean nitrogen dioxide (NO2) concentrations (ppb). If not provided AQHI+ will be calculated from PM2.5 only.
#' @param o3_1hr_ppb (Optional). Numeric vector of hourly mean ozone (O3) concentrations (ppb). If not provided AQHI+ will be calculated from PM2.5 only.
#' @param verbose (Optional). A single logical (TRUE/FALSE) value indicating if non-critical warnings/messages should be displayed. Default is TRUE
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
#' obs <- data.frame(
#'   date = seq(
#'     lubridate::ymd_h("2024-01-01 00"),
#'     lubridate::ymd_h("2024-01-01 23"), "1 hours"
#'   ),
#'   pm25 = sample(1:150, 24), o3 = sample(1:150, 24), no2 = sample(1:150, 24)
#' )
#' AQHI(
#'   dates = obs$date, pm25_1hr_ugm3 = obs$pm25,
#'   o3_1hr_ppb = obs$o3, no2_1hr_ppb = obs$no2
#' )
#'
#' AQHI(dates = obs$date, pm25_1hr_ugm3 = obs$pm25) # Returns AQHI+
AQHI <- function(dates, pm25_1hr_ugm3, no2_1hr_ppb = NA, o3_1hr_ppb = NA, verbose = TRUE) {
  obs <- dplyr::bind_cols(
    date = dates,
    pm25 = pm25_1hr_ugm3,
    o3   = o3_1hr_ppb,
    no2  = no2_1hr_ppb
  ) |>
    tidyr::complete(date = seq(min(date), max(date), "1 hours")) |>
    dplyr::arrange(date)

  # Calculate AQHI+ (PM2.5 Only) - AQHI+ overrides AQHI if higher
  aqhi_plus <- AQHI_plus(obs$pm25) |>
    dplyr::mutate(AQHI = .data$AQHI_plus, AQHI_plus_exceeds_AQHI = NA) |>
    dplyr::relocate("AQHI", .before = "AQHI_plus")
  # Calculate AQHI (if all 3 pollutants provided)
  has_all_3_pol <- any(
    !is.na(pm25_1hr_ugm3) & !is.na(no2_1hr_ppb) & !is.na(o3_1hr_ppb)
  )
  if (has_all_3_pol) {
    rolling_cols <- c("pm25", "no2", "o3")
    names(rolling_cols) <- paste0(rolling_cols, "_rolling_3hr")
    obs <- obs |> dplyr::mutate(
      dplyr::across(
        dplyr::all_of(rolling_cols),
        \(x) x |> 
          handyr::rolling(mean, .width = 3, .min_non_na = 2) |>
          round(digits = 1)
      ),
      AQHI = AQHI_formula(
        pm25_rolling_3hr = .data$pm25_rolling_3hr,
        no2_rolling_3hr  = .data$no2_rolling_3hr,
        o3_rolling_3hr   = .data$o3_rolling_3hr
      ),
      AQHI_plus = aqhi_plus$AQHI_plus,
      risk = AQHI_risk_category(.data$AQHI)
    )
    obs |>
      dplyr::bind_cols(AQHI_health_messaging(obs$risk)) |>
      AQHI_replace_w_AQHI_plus(aqhi_plus)
  } else {
    if (verbose) warning("Returning AQHI+ (PM2.5 only) as no non-missing NO2 / O3 provided.")
    return(aqhi_plus)
  }
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
#' obs <- data.frame(
#'   date = seq(
#'     lubridate::ymd_h("2024-01-01 00"),
#'     lubridate::ymd_h("2024-01-01 23"), "1 hours"
#'   ),
#'   pm25 = sample(1:150, 24)
#' )
#' # Calculate the AQHI+
#' AQHI_plus(obs$pm25)
#'
#' # Hourly pm2.5 concentrations (with negative values)
#' obs <- data.frame(
#'   date = seq(
#'     lubridate::ymd_h("2024-01-01 00"),
#'     lubridate::ymd_h("2024-01-01 23"), "1 hours"
#'   ),
#'   pm25 = c(-2, -0.1, sample(1:150, 22))
#' )
#' # Calculate the AQHI+ for each hour, except for hours where pm2.5 is < -0.5
#' AQHI_plus(obs$pm25, min_allowed_pm25 = -0.5)
#' @importFrom rlang .data
AQHI_plus <- function(pm25_1hr_ugm3, min_allowed_pm25 = 0) {
  # Remove values below the provided minimum
  pm25_1hr_ugm3[pm25_1hr_ugm3 < min_allowed_pm25] <- NA
  # Calculate AQHI+, and get the associated risk and health messaging
  aqhi_breakpoints <- c(-Inf, 1:10 * 10, Inf) |>
    stats::setNames(c(NA, 1:10, "+"))
  aqhi_p <- pm25_1hr_ugm3 |> cut(
    breaks = aqhi_breakpoints,
    labels = names(aqhi_breakpoints)[-1]
  )
  risk <- AQHI_risk_category(aqhi_p)
  health_messages <- AQHI_health_messaging(risk)
  # Combine and return
  dplyr::tibble(
    pm25_1hr_ugm3 = pm25_1hr_ugm3,
    AQHI_plus = aqhi_p,
    risk = risk,
    health_messages
  )
}

## AQHI Helpers -----------------------------------------------------------
AQHI_formula <- function(pm25_rolling_3hr, no2_rolling_3hr, o3_rolling_3hr) {
  aqhi_breakpoints <- c(-Inf, 1:10, Inf) |>
    stats::setNames(c(NA, 1:10, "+"))
  aqhi <- round(10 / 10.4 * 100 * (
    (exp(0.000537 * o3_rolling_3hr) - 1) +
      (exp(0.000871 * no2_rolling_3hr) - 1) +
      (exp(0.000487 * pm25_rolling_3hr) - 1)
  ))
  cut(
    aqhi,
    breaks = aqhi_breakpoints,
    labels = names(aqhi_breakpoints[-1])
  )
}

AQHI_risk_category <- function(AQHI) {
  aqhi_levels <- list(
    Low = 1:3, Moderate = 4:6,
    High = 7:10, "Very High" = "+"
  )
  aqhi_labels <- aqhi_levels |>
    seq_along() |>
    sapply(\(i) names(aqhi_levels)[i] |>
      rep(length(aqhi_levels[[i]])))
  AQHI |>
    factor(unlist(aqhi_levels), unlist(aqhi_labels))
}

AQHI_health_messaging <- function(risk_categories) {
  aqhi_messaging <- list(
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
  # TODO: is this necessary?
  aqhi_messaging[risk_categories] |> 
    handyr::for_each(
      .as_list = TRUE, .bind = TRUE,
      \(x) if (is.null(x)) {
        data.frame(high_risk_pop_message = NA, general_pop_message = NA)
      } else {
        x
      }
  )
}

# TODO: make sure AQHI is a column in obs
AQHI_replace_w_AQHI_plus <- function(obs, aqhi_plus) {
  dplyr::mutate(obs,
    AQHI_plus_exceeds_AQHI = (as.numeric(aqhi_plus$AQHI_plus) > as.numeric(.data$AQHI)) |>
      handyr::swap(NA, with = TRUE),
    AQHI = ifelse(.data$AQHI_plus_exceeds_AQHI,
      aqhi_plus$AQHI_plus, .data$AQHI
    ),
    risk = ifelse(.data$AQHI_plus_exceeds_AQHI,
      aqhi_plus$risk, .data$risk
    ),
    high_risk_pop_message = ifelse(.data$AQHI_plus_exceeds_AQHI,
      aqhi_plus$high_risk_pop_message, .data$high_risk_pop_message
    ),
    general_pop_message = ifelse(.data$AQHI_plus_exceeds_AQHI,
      aqhi_plus$general_pop_message, .data$general_pop_message
    )
  )
}
