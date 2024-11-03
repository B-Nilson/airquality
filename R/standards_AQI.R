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
AQI <- function(
    dates = Sys.time(),
    o3_8hr_ppm = NA, o3_1hr_ppm = NA,
    pm25_24hr_ugm3 = NA, pm25_1hr_ugm3 = NA,
    pm10_24hr_ugm3 = NA, pm10_1hr_ugm3 = NA,
    co_8hr_ppm = NA, co_1hr_ppm = NA,
    so2_1hr_ppb = NA, no2_1hr_ppb = NA) {
  # TODO: Reference https://forum.airnowtech.org/t/the-aqi-equation-2024-valid-beginning-may-6th-2024/453

  # Determine which pollutants provided as input
  AQI_pols <- methods::formalArgs(AQI)[-1]
  all_missing <- lapply_and_name(
    AQI_pols,
    \(pol) all(is.na(get(pol)))
  )
  all_missing$so2_24hr_ppb <- TRUE
  # Ensure at least one pollutants data is provided
  if (all(unlist(all_missing))) {
    stop(paste(
      "At least one pollutant's concentrations must",
      "be provided and have at least 1 non-NA value."
    ))
  }

  # Combine inputs and fill date gaps with NAs
  dat <- dplyr::tibble(
    date = dates,
    o3_8hr_ppm, o3_1hr_ppm,
    pm25_24hr_ugm3, pm25_1hr_ugm3,
    pm10_24hr_ugm3, pm10_1hr_ugm3,
    co_8hr_ppm, co_1hr_ppm,
    so2_1hr_ppb, so2_24hr_ppb = NA,
    no2_1hr_ppb
  ) |>
    tidyr::complete(date = seq(min(dates), max(dates), "1 hours")) |>
    dplyr::arrange(.data$date)

  # Make additional running averages if needed
  needs_o3_8hr <- all_missing$o3_8hr_ppm &
    !all_missing$o3_1hr_ppm
  if (needs_o3_8hr) {
    dat$o3_8hr_ppm <- dat$o3_1hr_ppm |>
      roll_mean(8, min_n = 5)
    all_missing$o3_8hr_ppm <- FALSE
  }
  needs_pm25_24hr <- all_missing$pm25_24hr_ugm3 &
    !all_missing$pm25_1hr_ugm3
  if (needs_pm25_24hr) {
    dat$pm25_24hr_ugm3 <- dat$pm25_1hr_ugm3 |>
      roll_mean(24, min_n = 15)
    all_missing$pm25_24hr_ugm3 <- FALSE
  }
  needs_pm10_24hr <- all_missing$pm10_24hr_ugm3 &
    !all_missing$pm10_1hr_ugm3
  if (needs_pm10_24hr) {
    dat$pm10_24hr_ugm3 <- dat$pm10_1hr_ugm3 |>
      roll_mean(24, min_n = 15)
    all_missing$pm10_24hr_ugm3 <- FALSE
  }
  needs_co_8hr <- all_missing$co_8hr_ppm &
    !all_missing$co_1hr_ppm
  if (needs_co_8hr) {
    dat$co_8hr_ppm <- dat$co_1hr_ppm |>
      roll_mean(8, min_n = 5)
    all_missing$co_8hr_ppm <- FALSE
  }
  needs_so2_24hr <- !all_missing$so2_1hr_ppb
  if (needs_so2_24hr) {
    dat$so2_24hr_ppb <- dat$so2_1hr_ppb |>
      roll_mean(24, min_n = 15)
    all_missing$so2_24hr_ppb <- FALSE
  }

  # Get Daily mean values for all pollutants/averaging times
  dat <- dat |>
    dplyr::group_by(date = .data$date |>
      lubridate::floor_date("days")) |>
    dplyr::summarise(
      .groups = "drop",
      dplyr::across(
        dplyr::everything(),
        mean_no_na
      )
    ) |>
    # Truncate daily means
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("o3"),
        \(x) trunc_n(x, 3)
      ),
      dplyr::across(
        dplyr::starts_with("pm25|co"),
        \(x) trunc_n(x, 1)
      ),
      dplyr::across(
        dplyr::starts_with("so2|no2|pm10"),
        \(x) trunc_n(x, 0)
      )
    )

  # Calculate AQI for each pollutant provided
  pols <- names(dat)[-1]
  for (pol in pols) {
    if (!all_missing[[pol]]) {
      dat <- AQI_from_con(dat, pol)
    } else {
      dat[[paste0("AQI_", pol)]] <- NA
    }
  }

  AQI_cols <- paste0("AQI_", pols)
  names(AQI_cols) <- (AQI_cols |>
    stringr::str_split("_", simplify = TRUE))[, 2]

  # Set hourly AQI to the highest of the calculated values
  dat |>
    dplyr::rowwise() |>
    dplyr::mutate(
      AQI = max_no_na(dplyr::across(dplyr::all_of(AQI_cols))) |>
        swap_inf(NA),
      risk_category = AQI_risk_category(.data$AQI)
    ) |>
    get_AQI_principal_pol(AQI_cols) |>
    dplyr::ungroup() |>
    dplyr::select("date", "AQI", "risk_category", "principal_pol")
}

## AQI Helpers ------------------------------------------------------------
aqi_levels <- list(
  "Good" = 0:50,
  "Moderate" = 51:100,
  "Unhealthy for Sensitive Groups" = 101:150,
  "Unhealthy" = 151:200,
  "Very Unhealthy" = 201:300,
  "Hazardous" = 301:500,
  "Beyond the AQI" = 500:5000
)

# Returns Risk category when AQI value provided
AQI_risk_category <- function(AQI) {
  labels <- seq_along(aqi_levels) |> sapply(\(i){
    names(aqi_levels)[i] |>
      stringr::str_remove("2$") |>
      rep(length(aqi_levels[[i]]))
  })
  factor(AQI,
    levels = unlist(aqi_levels),
    labels = unlist(labels)
  )
}

# Get risk category for breakpoint determination for AQI formulation
AQI_bp_cat <- function(obs, bps) {
  suppressMessages(
    bps$risk_category |> lapply(\(cat) {
      bp <- bps[bps$risk_category == cat, ]
      ifelse(obs >= bp$bp_low & obs <= bp$bp_high, cat, NA)
    }) |>
      as.data.frame() |>
      apply(1, \(row){
        ifelse(all(is.na(row)), NA, row[!is.na(row)])
      }) |>
      unlist()
  )
}

# When provided concentrations and corresponding breakpoints, return AQI
AQI_formulation <- function(obs, bp_low, bp_high, aqi_low, aqi_high) {
  ceiling(
    (aqi_high - aqi_low) / (bp_high - bp_low) * (obs - bp_low) + aqi_low
  )
}

# Workhorse function to determine risk category, append breakponints, then calc AQI
AQI_from_con <- function(dat, pol) {
  cols <- c("cat_", "AQI_") |> paste0(pol)
  dat[cols] <- NA # TODO: is this necessary?
  dat <- dat |>
    # Determine the risk category based on the concentrations and break points
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols[1]),
      \(x) AQI_bp_cat(
        obs = swap_na(dat[[pol]], 0),
        bps = AQI_breakpoints[[pol]]
      )
    )) |>
    # Append the corresponding break points and AQI breaks for each hour
    dplyr::left_join(
      AQI_breakpoints[[pol]] |>
        dplyr::rename_with(.cols = 2:5, \(x) paste0(x, "_", pol)),
      by = dplyr::join_by(!!cols[1] == "risk_category")
    )
  # Calculate AQI for each hour based on those
  dat |> dplyr::mutate(dplyr::across(
    dplyr::all_of(cols[2]),
    \(x) dat[[pol]] |> AQI_formulation(
      bp_low = dat[[paste0("bp_low_", pol)]],
      bp_high = dat[[paste0("bp_high_", pol)]],
      aqi_low = dat[[paste0("aqi_low_", pol)]],
      aqi_high = dat[[paste0("aqi_high_", pol)]]
    )
  ))
}

# Define breakpoints for AQI formulation
AQI_breakpoints <- list(
  ## 8 hour mean ozone
  # 8-hour O3 values do not define higher AQI values (≥ 301).
  # AQI values of 301 or higher are calculated with 1-hour O3 concentrations.
  # The highest of the 1hr/8hr AQI for ozone is used
  # (1 hour is only really used for some areas)
  o3_8hr_ppm = data.frame(
    risk_category = names(aqi_levels)[1:5],
    bp_low = c(0, 0.055, 0.071, 0.086, 0.106),
    bp_high = c(0.054, 0.07, 0.085, 0.105, 0.2),
    aqi_low = c(0, 51, 101, 151, 201),
    aqi_high = c(50, 100, 150, 200, 300)
  ),
  ## 1 Hour Mean Ozone
  # 1-hour O3 values do not define Good-Moderate AQI values (< 101).
  o3_1hr_ppm = data.frame(
    risk_category = names(aqi_levels)[3:7],
    bp_low = c(0.125, 0.165, 0.205, 0.405, 0.605),
    bp_high = c(0.164, 0.204, 0.404, 0.504, Inf),
    aqi_low = c(101, 151, 201, 301, 301),
    aqi_high = c(150, 200, 300, 500, 500)
  ),
  ## 24 Hour Mean Fine Particulate Matter
  # If a different SHL for PM2.5 is promulgated, Unhealthy and above will change accordingly.
  pm25_24hr_ugm3 = data.frame(
    risk_category = names(aqi_levels)[1:7],
    bp_low = c(0, 9.1, 35.5, 55.5, 125.5, 225.5, 325.5),
    bp_high = c(9, 35.4, 55.4, 125.4, 225.4, 325.4, Inf),
    aqi_low = c(0, 51, 101, 151, 201, 301, 301),
    aqi_high = c(50, 100, 150, 200, 300, 500, 500)
  ),
  ## 24 Hour Mean Fine-Coarse Particulate Matter
  pm10_24hr_ugm3 = data.frame(
    risk_category = names(aqi_levels)[1:7],
    bp_low = c(0, 55, 155, 255, 355, 425, 605),
    bp_high = c(54, 154, 254, 354, 424, 604, Inf),
    aqi_low = c(0, 51, 101, 151, 201, 301, 301),
    aqi_high = c(50, 100, 150, 200, 300, 500, 500)
  ),
  ## 8 Hour Mean Carbon Monoxide
  co_8hr_ppm = data.frame(
    risk_category = names(aqi_levels)[1:7],
    bp_low = c(0, 4.5, 9.5, 12.5, 15.5, 30.5, 50.5),
    bp_high = c(4.4, 9.4, 12.4, 15.4, 30.4, 50.4, Inf),
    aqi_low = c(0, 51, 101, 151, 201, 301, 301),
    aqi_high = c(50, 100, 150, 200, 300, 500, 500)
  ),
  ## 1 Hour Mean Sulfur Dioxide
  # 1-hour SO2 values do not define higher AQI values (≥ 200).
  so2_1hr_ppb = data.frame(
    risk_category = names(aqi_levels)[c(1:4)],
    bp_low = c(0, 36, 76, 186),
    bp_high = c(35, 75, 185, 304),
    aqi_low = c(0, 51, 101, 151),
    aqi_high = c(50, 100, 150, 200)
  ),
  ## 24 Hour Mean Sulfur Dioxide
  # AQI values of 200 or greater are calculated with 24-hour SO2 concentrations.
  so2_24hr_ppb = data.frame(
    risk_category = names(aqi_levels)[5:7],
    bp_low = c(305, 605, 1005),
    bp_high = c(604, 1004, Inf),
    aqi_low = c(201, 301, 301),
    aqi_high = c(300, 500, 500)
  ),
  ## 1 Hour Mean Nitrogen Dioxide
  no2_1hr_ppb = data.frame(
    risk_category = names(aqi_levels)[1:7],
    bp_low = c(0, 54, 101, 361, 650, 1250, 2050),
    bp_high = c(53, 100, 360, 649, 1249, 2049, Inf),
    aqi_low = c(0, 51, 101, 151, 201, 301, 301),
    aqi_high = c(50, 100, 150, 200, 300, 500, 500)
  )
)

get_AQI_principal_pol <- function(dat, AQI_cols) {
  dat |>
    dplyr::rowwise() |>
    dplyr::mutate(
      principal_pol_index = which.max(
        dplyr::across(
          dplyr::all_of(unname(AQI_cols)),
          \(x) swap_na(x, 0)
        )
      ),
      principal_pol = names(AQI_cols)[.data$principal_pol_index],
      principal_pol = (!is.na(.data$AQI)) |>
        ifelse(.data$principal_pol, NA) |>
        factor(unique(names(AQI_cols)))
    )
}
