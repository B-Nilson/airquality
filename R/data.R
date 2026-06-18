#' Example airquality station observations
#'
#' @format ## `example_obs`
#' A tibble with 8,740 rows and 15 columns:
#' \describe{
#'   \item{date_utc}{Observation time in UTC (time-ending)}
#'   \item{date_local}{Observation time in local time (time-ending)}
#'   \item{pm25_1hr}{PM2.5 concentration in micrograms per cubic meter}
#'   \item{pm10_1hr}{PM10 concentration in micrograms per cubic meter}
#'   \item{o3_1hr}{O3 concentration in parts per billion}
#'   \item{no_1hr}{NO concentration in parts per billion}
#'   \item{no2_1hr}{NO2 concentration in parts per billion}
#'   \item{so2_1hr}{SO2 concentration in parts per billion}
#'   \item{trs_1hr}{Total Reduced Sulphur concentration in parts per billion}
#'   \item{rh_1hr}{Relative Humidity in percent}
#'   \item{temp_1hr}{Temperature in degrees Celsius}
#'   \item{wd_1hr}{Wind direction in degrees}
#'   \item{wd_unitvector_1hr}{Wind direction (unit vector average) in degrees}
#'   \item{ws_1hr}{Wind speed in meters per second}
#'   \item{ws_vector_1hr}{Wind speed (vector average) in meters per second}
#' }
#' @source see get_bcgov_data()
"example_obs"