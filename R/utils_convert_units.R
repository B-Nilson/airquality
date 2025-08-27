# TODO: move to handyr
#' Convert between common units
#'
#' @param x Vector of numeric values to be converted.
#' @param in_unit A single character value indicating the units of `x`. See `units::available_units()` for options.
#' @param out_unit A single character value indicating the units to convert `x` to. See `units::available_units()` for options.
#' @param keep_units (Optional) A single logical (TRUE or FALSE) value indicating if the units of `x` should be kept.
#'
#' @description
#' `convert_units` provides a simple way to convert between units, leveraging the `units` package.
#'
#' @family Utilities
#'
#' @return a vector of `x` converted from `in_unit` to `out_unit`
#' @export
#'
#' @examples
#' convert_units(c(-20:20), in_unit = "degC", out_unit = "degF")
#' convert_units(c(0:10), in_unit = "ppm", out_unit = "ppb", keep_units = TRUE)
convert_units <- function(x, in_unit, out_unit, keep_units = FALSE) {
  converted <- x |>
    units::set_units(in_unit, mode = "standard") |>
    units::set_units(out_unit, mode = "standard")

  if (!keep_units) {
    converted <- as.numeric(converted)
  }
  return(converted)
}

saturation_vapour_pressure <- function(temperature_c) {
  if (!dplyr::between(temperature_c, -80, 50)) {
    warning(
      "Saturation vapour pressure estimation method only optimized within [-80, 50] celcius"
    )
  }
  # Using the Arden Buck equation (Buck, 1996)
  ifelse(
    temperature_c > 0,
    # over water
    6.1121 *
      exp(
        (18.678 - temperature_c / 234.5) *
          (temperature_c / (257.14 + temperature_c))
      ),
    # over ice
    6.1115 *
      exp(
        (23.036 - temperature_c / 333.7) *
          (temperature_c / (279.82 + temperature_c))
      )
  ) # units hPa (millibars)
}
