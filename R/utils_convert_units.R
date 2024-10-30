#' Convert between common units
#'
#' @param x Vector of values to be converted.
#' @param in_unit A single character value indicating the units of `x`.
#' @param out_unit A single character value indicating the units to convert `x` to.
#' @param y (Optional) [UNDER DEVELOPMENT] Vector of extra values required for certain conversions. (i.e. temperature for converting RH to DEWPOINT)
#' 
#' @description
#' `convert_units` provides a simple way to convert between common units in air quality / meteorology. 
#'
#' @references \url{https://ccme.ca/en/air-quality-report}
#' @family Utilities
#'
#' @return a vector of `x` converted from `in_unit` to `out_unit` 
#' @export
#'
#' @examples
#' convert_units(c(-20:20), in_unit = "C", out_unit = "F")
#' convert_units(c(0:10), in_unit = "PPM", out_unit = "PPB")
convert_units = function(x, in_unit, out_unit, y = NULL){
  # Handle inputs
  in_unit = toupper(in_unit)
  out_unit = toupper(out_unit)
  if (in_unit == out_unit) 
    return(x)

  # Determine conversion type
  all_units = all_conversions |> lapply(\(conversions)
    stringr::str_split(names(conversions), "_to_") |> unlist())
  conversion_type = sapply(all_units, \(x) in_unit %in% x)
  conversion_type = names(conversion_type[conversion_type])[1]

  # Convert into shared base unit first if needed
  base_unit = all_units[[conversion_type]][1]
  is_base_unit = in_unit == base_unit | out_unit == base_unit
  if (!is_base_unit) {
    x = convert_units(x, in_unit, base_unit)
    in_unit = base_unit
  }
  
  # Apply conversion to desired unit
  conversion = paste0(in_unit, "_to_", out_unit)
  conversion_fun = all_conversions[[conversion_type]][[conversion]]
  if(is.null(y)) conversion_fun(x) else conversion_fun(x, y)
}

# TODO: add to this
all_conversions = list(
  concentrations = list(
    PPM_to_PPB = function(PPM) PPM * 1000,
    PPB_to_PPM = function(PPB) PPB / 1000,
    PPM_to_UGM3 = function(PPM) PPM,
    UGM3_to_PPM = function(UGM3) UGM3
  ),
  temperature = list(
    C_to_F = function(C) (C * 9 / 5) + 32,
    C_to_K = function(C) C + 273.15,
    F_to_C = function(F) (F - 32) * 5 / 9,
    K_to_C = function(K) K - 273.15
  ),
  humidity = list(
    RH_to_DEWPOINT = function(RH, T){
      b = ifelse(T >= 0, 17.368, 17.966) # Over water, or over ice
      c = ifelse(T >= 0, 238.88, 247.15) # Over water, or over ice
      return(c * log(RH/100 * saturation_vapour_pressure(T) / 6.1121) /
              (b - log(RH/100 * saturation_vapour_pressure(T) / 6.1121)))
    },
    DEWPOINT_to_RH = function(Td, T)
      saturation_vapour_pressure(Td) / saturation_vapour_pressure(T) * 100
  ),
  trigonometry = list(
    DEGREES_to_RADIANS = function(degrees) {
      (degrees * pi / 180) %% (2 * pi)
    },
    RADIANS_to_DEGREES = function(radians) {
      (radians * 180 / pi) %% 360
    }
  )
)

saturation_vapour_pressure = function(temperature_c){
  if(!dplyr::between(temperature_c, -80, 50))
    warning("Saturation vapour pressure estimation method only optimized within [-80, 50] celcius")
  # Using the Arden Buck equation (Buck, 1996)
  e = ifelse(temperature_c > 0,
             6.1121 * exp( # over water
               (18.678 - temperature_c/234.5) *
               (temperature_c/(257.14 + temperature_c))),
             6.1115 * exp( # over ice
               (23.036 - temperature_c/333.7) *
               (temperature_c/(279.82 + temperature_c))))
  return(e) # units hPa (millibars)
}