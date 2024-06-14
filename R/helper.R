
# Calculates the mean if enough values are provided
mean_if_enough = function(x, min_n = 0, ...){
  ifelse(sum(!is.na(x)) >= min_n, mean(x, na.rm = T, ...), NA)
}

# Calculate rolling 3 hour mean if at least 2 hours available
roll_mean_3hr_min_2 = function(x) {
  zoo::rollapply(
    x, width = 3, align = "right", fill = NA,
    FUN = mean_if_enough, min_n = 2) %>%
    round(1)
}
# Calculate rolling 8 hour mean if at least 5 hours available
roll_mean_8hr_min_5 = function(x) {
  zoo::rollapply(
    x, width = 8, align = "right", fill = NA,
    FUN = mean_if_enough, min_n = 5) %>%
    round(1)
}

# helper functions to replace NA/inf with val
swap_na = function(x, val = -99) ifelse(is.na(x), val, x)
swap_inf = function(x, val = NA) ifelse(is.infinite(x), val, x)

# helper functions to remove NA by default
mean_no_na = function(x, ...) mean(x, na.rm = T, ...)
min_no_na = function(x, ...) swap_inf(suppressWarnings(min(x, na.rm = T, ...)), NA)
max_no_na = function(x, ...) swap_inf(suppressWarnings(max(x, na.rm = T, ...)), NA)

# helper function to make 3 (or n) year means (NA NOT IGNORED)
get_lag_n_mean = function(x, n = 3){
  out = x
  if(n <= 1) stop("`n` must be greater than one")
  for(i in 1:(n-1)){
    out = out + dplyr::lag(x, i)
  }
  return(out/n)
}
