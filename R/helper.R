
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

# helper functions to replace NA/inf with val
swap_na = function(x, val = -99) ifelse(is.na(x), val, x)
swap_inf = function(x, val = NA) ifelse(is.infinite(x), val, x)

# helper functions to remove NA by default
mean_no_na = function(x, ...) mean(x, na.rm = T, ...)
min_no_na = function(x, ...) swap_inf(suppressWarnings(min(x, na.rm = T, ...)), NA)
max_no_na = function(x, ...) swap_inf(suppressWarnings(max(x, na.rm = T, ...)), NA)
