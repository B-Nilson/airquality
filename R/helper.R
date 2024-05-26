
# Calculates the mean if enough values are provided
mean_if_enough = function(x, min_n = 0, ...){
  ifelse(sum(!is.na(x)) >= min_n, mean(x, na.rm = T, ...), NA)
}


# helper functions to replace NA/inf with val
swap_na = function(x, val = -99) ifelse(is.na(x), val, x)
swap_inf = function(x, val = NA) ifelse(is.infinite(x), val, x)
