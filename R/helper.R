
# Specify return value if code fails
on_error = function(..., return = NULL){
  tryCatch(..., error = \(...) return(return))
}

# Calculates the mean if enough values are provided
mean_if_enough = function(x, min_n = 0, ...){
  ifelse(sum(!is.na(x)) >= min_n, mean(x, na.rm = T, ...), NA)
}

# Calculates rolling mean if enough non-na provided
# TODO: code without zoo (use dplyr::lag/lead)
# TODO: document, test, and export
roll_mean = function(x, width, direction = "backward", fill = NA, min_n = 0){
  align = ifelse(direction == "backward", "right",
                 ifelse(direction == "forward", "left", "center"))
  zoo::rollapply(
    x, width = width, align = align, fill = fill,
    FUN = mean_if_enough, min_n = min_n) %>%
    round(1)
}


# TODO: do not use these - roll_mean is short enough now to use as is
# Calculate rolling 3 hour mean if at least 2 hours available
roll_mean_3hr_min_2 = function(x) {
  round(roll_mean(x, 3, min_n = 2), 1)
}

# TODO: do not use these - roll_mean is short enough now to use as is
# Calculate rolling 8 hour mean if at least 5 hours available
roll_mean_8hr_min_5 = function(x) {
  roll_mean(x, 8, min_n = 5)
}

# TODO: do not use these - roll_mean is short enough now to use as is
# Calculate rolling 24 hour mean if at least 15 hours available
roll_mean_24hr_min_15 = function(x) {
  roll_mean(x, 24, min_n = 15)
}

# Truncate to desired digits
trunc_n = function(x, n = 0){
  trunc(x*10^n)/10^n
}

# replace NA/inf with val
swap_na = function(x, val = -99) ifelse(is.na(x), val, x)
swap_inf = function(x, val = NA) ifelse(is.infinite(x), val, x)

# remove NA by default
mean_no_na = function(x, ...) mean(x, na.rm = T, ...)
min_no_na = function(x, ...) swap_inf(suppressWarnings(min(x, na.rm = T, ...)), NA)
max_no_na = function(x, ...) swap_inf(suppressWarnings(max(x, na.rm = T, ...)), NA)

# make backward looking rolling means (NAs not ignored)
# TODO: ignore NAs, make forward looking equivelant, incorp with roll_mean
get_lag_n_mean = function(x, n = 3){
  out = x
  if(n <= 1) stop("`n` must be greater than one")
  for(i in 1:(n-1)){
    out = out + dplyr::lag(x, i)
  }
  return(out/n)
}

standardize_colnames = function(df, all_col_names, raw = FALSE){
  if(raw){
    return(df)
  }else{
    col_names = all_col_names[all_col_names %in% names(df)]
    df %>%
      dplyr::rename_at(col_names, \(x) names(col_names)[col_names == x]) %>%
      dplyr::select_at(names(col_names))
  }
}





