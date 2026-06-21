# TODO: extend to other packages, implment throughout

#' @importFrom rlang `:=`
add_features <- function(obs, features, date_col = NULL) {
  features <- features[!features %in% names(obs)] # drop features already in obs
  if (length(features) == 0) {
    return(obs)
  }

  lubridate_funs <- getNamespaceExports("lubridate")
  lubridate_feats <- lubridate_funs[lubridate_funs %in% features]
  if (length(lubridate_feats) > 0) {
    stopifnot(
      "date_col must be provided for lubridate features" = !is.null(date_col)
    )
    features <- features[!features %in% lubridate_feats]
    for (feature in lubridate_feats) {
      FUN <- feature |> get(envir = asNamespace("lubridate"))
      obs <- obs |> dplyr::mutate(!!feature := FUN(get(date_col)))
    }
  }

  if ("season" %in% features) {
    stopifnot(
      "date_col must be provided for determining season" = !is.null(date_col)
    )
    obs$season <- obs[[date_col]] |>
      handyr::get_season(as_factor = TRUE)
  }
  return(obs)
}

# Used for ensureing fill scales in [ggplot2::ggplot()] show limits
breaks_min_max <- function(x) {
  pretty <- scales::breaks_pretty()(x)
  step <- pretty[2] - pretty[1]

  # Drop pretty breaks too close to the true min/max to avoid crowding
  pretty <- pretty[
    abs(pretty - x[1]) > step * 0.25 &
      abs(pretty - x[2]) > step * 0.25
  ]
  sort(unique(c(x[1], pretty, x[2])))
}
