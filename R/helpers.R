# TODO: extend to other packages, implment throughout

#' @importFrom rlang `:=`
add_features <- function(obs, features, date_col = NULL) {
  features <- features[!features %in% names(obs)] # drop features already in obs
  if (length(features) == 0) {
    return(obs)
  }

  lubridate_funs <- getNamespaceExports("lubridate")
  lubridate_feats <- lubridate_funs[lubridate_funs %in% tolower(features)] |>
    sort()
  lubridate_feats <- lubridate_feats |>
    as.list() |>
    setNames(sort(features[tolower(features) %in% lubridate_feats]))
  if (length(lubridate_feats) > 0) {
    if (is.null(date_col)) {
      feats_pretty <- paste0("`", names(lubridate_feats), "`") |>
        stringr::str_flatten_comma(last = ", and ")
      msg <- "Since %s specified and not present in data, `date_col` must be provided." |>
        sprintf(feats_pretty)
      stop(msg)
    }
    features <- features[!features %in% names(lubridate_feats)]
    for (feature in names(lubridate_feats)) {
      FUN <- lubridate_feats[[feature]] |> get(envir = asNamespace("lubridate"))
      obs <- obs |> dplyr::mutate(!!feature := FUN(get(date_col)))
    }
  }

  if ("season" %in% tolower(features)) {
    feature <- features[tolower(features) == "season"]
    if (is.null(date_col)) {
      msg <- "Since `%s` specified and not present in data, `date_col` must be provided." |>
        sprintf(feature)
      stop(msg)
    }
    obs[[feature]] <- obs[[date_col]] |> handyr::get_season(as_factor = TRUE)
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
