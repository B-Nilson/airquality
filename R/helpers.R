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
    for (feature in lubridate_feats) {
      FUN <- feature |> get(envir = asNamespace("lubridate"))
      obs <- obs |> dplyr::mutate(!!feature := FUN(get(date_col)))
    }
  }
  return(obs)
}
