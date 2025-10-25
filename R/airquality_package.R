#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}