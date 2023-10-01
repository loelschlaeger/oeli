#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' @keywords internal
.onAttach <- function(libname, pkgname){
  options("oeli_connection" = stdin())
}
