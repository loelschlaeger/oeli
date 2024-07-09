#' Response to an input check
#'
#' @description
#' Provides standardized responses to input checks, ensuring consistency.
#'
#' @param check (`TRUE` or `character()`)\cr
#' Either `TRUE` if the check was successful, or an error message else.
#'
#' This input type matches the return value of the `check*` functions from the
#' `{checkmate}` package.
#'
#' @param var_name (`NULL` or `character(1)`)\cr
#' Optionally specifies the name of the input being checked. This name will be
#' used as the default value for the `prefix` argument.
#'
#' @param error (`logical(1)`)\cr
#' If `check` is not `TRUE`, throw an error or return `FALSE`?
#'
#' @param prefix (`character(1)`)\cr
#' A prefix for the thrown error message, if `check` is not `TRUE` and
#' `error` is `TRUE`.
#'
#' @return
#' `TRUE` if `check` is `TRUE`. If `check` is not `TRUE`, depending on `error`:
#'
#' - If `error` is `TRUE`, throws an error.
#' - If `error` is `FALSE`, returns `FALSE`.
#'
#' @examples
#' x <- "1"
#' y <- 1
#'
#' ### check is successful
#' input_check_response(
#'   check = checkmate::check_character(x),
#'   var_name = "x",
#'   error = TRUE
#' )
#'
#' ### standardized check response
#' \dontrun{
#' input_check_response(
#'   check = checkmate::check_character(y),
#'   var_name = "y",
#'   error = TRUE
#' )
#' }
#'
#' @export
#' @keywords packaging
#' @family package helpers

input_check_response <- function(
    check, var_name = NULL, error = TRUE, prefix = "Input {var_name} is bad:"
  ) {
  if (!isTRUE(check)) {
    if (isTRUE(error)) {
      checkmate::assert_string(prefix, null.ok = TRUE)
      cli::cli_abort(paste(prefix, "{check}"), call = NULL)
    } else {
      FALSE
    }
  } else {
    TRUE
  }
}
