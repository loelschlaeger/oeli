#' Standardized response to input check
#'
#' @description
#' This function provides a standardized response to input checks, ensuring
#' consistency.
#'
#' @param check \[`TRUE` | `character(1)` | `list()`\]\cr
#' Matches the return value of the `check*` functions from the `{checkmate}`
#' package, i.e., either `TRUE` if the check was successful, or a `character`
#' (the error message) else.
#'
#' Can also be a `list` of multiple such values for alternative criteria, where
#' at least one must be `TRUE` for a successful check.
#'
#' @param var_name \[`NULL` | `character(1)`\]\cr
#' Optionally specifies the name of the input being checked. This name will be
#' used for the default value of the `prefix` argument.
#'
#' @param error \[`logical(1)`\]\cr
#' If `check` is not `TRUE` (or no element in `check` is `TRUE`, if `check` is
#' a `list`), throw an error?
#'
#' @param prefix \[`character(1)`\]\cr
#' A prefix for the thrown error message, only relevant if `error` is `TRUE`.
#'
#' @return
#' `TRUE` if `check` is `TRUE` (or any element in `check` is `TRUE`, if `check`
#' is a `list`) . Else, depending on `error`:
#'
#' - If `error` is `TRUE`, throws an error.
#' - If `error` is `FALSE`, returns `FALSE`.
#'
#' @export
#' @keywords packaging
#' @family package helpers
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
#' ### alternative checks
#' input_check_response(
#'   check = list(
#'     checkmate::check_character(x),
#'     checkmate::check_character(y)
#'   ),
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
#'
#' input_check_response(
#'   check = list(
#'     checkmate::check_flag(x),
#'     checkmate::check_character(y)
#'   ),
#'   var_name = "y",
#'   error = TRUE
#' )
#' }

input_check_response <- function(
    check,
    var_name = NULL,
    error = TRUE,
    prefix = "Input {.var {var_name}} is bad:"
  ) {
  if (is.list(check)) {
    check_true <- sapply(check, isTRUE)
    check_result <- any(check_true)
    check_msg <- check[!check_true]
  } else {
    check_result <- check_msg <- check
  }
  if (!isTRUE(check_result)) {
    if (isTRUE(error)) {
      checkmate::assert_string(prefix, null.ok = TRUE)
      if (length(check_msg) > 1) {
        cli::cli_abort(
          c(paste(prefix, "Either:"), paste0("- ", check_msg)),
          call = NULL
        )
      } else {
        cli::cli_abort(
          paste(prefix, "{check_msg}"),
          call = NULL
        )
      }
    } else {
      FALSE
    }
  } else {
    TRUE
  }
}
