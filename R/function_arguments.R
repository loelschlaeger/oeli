#' Get function arguments
#'
#' @description
#' This function returns the names of function arguments.
#'
#' @param f \[`function`\]\cr
#' A \code{function}.
#'
#' @param with_default \[`logical(1)`\]\cr
#' Include function arguments that have default values?
#'
#' @param with_ellipsis \[`logical(1)`\]\cr
#' Include the \code{"..."} argument if present?
#'
#' @return
#' A \code{character} vector.
#'
#' @keywords functional
#' @family function helpers
#' @export
#'
#' @examples
#' f <- function(a, b = 1, c = "", ...) { }
#' function_arguments(f)
#' function_arguments(f, with_default = FALSE)
#' function_arguments(f, with_ellipsis = FALSE)

function_arguments <- function(f, with_default = TRUE, with_ellipsis = TRUE) {
  checkmate::assert_function(f)
  checkmate::assert_flag(with_default)
  checkmate::assert_flag(with_ellipsis)
  args <- formals(f)
  if (is.null(args)) {
    return(character())
  }
  if (!with_default) {
    no_default <- vapply(args, function(arg) {
      is.symbol(arg) && !nzchar(as.character(arg))
    }, logical(1))
    args <- args[no_default]
  }
  if (!with_ellipsis) {
    args <- args[names(args) != "..."]
  }
  names(args)
}

