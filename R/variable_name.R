#' Determine variable name
#'
#' @description
#' This function tries to determine the name of a variable passed to a
#' \code{function}.
#'
#' @param variable \[`any`\]\cr
#' Any object.
#'
#' @param fallback \[`character(1)`\]\cr
#' A fallback name if for some reason the actual variable name
#' (which must be a single \code{character}) cannot be determined.
#'
#' @return
#' A \code{character}, the variable name.
#'
#' @keywords functional
#' @family function helpers
#' @export
#'
#' @examples
#' variable_name(a)
#' f <- function(x) variable_name(x)
#' f(x = a)

variable_name <- function(variable, fallback = "unnamed") {
  argument_name <- deparse(
    eval.parent(substitute(substitute(variable))),
    width.cutoff = 500L
  )
  if (!checkmate::test_string(argument_name)) {
    checkmate::assert_string(fallback)
    argument_name <- fallback
  }
  return(argument_name)
}
