# These functions provide tools for extracting function arguments.

#' Function arguments
#'
#' @description
#' This function returns the names of function arguments.
#'
#' @param f
#' A \code{function}.
#' @param with_default
#' Either \code{TRUE} to include function arguments that have default values,
#' or \code{FALSE} else.
#' @param with_ellipsis
#' Either \code{TRUE} to include the \code{"..."} argument if present,
#' or \code{FALSE} else.
#'
#' @return
#' A \code{character} vector.
#'
#' @examples
#' f <- function(a, b = 1, c = "", ...) { }
#' function_arguments(f)
#' function_arguments(f, with_default = FALSE)
#' function_arguments(f, with_ellipsis = FALSE)
#'
#' @export

function_arguments <- function(f, with_default = TRUE, with_ellipsis = TRUE) {
  checkmate::assert_function(f)
  checkmate::assert_flag(with_default)
  args <- formals(f)
  if (!with_default) {
    args <- args[sapply(seq_along(args), function(n) {
      !nzchar(args[[n]]) & is.name(args[[n]])
    })]
  }
  if (!with_ellipsis) {
    args <- args[names(args) != "..."]
  }
  names(args)
}
