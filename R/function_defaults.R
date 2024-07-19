#' Get default function arguments
#'
#' @description
#' This function returns the default function arguments (if any).
#'
#' @param f \[`function`\]\cr
#' A \code{function}.
#'
#' @param exclude \[`NULL` | `character()`\]\cr
#' Argument names to exclude.
#'
#' Can be \code{NULL} (default) to not exclude any argument names.
#'
#' @return
#' A named \code{list}.
#'
#' @keywords functional
#' @family function helpers
#' @export
#'
#' @examples
#' f <- function(a, b = 1, c = "", ...) { }
#' function_defaults(f)
#' function_defaults(f, exclude = "b")

function_defaults <- function(f, exclude = NULL) {
  checkmate::assert_function(f)
  checkmate::assert_character(exclude, null.ok = TRUE)
  formals_f <- formals(f)
  formals_f <- formals_f[!sapply(formals_f, is.symbol)]
  formals_f <- formals_f[!names(formals_f) %in% exclude]
  return(formals_f)
}
