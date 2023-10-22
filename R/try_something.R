# These functions help to evaluate an expression which potentially will

#' Try an expression silently
#'
#' @description
#' This function tries to execute \code{expr} and returns a string with the
#' error message if the execution failed.
#'
#' @details
#' This function is a wrapper for \code{\link[base]{try}}.
#'
#' @param expr
#' An R expression to try.
#'
#' @return
#' Either the value of \code{expr} or in case of a failure an object of class
#' \code{fail}, which contains the error message.
#'
#' @examples
#' \dontrun{
#' try_silent(1 + 1)
#' try_silent(1 + "1")
#' }
#'
#' @export

try_silent <- function(expr) {
  out <- suppressWarnings(try(expr, silent = TRUE))
  if ("try-error" %in% class(out)) {
    out <- structure(as.character(out[1]), class = "fail")
  }
  return(out)
}
