#' Extract function body
#'
#' @description
#' This function extracts the body of a function as a single \code{character}.
#'
#' @param fun \[`function`\]\cr
#' A \code{function}.
#'
#' @param braces \[`logical(1)`\]\cr
#' Remove \code{"{"} and \code{"}"} at start and end (if any)?
#'
#' @param nchar \[`integer(1)`\]\cr
#' The maximum number of characters before abbreviation, at least \code{3}.
#'
#' @return
#' A \code{character}, the body of \code{f}.
#'
#' @keywords functional
#' @family function helpers
#' @export
#'
#' @examples
#' fun <- mean.default
#' function_body(fun)
#' function_body(fun, braces = TRUE)
#' function_body(fun, nchar = 30)

function_body <- function(fun, braces = FALSE, nchar = getOption("width") - 4) {
  checkmate::assert_function(fun)
  checkmate::assert_flag(braces)
  checkmate::assert_int(nchar, lower = 3)
  out <- deparse1(body(fun))
  if (!braces) out <- gsub("^[{]|[}]$", "", out)
  out <- trimws(gsub("\\s+", " ", out))
  if (nchar(out) > nchar) out <- paste0(strtrim(out, nchar - 3), "...")
  out
}
