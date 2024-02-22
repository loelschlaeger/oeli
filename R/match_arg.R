#' Argument matching
#'
#' @description
#' This function matches function arguments and is a modified version of
#' \code{\link[base]{match.arg}}.
#'
#' @param arg
#' A \code{character} (vector), the function argument.
#' @param choices
#' A \code{character} (vector) of allowed values for \code{arg}.
#' @param several.ok
#' Either \code{TRUE} if \code{arg} is allowed to have more than one element,
#' or \code{FALSE} else.
#' @param none.ok
#' Either \code{TRUE} if \code{arg} is allowed to have zero elements,
#' or \code{FALSE} else.
#'
#' @return
#' The un-abbreviated version of the exact or unique partial match if there is
#' one. Otherwise, an error is signaled if \code{several.ok} is \code{FALSE}
#' or \code{none.ok} is \code{FALSE}.
#' When \code{several.ok} is \code{TRUE} and (at least) one element of
#' \code{arg} has a match, all un-abbreviated versions of matches are returned.
#' When \code{none.ok} is \code{TRUE} and \code{arg} has zero elements,
#' \code{character(0)} is returned.
#'
#' @export

match_arg <- function(arg, choices, several.ok = FALSE, none.ok = FALSE) {
  checkmate::assert_character(arg)
  checkmate::assert_character(choices)
  checkmate::assert_flag(several.ok)
  checkmate::assert_flag(none.ok)
  arg_name <- deparse(substitute(arg))
  if (!several.ok && length(arg) > 1L) {
    cli::cli_abort(
      "{.var {arg_name}} must be of length 1.",
      call = NULL
    )
  }
  if (length(arg) == 0L) {
    if (none.ok) {
      return(character(0))
    } else {
      cli::cli_abort(
        "{.var {arg_name}} must be of length greater or equal 1.",
        call = NULL
      )
    }
  }
  i <- pmatch(arg, choices, nomatch = 0, duplicates.ok = TRUE)
  if (all(i == 0L)) {
    cli::cli_abort(
      "{.var {arg_name}} {ifelse(none.ok, 'can', 'must')} be one
      {ifelse(several.ok, 'or more', '')} of {.val {choices}}.",
      call = NULL
    )
  }
  i <- i[i > 0L]
  choices[i]
}
