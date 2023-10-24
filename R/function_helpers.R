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
  if (is.null(args)) {
    return(character())
  }
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

#' Extract function body
#'
#' @description
#' This function extracts the body of a function as a single \code{character}.
#'
#' @param fun
#' A \code{function}.
#' @param braces
#' Either \code{FALSE} (default) to remove \code{"{"} and \code{"}"}
#' at start and end (if any), or \code{TRUE} if not.
#' @param nchar
#' An \code{integer}, the maximum number of characters before abbreviation.
#' Must be at least \code{3}.
#' By default, \code{nchar = getOption("width") - 4}.
#'
#' @return
#' A \code{character}, the body of \code{f}.
#'
#' @examples
#' fun <- mean.default
#' function_body(fun)
#' function_body(fun, braces = TRUE)
#' function_body(fun, nchar = 30)
#'
#' @export

function_body <- function(fun, braces = FALSE, nchar = getOption("width") - 4) {
  checkmate::assert_function(fun)
  checkmate::assert_flag(braces)
  checkmate::assert_int(nchar, lower = 3)
  out <- deparse1(body(fun))
  if (!braces) out <- gsub("^[{]|[}]$","", out)
  out <- trimws(gsub("\\s+", " ", out))
  if (nchar(out) > nchar) out <- paste0(strtrim(out, nchar - 3), '...')
  out
}
