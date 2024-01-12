#' Get function arguments
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
      any(!nzchar(args[[n]]) & is.name(args[[n]]))
    })]
  }
  if (!with_ellipsis) {
    args <- args[names(args) != "..."]
  }
  names(args)
}

#' Get default function arguments
#'
#' @description
#' This function returns the default function arguments (if any).
#'
#' @param f
#' A \code{function}.
#' @param exclude
#' A \code{character} of argument names to exclude. Can be \code{NULL} (default)
#' to not exclude any argument names.
#'
#' @return
#' A named \code{list}.
#'
#' @examples
#' f <- function(a, b = 1, c = "", ...) { }
#' function_defaults(f)
#' function_defaults(f, exclude = "b")
#'
#' @export

function_defaults <- function(f, exclude = NULL) {
  checkmate::assert_function(f)
  checkmate::assert_character(exclude, null.ok = TRUE)
  formals_f <- formals(f)
  formals_f <- formals_f[!sapply(formals_f, is.symbol)]
  formals_f <- formals_f[!names(formals_f) %in% exclude]
  return(formals_f)
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
  if (!braces) out <- gsub("^[{]|[}]$", "", out)
  out <- trimws(gsub("\\s+", " ", out))
  if (nchar(out) > nchar) out <- paste0(strtrim(out, nchar - 3), "...")
  out
}

#' Determine variable name
#'
#' @description
#' This function tries to determine the name of a variable passed to a
#' \code{function}.
#'
#' @param variable
#' Any object.
#' @param fallback
#' A \code{character}, a fallback if for some reason the actual variable name
#' (which must be a single \code{character}) cannot be determined.
#'
#' @return
#' A \code{character}, the variable name.
#'
#' @examples
#' variable_name(a)
#' f <- function(x) variable_name(x)
#' f(x = a)
#'
#' @export

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
