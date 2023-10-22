# These functions allow for timing of R evaluations.

#' Interrupt long evaluations
#'
#' @description
#' This function evaluates \code{expr} and interrupts the evaluation after
#' \code{secs} seconds.
#'
#' @details
#' This function is a wrapper for \code{\link[R.utils]{withTimeout}}.
#'
#' @param expr
#' An R expression to be evaluated.
#' @param secs
#' The number of seconds.
#'
#' @return
#' Either the value of \code{expr} or \code{NULL} if the evaluation time
#' exceeded \code{secs} seconds.
#'
#' @examples
#' \dontrun{
#' foo <- function(x) { for(i in 1:10) Sys.sleep(x/10); return(x) }
#' timed(foo(0.5), 1)
#' timed(foo(1.5), 1)
#' }
#'
#' @export

timed <- function(expr, secs) {
  checkmate::assert_number(secs, lower = 0)
  setTimeLimit(cpu = secs, elapsed = secs, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  tryCatch(
    {
      expr
    },
    error = function(e) {
      msg <- e$message
      tl <- grepl("reached elapsed time limit|reached CPU time limit", msg)
      if (tl) return(NULL) else stop(msg, call. = FALSE)
    }
  )
}

#' Measure computation time
#'
#' @description
#' This function measures the computation time of a \code{do.call} call.
#'
#' @details
#' This function is a wrapper for \code{\link[base]{do.call}}.
#'
#' @param what
#' Passed to \code{\link[base]{do.call}}.
#' @param args
#' Passed to \code{\link[base]{do.call}}.
#'
#' @return
#' A list of the two elements \code{"result"} (the results of the \code{do.call}
#' call) and \code{"time"} (the computation time).
#'
#' @examples
#' \dontrun{
#' what <- function(s) { Sys.sleep(s); return(s) }
#' args <- list(s = 1)
#' do.call_timed(what = what, args = args)
#' }
#'
#' @export

do.call_timed <- function(what, args) {
  start <- Sys.time()
  res <- do.call(what = what, args = args)
  end <- Sys.time()
  total <- difftime(end, start)
  return(list("result" = res, "time" = total))
}
