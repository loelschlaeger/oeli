#' Interrupt long evaluations
#'
#' @description
#' This function interrupts an evaluation after a certain number of seconds.
#'
#' @details
#' This function is a wrapper for \code{\link[R.utils]{withTimeout}}.
#'
#' @param expression
#' An R expression to be evaluated.
#' @param seconds
#' The number of seconds.
#'
#' @return
#' Either the value of \code{expression} or \code{NULL} if the evaluation time
#' exceeded \code{seconds} seconds.
#'
#' @examples
#' \dontrun{
#' foo <- function(x) { for(i in 1:10) Sys.sleep(x/10); return(x) }
#' timed(foo(0.5), 1)
#' timed(foo(1.5), 1)
#' }
#'
#' @export

timed <- function(expression, seconds = Inf) {
  checkmate::assert_number(seconds, lower = 0)
  setTimeLimit(cpu = seconds, elapsed = seconds, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  tryCatch(
    expression,
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
#' @param units
#' Passed to \code{\link[base]{difftime}}.
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

do.call_timed <- function(what, args, units = "secs") {
  start <- Sys.time()
  res <- do.call(what = what, args = args)
  end <- Sys.time()
  list("result" = res, "time" = difftime(end, start, units = units))
}
