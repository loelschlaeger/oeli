#' Interrupt long evaluations
#'
#' @description
#' This function interrupts an evaluation after a certain number of seconds.
#' Note the limitations documented in \code{\link[base]{setTimeLimit}}.
#'
#' @param expression
#' An R expression to be evaluated.
#' @param seconds
#' The number of seconds.
#' @param on_time_out
#' Defines what action to take if the evaluation time exceeded, either:
#' \itemize{
#'   \item \code{"error"} to throw an error exception
#'   \item \code{"warning"} to return \code{NULL} along with a warning
#'   \item \code{"silent"} (the default) to just return \code{NULL}
#' }
#'
#' @return
#' The value of \code{expression} or, if the evaluation time exceeded, whatever
#' is specified for \code{on_time_out}.
#'
#' @examples
#' foo <- function(x) {
#'   for (i in 1:10) Sys.sleep(x / 10)
#'   return(x)
#' }
#' timed(foo(0.5), 1)
#' timed(foo(1.5), 1)
#'
#' @export

timed <- function(
    expression, seconds = Inf, on_time_out = "silent") {
  checkmate::assert_number(seconds, lower = 0)
  checkmate::assert_choice(on_time_out, c("error", "warning", "silent"))
  setTimeLimit(cpu = seconds, elapsed = seconds, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  tryCatch(
    expression,
    error = function(e) {
      msg <- e$message
      time_out <- grepl("reached elapsed time limit|reached CPU time limit", msg)
      if (time_out) {
        if (on_time_out == "error") {
          stop("time limit exceeded", call. = FALSE)
        } else if (on_time_out == "warning") {
          warning("time limit exceeded", call. = FALSE)
        }
        return(NULL)
      } else {
        stop(msg, call. = FALSE)
      }
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
#' what <- function(s) {
#'   Sys.sleep(s)
#'   return(s)
#' }
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
