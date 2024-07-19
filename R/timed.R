#' Interrupt long evaluations
#'
#' @description
#' This function interrupts an evaluation after a certain number of seconds.
#' Note the limitations documented in \code{\link[base]{setTimeLimit}}.
#'
#' @param expression \[`expression`\]\cr
#' An R expression to be evaluated.
#'
#' @param seconds \[`numeric(1)`\]\cr
#' The number of seconds.
#'
#' @param on_time_out \[`character(1)`\]\cr
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
#' @keywords simulation
#' @family function helpers
#' @export
#'
#' @examples
#' foo <- function(x) {
#'   for (i in 1:10) Sys.sleep(x / 10)
#'   return(x)
#' }
#' timed(foo(0.5), 1)
#' timed(foo(1.5), 1)

timed <- function(
    expression, seconds = Inf, on_time_out = "silent"
  ) {
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
