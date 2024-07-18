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
