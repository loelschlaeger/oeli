#' Check date format
#'
#' @description
#' This function checks if the input \code{date} has the format
#' \code{"YYYY-MM-DD"}.
#'
#' @param date \[`character(1)`\]\cr
#' The date in format \code{"YYYY-MM-DD"}.
#'
#' @return
#' \code{as.Date(date)} if \code{date} has the format \code{"YYYY-MM-DD"}.
#' Otherwise, the function throws an error.
#'
#' @keywords validation
#' @family date helpers
#' @export
#'
#' @examples
#' check_date(date = "2000-01-01")

check_date <- function(date) {
  date <- try(as.Date(date, format = "%Y-%m-%d"), silent = TRUE)
  if (inherits(date, "try-error") || anyNA(date)) {
    stop("Date is not in required format 'YYYY-MM-DD'.", call. = FALSE)
  }
  return(date)
}
