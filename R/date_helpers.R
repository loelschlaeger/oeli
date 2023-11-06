#' Check date format
#'
#' @description
#' This function checks if the input \code{date} has the format
#' \code{"YYYY-MM-DD"}.
#'
#' @param date
#' A \code{character}, specifying a date in format \code{"YYYY-MM-DD"}.
#'
#' @return
#' \code{as.Date(date)} if \code{date} has the format \code{"YYYY-MM-DD"}.
#' Otherwise, the function throws an error.
#'
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

#' Find the closest year to a given date
#'
#' @description
#' This function takes a date as input and returns the closest year.
#'
#' @param date
#' A \code{date} in the format of \code{"YYYY-MM-DD"}.
#'
#' @return
#' An \code{integer}, the closest year to the input date.
#'
#' @export
#'
#' @examples
#' find_closest_year(as.Date("2022-07-15"))
#' find_closest_year(as.Date("2022-01-01"))

find_closest_year <- function(date) {
  year <- as.numeric(format(date, "%Y"))
  ifelse(
    date <= as.Date(paste0(year, "-06-30")),
    year,
    year + 1
  )
}
