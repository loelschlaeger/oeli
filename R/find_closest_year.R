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
