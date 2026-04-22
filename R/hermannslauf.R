# hermann_web <- rvest::read_html("https://de.wikipedia.org/wiki/Hermannslauf") |>
#   rvest::html_nodes("table") |>
#   rvest::html_table(fill = TRUE)
#
# hermann_men <- hermann_web[[3]] |>
#   dplyr::select(1:3) |>
#   dplyr::filter(Jahr != 2020) |>
#   dplyr::mutate(Zeit = lubridate::hms(Zeit)) |>
#   dplyr::rename("year" = "Jahr", "winner_men" = "Männer", "time_men" = "Zeit") |>
#   tsibble::as_tsibble(index = year) |>
#   tsibble::fill_gaps()
#
# hermann_women <- hermann_web[[3]] |>
#   dplyr::select(1, 4:5) |>
#   dplyr::filter(Jahr != 2020) |>
#   dplyr::mutate(Zeit = lubridate::hms(Zeit)) |>
#   dplyr::rename("year" = "Jahr", "winner_women" = "Frauen", "time_women" = "Zeit") |>
#   tsibble::as_tsibble(index = year) |>
#   tsibble::fill_gaps()
#
# hermann_weather <- tibble::tribble(
#   ~edition, ~year, ~date,    ~temp,
#   1,        1972,  "16.04.", NA,
#   2,        1973,  "29.04.", 14.0,
#   3,        1974,  "28.04.", 14.0,
#   4,        1975,  "27.04.", 11.0,
#   5,        1976,  "25.04.", 11.0,
#   6,        1977,  "24.04.", 9.0,
#   7,        1978,  "30.04.", 16.0,
#   8,        1979,  "29.04.", 7.0,
#   9,        1980,  "27.04.", 9.0,
#   10,       1981,  "26.04.", 16.0,
#   11,       1982,  "25.04.", 11.4,
#   12,       1983,  "24.04.", 10.0,
#   13,       1984,  "29.04.", 10.2,
#   14,       1985,  "28.04.", 6.9,
#   15,       1986,  "27.04.", 15.0,
#   16,       1987,  "26.04.", 15.9,
#   17,       1988,  "24.04.", 6.0,
#   18,       1989,  "30.04.", 11.9,
#   19,       1990,  "29.04.", 12.9,
#   20,       1991,  "28.04.", 7.3,
#   21,       1992,  "26.04.", 17.7,
#   22,       1993,  "25.04.", 20.1,
#   23,       1994,  "24.04.", 19.9,
#   24,       1995,  "30.04.", 12.2,
#   25,       1996,  "28.04.", 12.7,
#   26,       1997,  "27.04.", 11.0,
#   27,       1998,  "26.04.", 10.7,
#   28,       1999,  "25.04.", 15.0,
#   29,       2000,  "30.04.", 14.2,
#   30,       2001,  "29.04.", 14.4,
#   31,       2002,  "28.04.", 8.8,
#   32,       2003,  "27.04.", 13.7,
#   33,       2004,  "25.04.", 13.5,
#   34,       2005,  "24.04.", 16.1,
#   35,       2006,  "30.04.", 7.1,
#   36,       2007,  "29.04.", 15.8,
#   37,       2008,  "27.04.", 18.7,
#   38,       2009,  "26.04.", 20.6,
#   39,       2010,  "25.04.", 20.9,
#   40,       2011,  "17.04.", 15.5,
#   41,       2012,  "29.04.", 16.4,
#   42,       2013,  "28.04.", 7.5,
#   43,       2014,  "27.04.", 18.0,
#   44,       2015,  "26.04.", 14.0,
#   45,       2016,  "24.04.", 6.0,
#   46,       2017,  "30.04.", 12.0,
#   47,       2018,  "29.04.", 16.0,
#   48,       2019,  "28.04.", 10.0,
#   NA,       2020,  NA,       NA,
#   49,       2021,  "10.10.", 11.7,
#   50,       2022,  "24.04.", 12.7,
#   51,       2023,  "30.04.", 11.6,
#   52,       2024,  "28.04.", 17.6,
#   53,       2025,  "27.04.", 15.3
# )
#
# hermann <- hermann_weather |>
#   dplyr::left_join(hermann_men, by = "year") |>
#   dplyr::left_join(hermann_women, by = "year") |>
#   dplyr::mutate(date = lubridate::dmy(paste0(date, year)))

#' Hermannslauf
#'
#' @description
#' The Hermannslauf is an annual long-distance road and trail race in Germany
#' that runs from Detmold to Bielefeld through the Teutoburg Forest. It is one
#' of the best-known running events in the region and is especially noted for
#' its demanding, hilly course of about 31 kilometers. This dataset contains
#' historical information on editions of the race, including the date,
#' temperature, and winning times for men and women.
#'
#' From 1972 to 1976, the course followed the actual Hermannsweg for 30.4
#' kilometers, and in 1977 the first 13 kilometers were moved to a parallel
#' route. Since 2005, the total course length has been 31.1 kilometers; until
#' 2004, it was 30.6 kilometers. In 2020, the Hermannslauf was canceled due to
#' the pandemic, and for the same reason, in 2021 it was exceptionally moved
#' from April to October.
#'
#' @details
#' Temperature in degrees Celsius, measured at 12:00 noon, obtained from
#' different sources:
#' - from 1973 to 1992 and from 1998 to 2013 from
#' <https://de.weatherspark.com/>, Gütersloh Airport (approx. 30 km from start
#' and finish)
#' - from 1993 to 1997 from <https://de.weatherspark.com/>, Wunstorf Air Base
#' (approx. 100 km from start and finish)
#' - from 2014 to 2017 from <https://de.weatherspark.com/>, Hannover-Langenhagen
#' Airport (approx. 90 km from start and finish)
#' - from 2018 onward from <https://meteostat.net/de/station/D7106>,
#' Airfield Bielefeld-Windelsbleiche (approx. 20 km from start and finish)
#'
#' @format
#' A `tibble` with 54 rows and 8 columns:
#' \describe{
#'   \item{edition}{the edition of the Hermannslauf}
#'   \item{year}{the year}
#'   \item{date}{the date}
#'   \item{temp}{the temperature on that day at 12:00 noon; see details}
#'   \item{winner_men}{the men's winner}
#'   \item{time_men}{the men's winner's total time}
#'   \item{winner_women}{the women's winner}
#'   \item{time_women}{the women's winner's total time}
#' }
#'
#' @source <https://de.wikipedia.org/wiki/Hermannslauf>
#'
#' @keywords data
#' @family data

"hermann"
