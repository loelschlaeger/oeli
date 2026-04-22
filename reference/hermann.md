# Hermannslauf

The Hermannslauf is an annual long-distance road and trail race in
Germany that runs from Detmold to Bielefeld through the Teutoburg
Forest. It is one of the best-known running events in the region and is
especially noted for its demanding, hilly course of about 31 kilometers.
This dataset contains historical information on editions of the race,
including the date, temperature, and winning times for men and women.

From 1972 to 1976, the course followed the actual Hermannsweg for 30.4
kilometers, and in 1977 the first 13 kilometers were moved to a parallel
route. Since 2005, the total course length has been 31.1 kilometers;
until 2004, it was 30.6 kilometers. In 2020, the Hermannslauf was
canceled due to the pandemic, and for the same reason, in 2021 it was
exceptionally moved from April to October.

## Usage

``` r
hermann
```

## Format

A `tibble` with 54 rows and 8 columns:

- edition:

  the edition of the Hermannslauf

- year:

  the year

- date:

  the date

- temp:

  the temperature on that day at 12:00 noon; see details

- winner_men:

  the men's winner

- time_men:

  the men's winner's total time

- winner_women:

  the women's winner

- time_women:

  the women's winner's total time

## Source

<https://de.wikipedia.org/wiki/Hermannslauf>

## Details

Temperature in degrees Celsius, measured at 12:00 noon, obtained from
different sources:

- from 1973 to 1992 and from 1998 to 2013 from
  <https://de.weatherspark.com/>, Gütersloh Airport (approx. 30 km from
  start and finish)

- from 1993 to 1997 from <https://de.weatherspark.com/>, Wunstorf Air
  Base (approx. 100 km from start and finish)

- from 2014 to 2017 from <https://de.weatherspark.com/>,
  Hannover-Langenhagen Airport (approx. 90 km from start and finish)

- from 2018 onward from <https://meteostat.net/de/station/D7106>,
  Airfield Bielefeld-Windelsbleiche (approx. 20 km from start and
  finish)
