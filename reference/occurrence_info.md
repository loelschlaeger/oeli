# Provide information about occurrences

This function provides verbose information about absolute or relative
element occurrences in `data.frame` columns.

## Usage

``` r
occurrence_info(x, relative = FALSE, named = FALSE)
```

## Arguments

- x:

  \[`data.frame`\]  
  The object to check for occurrences.

- relative:

  \[`logical(1)` \]  
  The number of rows or columns to be printed, greater or equal `2`.

- named:

  \[`logical(1)` \]  
  Prepend column names of `x` (if not `NA`)?

## Value

A [`character()`](https://rdrr.io/r/base/character.html).

## See also

Other data.frame helpers:
[`delete_columns_data.frame()`](http://loelschlaeger.de/oeli/reference/delete_columns_data.frame.md),
[`group_data.frame()`](http://loelschlaeger.de/oeli/reference/group_data.frame.md),
[`round_data.frame()`](http://loelschlaeger.de/oeli/reference/round_data.frame.md)

## Examples

``` r
occurrence_info(datasets::warpbreaks, relative = FALSE, named = TRUE)
#> [1] "breaks: 4x 21, 4x 26, 4x 29, 3x 15, 3x 18, 3x 28, 2x 16, 2x 17, 2x 19, 2x 20, 2x 24, 2x 30, 2x 36, 2x 39, 1x 10, 1x 12, 1x 13, 1x 14, 1x 25, 1x 27, 1x 31, 1x 35, 1x 41, 1x 42, 1x 43, 1x 44, 1x 51, 1x 52, 1x 54, 1x 67, 1x 70"
#> [2] "wool: 27x A, 27x B"                                                                                                                                                                                                             
#> [3] "tension: 18x L, 18x M, 18x H"                                                                                                                                                                                                   
```
