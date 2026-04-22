# Round `numeric` columns of a `data.frame`

This function rounds (only) the `numeric` columns of a `data.frame`.

## Usage

``` r
round_data.frame(df, digits = 0)
```

## Arguments

- df:

  \[`data.frame`\]  
  A `data.frame`.

- digits:

  \[`integer(1)` \| `NULL` \]  
  The number of decimal places to be used.

  Negative values are allowed, resulting in rounding to a power of ten.

  Can be `NULL` to not round.

## Value

A `data.frame`.

## See also

Other data.frame helpers:
[`delete_columns_data.frame()`](http://loelschlaeger.de/oeli/reference/delete_columns_data.frame.md),
[`group_data.frame()`](http://loelschlaeger.de/oeli/reference/group_data.frame.md),
[`occurrence_info()`](http://loelschlaeger.de/oeli/reference/occurrence_info.md)

## Examples

``` r
df <- data.frame("label" = c("A", "B"), "number" = rnorm(10))
round_data.frame(df, digits = 1)
#>    label number
#> 1      A   -1.0
#> 2      B    0.0
#> 3      A    0.7
#> 4      B    0.9
#> 5      A   -0.1
#> 6      B   -1.2
#> 7      A   -1.1
#> 8      B   -0.8
#> 9      A    1.1
#> 10     B   -0.2
```
