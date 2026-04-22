# Deleting `data.frame` columns

This function deletes columns of a `data.frame` by name.

## Usage

``` r
delete_columns_data.frame(df, column_names)
```

## Arguments

- df:

  \[`data.frame`\]  
  A `data.frame`.

- column_names:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  The name(s) of column(s) of `df` to delete.

## Value

The input `df` without the columns defined by `column_names`.

## See also

Other data.frame helpers:
[`group_data.frame()`](http://loelschlaeger.de/oeli/reference/group_data.frame.md),
[`occurrence_info()`](http://loelschlaeger.de/oeli/reference/occurrence_info.md),
[`round_data.frame()`](http://loelschlaeger.de/oeli/reference/round_data.frame.md)

## Examples

``` r
df <- data.frame("label" = c("A", "B"), "number" = 1:10)
delete_columns_data.frame(df = df, column_names = "label")
#>    number
#> 1       1
#> 2       2
#> 3       3
#> 4       4
#> 5       5
#> 6       6
#> 7       7
#> 8       8
#> 9       9
#> 10     10
delete_columns_data.frame(df = df, column_names = "number")
#>    label
#> 1      A
#> 2      B
#> 3      A
#> 4      B
#> 5      A
#> 6      B
#> 7      A
#> 8      B
#> 9      A
#> 10     B
delete_columns_data.frame(df = df, column_names = c("label", "number"))
#> data frame with 0 columns and 10 rows
```
