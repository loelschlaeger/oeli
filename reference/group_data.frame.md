# Grouping of a `data.frame`

This function groups a `data.frame` according to values of one column.

## Usage

``` r
group_data.frame(df, by, keep_by = TRUE)
```

## Arguments

- df:

  \[`data.frame`\]  
  A `data.frame`.

- by:

  \[`character(1)`\]  
  The name of a column of `df` to group by.

- keep_by:

  \[`logical(1)`\]  
  Keep the grouping column `by`?

## Value

A `list` of `data.frame`s, subsets of `df`.

## See also

Other data.frame helpers:
[`delete_columns_data.frame()`](http://loelschlaeger.de/oeli/reference/delete_columns_data.frame.md),
[`occurrence_info()`](http://loelschlaeger.de/oeli/reference/occurrence_info.md),
[`round_data.frame()`](http://loelschlaeger.de/oeli/reference/round_data.frame.md)

## Examples

``` r
df <- data.frame("label" = c("A", "B"), "number" = 1:10)
group_data.frame(df = df, by = "label")
#> $A
#>   label number
#> 1     A      1
#> 3     A      3
#> 5     A      5
#> 7     A      7
#> 9     A      9
#> 
#> $B
#>    label number
#> 2      B      2
#> 4      B      4
#> 6      B      6
#> 8      B      8
#> 10     B     10
#> 
group_data.frame(df = df, by = "label", keep_by = FALSE)
#> $A
#>   number
#> 1      1
#> 3      3
#> 5      5
#> 7      7
#> 9      9
#> 
#> $B
#>    number
#> 2       2
#> 4       4
#> 6       6
#> 8       8
#> 10     10
#> 
```
