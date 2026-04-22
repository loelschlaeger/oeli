# Merge named lists

This function merges `list`s based on their element names. Elements are
only included in the final output `list`, if no former `list` has
contributed an element with the same name.

## Usage

``` r
merge_lists(...)
```

## Arguments

- ...:

  One or more named `list`(s).

## Value

A `list`.

## See also

Other list helpers:
[`check_list_of_lists()`](http://loelschlaeger.de/oeli/reference/check_list_of_lists.md)

## Examples

``` r
merge_lists(list("a" = 1, "b" = 2), list("b" = 3, "c" = 4, "d" = NULL))
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 4
#> 
#> $d
#> NULL
#> 
```
