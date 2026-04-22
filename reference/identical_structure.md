# Check if two objects have identical structure

This function determines whether two objects have the same structure,

- which includes the [`mode`](https://rdrr.io/r/base/mode.html),
  [`class`](https://rdrr.io/r/base/class.html) and dimension

- but does *not* include concrete values or attributes.

## Usage

``` r
identical_structure(x, y)
```

## Arguments

- x, y:

  \[`any`\]  
  Two objects.

## Value

Either `TRUE` if `x` and `y` have the same structure, and `FALSE`, else.

## References

Inspired by <https://stackoverflow.com/a/45548885/15157768>.

## See also

Other package helpers:
[`Dictionary`](http://loelschlaeger.de/oeli/reference/Dictionary.md),
[`Storage`](http://loelschlaeger.de/oeli/reference/Storage.md),
[`check_missing()`](http://loelschlaeger.de/oeli/reference/check_missing.md),
[`find_namespace_calls()`](http://loelschlaeger.de/oeli/reference/find_namespace_calls.md),
[`input_check_response()`](http://loelschlaeger.de/oeli/reference/input_check_response.md),
[`match_arg()`](http://loelschlaeger.de/oeli/reference/match_arg.md),
[`package_logo()`](http://loelschlaeger.de/oeli/reference/package_logo.md),
[`print_data.frame()`](http://loelschlaeger.de/oeli/reference/print_data.frame.md),
[`print_matrix()`](http://loelschlaeger.de/oeli/reference/print_matrix.md),
[`system_information()`](http://loelschlaeger.de/oeli/reference/system_information.md),
[`unexpected_error()`](http://loelschlaeger.de/oeli/reference/unexpected_error.md),
[`user_confirm()`](http://loelschlaeger.de/oeli/reference/user_confirm.md)

## Examples

``` r
identical_structure(integer(1), 1L)
#> [1] TRUE
identical_structure(diag(2), matrix(rnorm(4), 2, 2))
#> [1] TRUE
identical_structure(diag(2), data.frame(diag(2)))
#> [1] FALSE
```
