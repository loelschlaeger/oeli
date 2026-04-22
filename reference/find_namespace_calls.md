# Namespace calls

This function searches for namespace calls in `.R` files, i.e., code
lines of the format `<package name>::<function name>`.

## Usage

``` r
find_namespace_calls(path = "R", triple_colon = FALSE, as_list = FALSE)
```

## Arguments

- path:

  \[`character(1)`\]  
  The path name to a folder. All `.R` files in this folder and
  sub-directories will be searched.

- triple_colon:

  \[`logical(1)`\]  
  Also search for `:::`?

- as_list:

  \[`logical(1)`\]  
  Simplify the output into a `list` of unique function names per
  package?

## Value

A `data.frame`. If `as_list = TRUE`, a `list`.

## See also

Other package helpers:
[`Dictionary`](http://loelschlaeger.de/oeli/reference/Dictionary.md),
[`Storage`](http://loelschlaeger.de/oeli/reference/Storage.md),
[`check_missing()`](http://loelschlaeger.de/oeli/reference/check_missing.md),
[`identical_structure()`](http://loelschlaeger.de/oeli/reference/identical_structure.md),
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
if (FALSE) { # \dontrun{
find_namespace_calls()
find_namespace_calls(as_list = TRUE)
} # }
```
