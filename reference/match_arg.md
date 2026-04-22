# Argument matching

This function matches function arguments and is a modified version of
[`match.arg`](https://rdrr.io/r/base/match.arg.html).

## Usage

``` r
match_arg(arg, choices, several.ok = FALSE, none.ok = FALSE)
```

## Arguments

- arg:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  The function argument.

- choices:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Allowed values for `arg`.

- several.ok:

  \[`logical(1)`\]  
  Is `arg` allowed to have more than one element?

- none.ok:

  \[`logical(1)`\]  
  Is `arg` allowed to have zero elements?

## Value

The un-abbreviated version of the exact or unique partial match if there
is one. Otherwise, an error is signaled if `several.ok` is `FALSE` or
`none.ok` is `FALSE`. When `several.ok` is `TRUE` and (at least) one
element of `arg` has a match, all un-abbreviated versions of matches are
returned. When `none.ok` is `TRUE` and `arg` has zero elements,
`character(0)` is returned.

## See also

Other package helpers:
[`Dictionary`](http://loelschlaeger.de/oeli/reference/Dictionary.md),
[`Storage`](http://loelschlaeger.de/oeli/reference/Storage.md),
[`check_missing()`](http://loelschlaeger.de/oeli/reference/check_missing.md),
[`find_namespace_calls()`](http://loelschlaeger.de/oeli/reference/find_namespace_calls.md),
[`identical_structure()`](http://loelschlaeger.de/oeli/reference/identical_structure.md),
[`input_check_response()`](http://loelschlaeger.de/oeli/reference/input_check_response.md),
[`package_logo()`](http://loelschlaeger.de/oeli/reference/package_logo.md),
[`print_data.frame()`](http://loelschlaeger.de/oeli/reference/print_data.frame.md),
[`print_matrix()`](http://loelschlaeger.de/oeli/reference/print_matrix.md),
[`system_information()`](http://loelschlaeger.de/oeli/reference/system_information.md),
[`unexpected_error()`](http://loelschlaeger.de/oeli/reference/unexpected_error.md),
[`user_confirm()`](http://loelschlaeger.de/oeli/reference/user_confirm.md)
