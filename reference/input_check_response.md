# Standardized response to input check

This function provides a standardized response to input checks, ensuring
consistency.

## Usage

``` r
input_check_response(
  check,
  var_name = NULL,
  error = TRUE,
  prefix = "Input {.var {var_name}} is bad:"
)
```

## Arguments

- check:

  \[`TRUE` \| `character(1)` \|
  [`list()`](https://rdrr.io/r/base/list.html)\]  
  Matches the return value of the `check*` functions from the
  `{checkmate}` package, i.e., either `TRUE` if the check was
  successful, or a `character` (the error message) else.

  Can also be a `list` of multiple such values for alternative criteria,
  where at least one must be `TRUE` for a successful check.

- var_name:

  \[`NULL` \| `character(1)`\]  
  Optionally specifies the name of the input being checked. This name
  will be used for the default value of the `prefix` argument.

- error:

  \[`logical(1)`\]  
  If `check` is not `TRUE` (or no element in `check` is `TRUE`, if
  `check` is a `list`), throw an error?

- prefix:

  \[`character(1)`\]  
  A prefix for the thrown error message, only relevant if `error` is
  `TRUE`.

## Value

`TRUE` if `check` is `TRUE` (or any element in `check` is `TRUE`, if
`check` is a `list`) . Else, depending on `error`:

- If `error` is `TRUE`, throws an error.

- If `error` is `FALSE`, returns `FALSE`.

## See also

Other package helpers:
[`Dictionary`](http://loelschlaeger.de/oeli/reference/Dictionary.md),
[`Storage`](http://loelschlaeger.de/oeli/reference/Storage.md),
[`check_missing()`](http://loelschlaeger.de/oeli/reference/check_missing.md),
[`find_namespace_calls()`](http://loelschlaeger.de/oeli/reference/find_namespace_calls.md),
[`identical_structure()`](http://loelschlaeger.de/oeli/reference/identical_structure.md),
[`match_arg()`](http://loelschlaeger.de/oeli/reference/match_arg.md),
[`package_logo()`](http://loelschlaeger.de/oeli/reference/package_logo.md),
[`print_data.frame()`](http://loelschlaeger.de/oeli/reference/print_data.frame.md),
[`print_matrix()`](http://loelschlaeger.de/oeli/reference/print_matrix.md),
[`system_information()`](http://loelschlaeger.de/oeli/reference/system_information.md),
[`unexpected_error()`](http://loelschlaeger.de/oeli/reference/unexpected_error.md),
[`user_confirm()`](http://loelschlaeger.de/oeli/reference/user_confirm.md)

## Examples

``` r
x <- "1"
y <- 1

### check is successful
input_check_response(
  check = checkmate::check_character(x),
  var_name = "x",
  error = TRUE
)
#> [1] TRUE

### alternative checks
input_check_response(
  check = list(
    checkmate::check_character(x),
    checkmate::check_character(y)
  ),
  var_name = "x",
  error = TRUE
)
#> [1] TRUE

### standardized check response
if (FALSE) { # \dontrun{
input_check_response(
  check = checkmate::check_character(y),
  var_name = "y",
  error = TRUE
)

input_check_response(
  check = list(
    checkmate::check_flag(x),
    checkmate::check_character(y)
  ),
  var_name = "y",
  error = TRUE
)
} # }
```
