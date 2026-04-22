# Check missing formal argument

These functions check whether a value was specified as an argument to a
function.

## Usage

``` r
check_missing(x)

assert_missing(x)

test_missing(x)
```

## Arguments

- x:

  \[`any`\]  
  A formal argument.

## Value

Depending on the function prefix:

- If the check is successful, `assert_missing()` returns `x` invisibly,
  whereas `check_missing()` and `test_missing()` return `TRUE`.

- If the check is not successful, `assert_missing()` throws an error
  message, `test_missing()` returns `FALSE`, and `check_missing()`
  returns a string with the error message.

## See also

Other package helpers:
[`Dictionary`](http://loelschlaeger.de/oeli/reference/Dictionary.md),
[`Storage`](http://loelschlaeger.de/oeli/reference/Storage.md),
[`find_namespace_calls()`](http://loelschlaeger.de/oeli/reference/find_namespace_calls.md),
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
f <- function(x) {
  check_missing(x)
}
f()
#> [1] "Argument needs a value"

g <- function(x) {
  test_missing(x)
}
g()
#> [1] FALSE

h <- function(x) {
  assert_missing(x)
}
if (FALSE) { # \dontrun{
h()
} # }
```
