# Get function arguments

This function returns the names of function arguments.

## Usage

``` r
function_arguments(f, with_default = TRUE, with_ellipsis = TRUE)
```

## Arguments

- f:

  \[`function`\]  
  A `function`.

- with_default:

  \[`logical(1)`\]  
  Include function arguments that have default values?

- with_ellipsis:

  \[`logical(1)`\]  
  Include the `"..."` argument if present?

## Value

A `character` vector.

## See also

Other function helpers:
[`do.call_timed()`](http://loelschlaeger.de/oeli/reference/do.call_timed.md),
[`function_body()`](http://loelschlaeger.de/oeli/reference/function_body.md),
[`function_defaults()`](http://loelschlaeger.de/oeli/reference/function_defaults.md),
[`quiet()`](http://loelschlaeger.de/oeli/reference/quiet.md),
[`timed()`](http://loelschlaeger.de/oeli/reference/timed.md),
[`try_silent()`](http://loelschlaeger.de/oeli/reference/try_silent.md),
[`variable_name()`](http://loelschlaeger.de/oeli/reference/variable_name.md)

## Examples

``` r
f <- function(a, b = 1, c = "", ...) { }
function_arguments(f)
#> [1] "a"   "b"   "c"   "..."
function_arguments(f, with_default = FALSE)
#> [1] "a"   "..."
function_arguments(f, with_ellipsis = FALSE)
#> [1] "a" "b" "c"
```
