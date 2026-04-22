# Extract function body

This function extracts the body of a function as a single `character`.

## Usage

``` r
function_body(fun, braces = FALSE, nchar = getOption("width") - 4)
```

## Arguments

- fun:

  \[`function`\]  
  A `function`.

- braces:

  \[`logical(1)`\]  
  Remove `"{"` and `"}"` at start and end (if any)?

- nchar:

  \[`integer(1)`\]  
  The maximum number of characters before abbreviation, at least `3`.

## Value

A `character`, the body of `f`.

## See also

Other function helpers:
[`do.call_timed()`](http://loelschlaeger.de/oeli/reference/do.call_timed.md),
[`function_arguments()`](http://loelschlaeger.de/oeli/reference/function_arguments.md),
[`function_defaults()`](http://loelschlaeger.de/oeli/reference/function_defaults.md),
[`quiet()`](http://loelschlaeger.de/oeli/reference/quiet.md),
[`timed()`](http://loelschlaeger.de/oeli/reference/timed.md),
[`try_silent()`](http://loelschlaeger.de/oeli/reference/try_silent.md),
[`variable_name()`](http://loelschlaeger.de/oeli/reference/variable_name.md)

## Examples

``` r
fun <- mean.default
function_body(fun)
#> [1] "if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) { warning(\"argume..."
function_body(fun, braces = TRUE)
#> [1] "{ if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) { warning(\"argu..."
function_body(fun, nchar = 30)
#> [1] "if (!is.numeric(x) && !is.c..."
```
