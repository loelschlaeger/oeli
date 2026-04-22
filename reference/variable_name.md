# Determine variable name

This function tries to determine the name of a variable passed to a
`function`.

## Usage

``` r
variable_name(variable, fallback = "unnamed")
```

## Arguments

- variable:

  \[`any`\]  
  Any object.

- fallback:

  \[`character(1)`\]  
  A fallback name if for some reason the actual variable name (which
  must be a single `character`) cannot be determined.

## Value

A `character`, the variable name.

## See also

Other function helpers:
[`do.call_timed()`](http://loelschlaeger.de/oeli/reference/do.call_timed.md),
[`function_arguments()`](http://loelschlaeger.de/oeli/reference/function_arguments.md),
[`function_body()`](http://loelschlaeger.de/oeli/reference/function_body.md),
[`function_defaults()`](http://loelschlaeger.de/oeli/reference/function_defaults.md),
[`quiet()`](http://loelschlaeger.de/oeli/reference/quiet.md),
[`timed()`](http://loelschlaeger.de/oeli/reference/timed.md),
[`try_silent()`](http://loelschlaeger.de/oeli/reference/try_silent.md)

## Examples

``` r
variable_name(a)
#> [1] "a"
f <- function(x) variable_name(x)
f(x = a)
#> [1] "a"
```
