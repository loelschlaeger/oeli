# Get default function arguments

This function returns the default function arguments (if any).

## Usage

``` r
function_defaults(f, exclude = NULL)
```

## Arguments

- f:

  \[`function`\]  
  A `function`.

- exclude:

  \[`NULL` \| [`character()`](https://rdrr.io/r/base/character.html)\]  
  Argument names to exclude.

  Can be `NULL` (default) to not exclude any argument names.

## Value

A named `list`.

## See also

Other function helpers:
[`do.call_timed()`](http://loelschlaeger.de/oeli/reference/do.call_timed.md),
[`function_arguments()`](http://loelschlaeger.de/oeli/reference/function_arguments.md),
[`function_body()`](http://loelschlaeger.de/oeli/reference/function_body.md),
[`quiet()`](http://loelschlaeger.de/oeli/reference/quiet.md),
[`timed()`](http://loelschlaeger.de/oeli/reference/timed.md),
[`try_silent()`](http://loelschlaeger.de/oeli/reference/try_silent.md),
[`variable_name()`](http://loelschlaeger.de/oeli/reference/variable_name.md)

## Examples

``` r
f <- function(a, b = 1, c = "", ...) { }
function_defaults(f)
#> $b
#> [1] 1
#> 
#> $c
#> [1] ""
#> 
function_defaults(f, exclude = "b")
#> $c
#> [1] ""
#> 
```
