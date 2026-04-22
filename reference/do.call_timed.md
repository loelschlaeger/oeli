# Measure computation time

This function measures the computation time of a call.

## Usage

``` r
do.call_timed(what, args, units = "secs")
```

## Arguments

- what, args:

  Passed to [`do.call`](https://rdrr.io/r/base/do.call.html).

- units:

  Passed to [`difftime`](https://rdrr.io/r/base/difftime.html).

## Value

A list of the two elements `"result"` (the results of the `do.call`
call) and `"time"` (the computation time).

## Details

This function is a wrapper for
[`do.call`](https://rdrr.io/r/base/do.call.html).

## See also

Other function helpers:
[`function_arguments()`](http://loelschlaeger.de/oeli/reference/function_arguments.md),
[`function_body()`](http://loelschlaeger.de/oeli/reference/function_body.md),
[`function_defaults()`](http://loelschlaeger.de/oeli/reference/function_defaults.md),
[`quiet()`](http://loelschlaeger.de/oeli/reference/quiet.md),
[`timed()`](http://loelschlaeger.de/oeli/reference/timed.md),
[`try_silent()`](http://loelschlaeger.de/oeli/reference/try_silent.md),
[`variable_name()`](http://loelschlaeger.de/oeli/reference/variable_name.md)

## Examples

``` r
if (FALSE) { # \dontrun{
what <- function(s) {
  Sys.sleep(s)
  return(s)
}
args <- list(s = 1)
do.call_timed(what = what, args = args)
} # }
```
