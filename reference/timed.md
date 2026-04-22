# Interrupt long evaluations

This function interrupts an evaluation after a certain number of
seconds. Note the limitations documented in
[`setTimeLimit`](https://rdrr.io/r/base/setTimeLimit.html).

## Usage

``` r
timed(expression, seconds = Inf, on_time_out = "silent")
```

## Arguments

- expression:

  \[`expression`\]  
  An R expression to be evaluated.

- seconds:

  \[`numeric(1)`\]  
  The number of seconds.

- on_time_out:

  \[`character(1)`\]  
  Defines what action to take if the evaluation time exceeded, either:

  - `"error"` to throw an error exception

  - `"warning"` to return `NULL` along with a warning

  - `"silent"` (the default) to just return `NULL`

## Value

The value of `expression` or, if the evaluation time exceeded, whatever
is specified for `on_time_out`.

## See also

Other function helpers:
[`do.call_timed()`](http://loelschlaeger.de/oeli/reference/do.call_timed.md),
[`function_arguments()`](http://loelschlaeger.de/oeli/reference/function_arguments.md),
[`function_body()`](http://loelschlaeger.de/oeli/reference/function_body.md),
[`function_defaults()`](http://loelschlaeger.de/oeli/reference/function_defaults.md),
[`quiet()`](http://loelschlaeger.de/oeli/reference/quiet.md),
[`try_silent()`](http://loelschlaeger.de/oeli/reference/try_silent.md),
[`variable_name()`](http://loelschlaeger.de/oeli/reference/variable_name.md)

## Examples

``` r
foo <- function(x) {
  for (i in 1:10) Sys.sleep(x / 10)
  return(x)
}
timed(foo(0.5), 1)
#> [1] 0.5
timed(foo(1.5), 1)
#> NULL
```
