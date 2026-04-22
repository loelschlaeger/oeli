# Try an expression silently

This function tries to execute `expr` and returns a string with the
error message if the execution failed.

## Usage

``` r
try_silent(expr)
```

## Arguments

- expr:

  \[`expression`\]  
  An R expression to be evaluated.

## Value

Either the value of `expr` or in case of a failure an object of class
`fail`, which contains the error message.

## Details

This function is a wrapper for [`try`](https://rdrr.io/r/base/try.html).

## See also

Other function helpers:
[`do.call_timed()`](http://loelschlaeger.de/oeli/reference/do.call_timed.md),
[`function_arguments()`](http://loelschlaeger.de/oeli/reference/function_arguments.md),
[`function_body()`](http://loelschlaeger.de/oeli/reference/function_body.md),
[`function_defaults()`](http://loelschlaeger.de/oeli/reference/function_defaults.md),
[`quiet()`](http://loelschlaeger.de/oeli/reference/quiet.md),
[`timed()`](http://loelschlaeger.de/oeli/reference/timed.md),
[`variable_name()`](http://loelschlaeger.de/oeli/reference/variable_name.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try_silent(1 + 1)
try_silent(1 + "1")
} # }
```
