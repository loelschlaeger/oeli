# Silence R code

This function silences warnings, messages and any
[`cat()`](https://rdrr.io/r/base/cat.html) or
[`print()`](https://rdrr.io/r/base/print.html) output from R expressions
or functions.

## Usage

``` r
quiet(x, print_cat = TRUE, message = TRUE, warning = TRUE)
```

## Arguments

- x:

  \[`expression`\]  
  Any function or expression or value assignment expression.

- print_cat:

  \[`logical(1)`\]  
  Silence [`print()`](https://rdrr.io/r/base/print.html) and
  [`cat()`](https://rdrr.io/r/base/cat.html) outputs?

- message:

  \[`logical(1)`\]  
  Silence messages?

- warning:

  \[`logical(1)`\]  
  Silence warnings?

## Value

Invisibly the expression `x`.

## References

This function is a modified version of `quiet`.

## See also

Other function helpers:
[`do.call_timed()`](http://loelschlaeger.de/oeli/reference/do.call_timed.md),
[`function_arguments()`](http://loelschlaeger.de/oeli/reference/function_arguments.md),
[`function_body()`](http://loelschlaeger.de/oeli/reference/function_body.md),
[`function_defaults()`](http://loelschlaeger.de/oeli/reference/function_defaults.md),
[`timed()`](http://loelschlaeger.de/oeli/reference/timed.md),
[`try_silent()`](http://loelschlaeger.de/oeli/reference/try_silent.md),
[`variable_name()`](http://loelschlaeger.de/oeli/reference/variable_name.md)

## Examples

``` r
f <- function() {
  warning("warning")
  message("message")
  cat("cat")
  print("print")
}
quiet(f())
```
