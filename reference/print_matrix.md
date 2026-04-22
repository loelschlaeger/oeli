# Print (abbreviated) `matrix`

This function prints a (possibly abbreviated) `matrix`.

## Usage

``` r
print_matrix(
  x,
  rowdots = 4,
  coldots = 4,
  digits = 2,
  label = NULL,
  simplify = FALSE,
  details = !simplify
)
```

## Arguments

- x:

  \[`atomic()` \| `matrix`\]  
  The object to be printed.

- rowdots:

  \[`integer(1)`\]  
  The row number which is replaced by `...`.

- coldots:

  \[`integer(1)`\]  
  The column number which is replaced by `...`.

- digits:

  \[`integer(1)`\]  
  The number of printed decimal places if input `x` is `numeric`.

- label:

  \[`character(1)`\]  
  A label for `x`. Only printed if `simplify = FALSE`.

- simplify:

  \[`logical(1)`\]  
  Simplify the output?

- details:

  \[`logical(1)`\]  
  Print the type and dimension of `x`?

## Value

Invisibly returns `x`.

## References

This function is a modified version of `pprint()` from the `{ramify}`
package.

## See also

Other package helpers:
[`Dictionary`](http://loelschlaeger.de/oeli/reference/Dictionary.md),
[`Storage`](http://loelschlaeger.de/oeli/reference/Storage.md),
[`check_missing()`](http://loelschlaeger.de/oeli/reference/check_missing.md),
[`find_namespace_calls()`](http://loelschlaeger.de/oeli/reference/find_namespace_calls.md),
[`identical_structure()`](http://loelschlaeger.de/oeli/reference/identical_structure.md),
[`input_check_response()`](http://loelschlaeger.de/oeli/reference/input_check_response.md),
[`match_arg()`](http://loelschlaeger.de/oeli/reference/match_arg.md),
[`package_logo()`](http://loelschlaeger.de/oeli/reference/package_logo.md),
[`print_data.frame()`](http://loelschlaeger.de/oeli/reference/print_data.frame.md),
[`system_information()`](http://loelschlaeger.de/oeli/reference/system_information.md),
[`unexpected_error()`](http://loelschlaeger.de/oeli/reference/unexpected_error.md),
[`user_confirm()`](http://loelschlaeger.de/oeli/reference/user_confirm.md)

## Examples

``` r
print_matrix(x = 1, label = "single numeric")
#> single numeric : 1
print_matrix(x = LETTERS[1:26], label = "letters")
#> letters : character vector of length 26 
#> A B C ... Z
print_matrix(x = 1:3, coldots = 2)
#> double vector of length 3 
#> 1 ... 3
print_matrix(x = matrix(rnorm(99), ncol = 1), label = "single column matrix")
#> single column matrix : 99 x 1 matrix of doubles 
#>        [,1]
#> [1,]  -0.63
#> [2,]   0.79
#> [3,]   -1.3
#> ...     ...
#> [99,] -0.28
print_matrix(x = matrix(1:100, nrow = 1), label = "single row matrix")
#> single row matrix : 1 x 100 matrix of doubles 
#>      [,1] [,2] [,3] ... [,100]
#> [1,]    1    2    3 ...    100
print_matrix(x = matrix(LETTERS[1:24], ncol = 6), label = "big matrix")
#> big matrix : 4 x 6 matrix of characters 
#>      [,1] [,2] [,3] ... [,6]
#> [1,]    A    E    I ...    U
#> [2,]    B    F    J ...    V
#> [3,]    C    G    K ...    W
#> [4,]    D    H    L ...    X
print_matrix(x = diag(5), coldots = 2, rowdots = 2, simplify = TRUE)
#> [ 1 ... 0; ... ... ...; 0 ... 1 ]
```
