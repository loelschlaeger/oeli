# Print (abbreviated) `data.frame`

This function prints a (possibly abbreviated) `data.frame`.

## Usage

``` r
print_data.frame(
  x,
  rows = NULL,
  cols = NULL,
  digits = NULL,
  row.names = TRUE,
  col.names = TRUE
)
```

## Arguments

- x:

  \[`data.frame`\]  
  A `data.frame`.

- rows, cols:

  \[`integer(1)` \| `NULL` \]  
  The number of rows or columns to be printed, greater or equal `2`.

  Printing is abbreviated in the middle.

  Can be `NULL` to print everything.

- digits:

  \[`integer(1)` \| `NULL` \]  
  The number of decimal places to be used.

  Negative values are allowed, resulting in rounding to a power of ten.

  Can be `NULL` to not round.

- row.names, col.names:

  \[`logical(1)`\]  
  Print row names or column names?

## Value

Invisibly returns `x`.

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
[`print_matrix()`](http://loelschlaeger.de/oeli/reference/print_matrix.md),
[`system_information()`](http://loelschlaeger.de/oeli/reference/system_information.md),
[`unexpected_error()`](http://loelschlaeger.de/oeli/reference/unexpected_error.md),
[`user_confirm()`](http://loelschlaeger.de/oeli/reference/user_confirm.md)

## Examples

``` r
x <- data.frame(1:10, LETTERS[1:10], stats::rnorm(10))
print_data.frame(x, rows = 7)
#>    X1.10 LETTERS.1.10. stats..rnorm.10.
#> 1   1    A              1.2889860      
#> 2   2    B             -0.2753581      
#> 3   3    C              0.5311298      
#> 4   4    D             -0.4484039      
#> 
#> <3 rows hidden>
#>                                        
#> 8   8    H             -0.2501448      
#> 9   9    I             -0.4603740      
#> 10 10    J             -0.2508766      
print_data.frame(x, rows = 7, cols = 2)
#>    X1.10 <1 col hidden> stats..rnorm.10.
#> 1   1          -         1.2889860      
#> 2   2          -        -0.2753581      
#> 3   3          -         0.5311298      
#> 4   4          -        -0.4484039      
#> 
#> <3 rows hidden>
#>                                         
#> 8   8          -        -0.2501448      
#> 9   9          -        -0.4603740      
#> 10 10          -        -0.2508766      
print_data.frame(x, rows = 7, cols = 2, digits = 1)
#>    X1.10 <1 col hidden> stats..rnorm.10.
#> 1   1          -         1.3            
#> 2   2          -        -0.3            
#> 3   3          -         0.5            
#> 4   4          -        -0.4            
#> 
#> <3 rows hidden>
#>                                         
#> 8   8          -        -0.3            
#> 9   9          -        -0.5            
#> 10 10          -        -0.3            
print_data.frame(x, rows = 7, cols = 2, digits = 1, row.names = FALSE)
#>  X1.10 <1 col hidden> stats..rnorm.10.
#>   1          -         1.3            
#>   2          -        -0.3            
#>   3          -         0.5            
#>   4          -        -0.4            
#> 
#> <3 rows hidden>
#>                                       
#>   8          -        -0.3            
#>   9          -        -0.5            
#>  10          -        -0.3            
print_data.frame(x, rows = 7, cols = 2, digits = 1, col.names = FALSE)
#>       <1 col hidden>     
#> 1   1       -         1.3
#> 2   2       -        -0.3
#> 3   3       -         0.5
#> 4   4       -        -0.4
#> 
#> <3 rows hidden>
#>                          
#> 8   8       -        -0.3
#> 9   9       -        -0.5
#> 10 10       -        -0.3
```
