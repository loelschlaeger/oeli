# Insert column in matrix

This function inserts a column into a matrix.

## Usage

``` r
insert_matrix_column(A, x, p)
```

## Arguments

- A:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  A `matrix`.

- x:

  \[`atomic()`\]  
  The column to be added, of length `nrow(A)`.

  Can also be a single value.

- p:

  \[`integer())`\]  
  The position(s) where to add the column, one or more of:

  - `p = 0` appends the column left

  - `p = ncol(A)` appends the column right

  - `p = n` inserts the column between the `n`-th and `(n + 1)`-th
    column of `A`.

## Value

A `matrix`.

## See also

Other matrix helpers:
[`check_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/check_correlation_matrix.md),
[`check_covariance_matrix()`](http://loelschlaeger.de/oeli/reference/check_covariance_matrix.md),
[`check_one_hot_matrix()`](http://loelschlaeger.de/oeli/reference/check_one_hot_matrix.md),
[`check_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/check_transition_probability_matrix.md),
[`cov_to_chol()`](http://loelschlaeger.de/oeli/reference/cov_to_chol.md),
[`diff_cov()`](http://loelschlaeger.de/oeli/reference/diff_cov.md),
[`matrix_diagonal_indices()`](http://loelschlaeger.de/oeli/reference/matrix_diagonal_indices.md),
[`matrix_indices()`](http://loelschlaeger.de/oeli/reference/matrix_indices.md),
[`sample_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/sample_correlation_matrix.md),
[`sample_covariance_matrix()`](http://loelschlaeger.de/oeli/reference/sample_covariance_matrix.md),
[`sample_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/sample_transition_probability_matrix.md),
[`stationary_distribution()`](http://loelschlaeger.de/oeli/reference/stationary_distribution.md)

## Examples

``` r
A <- diag(3)
x <- 1:3
insert_matrix_column(A, x, 0)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    1    0    0
#> [2,]    2    0    1    0
#> [3,]    3    0    0    1
insert_matrix_column(A, x, 1)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    1    0    0
#> [2,]    0    2    1    0
#> [3,]    0    3    0    1
insert_matrix_column(A, x, 2)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    1    0
#> [2,]    0    1    2    0
#> [3,]    0    0    3    1
insert_matrix_column(A, x, 3)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    1
#> [2,]    0    1    0    2
#> [3,]    0    0    1    3

### also single value
x <- 2
insert_matrix_column(A, x, 0)
#>      [,1] [,2] [,3] [,4]
#> [1,]    2    1    0    0
#> [2,]    2    0    1    0
#> [3,]    2    0    0    1

### also multiple positions
insert_matrix_column(A, x, 0:3)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#> [1,]    2    1    2    0    2    0    2
#> [2,]    2    0    2    1    2    0    2
#> [3,]    2    0    2    0    2    1    2

### also trivial case
insert_matrix_column(matrix(nrow = 0, ncol = 0), integer(), integer())
#> <0 x 0 matrix>
```
