# Get indices of matrix diagonal

This function returns the indices of the diagonal elements of a
quadratic matrix.

## Usage

``` r
matrix_diagonal_indices(n, triangular = NULL)
```

## Arguments

- n:

  \[`integer(1)`\]  
  The matrix dimension.

- triangular:

  \[`NULL` or `character(1)`\]  
  If `NULL` (default), all elements of the matrix are considered. If
  `"lower"` (`"upper"`), only the lower- (upper-) triangular matrix is
  considered.

## Value

An `integer` `vector`.

## See also

Other matrix helpers:
[`check_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/check_correlation_matrix.md),
[`check_covariance_matrix()`](http://loelschlaeger.de/oeli/reference/check_covariance_matrix.md),
[`check_one_hot_matrix()`](http://loelschlaeger.de/oeli/reference/check_one_hot_matrix.md),
[`check_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/check_transition_probability_matrix.md),
[`cov_to_chol()`](http://loelschlaeger.de/oeli/reference/cov_to_chol.md),
[`diff_cov()`](http://loelschlaeger.de/oeli/reference/diff_cov.md),
[`insert_matrix_column()`](http://loelschlaeger.de/oeli/reference/insert_matrix_column.md),
[`matrix_indices()`](http://loelschlaeger.de/oeli/reference/matrix_indices.md),
[`sample_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/sample_correlation_matrix.md),
[`sample_covariance_matrix()`](http://loelschlaeger.de/oeli/reference/sample_covariance_matrix.md),
[`sample_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/sample_transition_probability_matrix.md),
[`stationary_distribution()`](http://loelschlaeger.de/oeli/reference/stationary_distribution.md)

## Examples

``` r
# indices of diagonal elements
n <- 3
matrix(1:n^2, n, n)
#>      [,1] [,2] [,3]
#> [1,]    1    4    7
#> [2,]    2    5    8
#> [3,]    3    6    9
matrix_diagonal_indices(n)
#> [1] 1 5 9

# indices of diagonal elements of lower-triangular matrix
L <- matrix(0, n, n)
L[lower.tri(L, diag=TRUE)] <- 1:((n * (n + 1)) / 2)
L
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    2    4    0
#> [3,]    3    5    6
matrix_diagonal_indices(n, triangular = "lower")
#> [1] 1 4 6

# indices of diagonal elements of upper-triangular matrix
U <- matrix(0, n, n)
U[upper.tri(U, diag=TRUE)] <- 1:((n * (n + 1)) / 2)
U
#>      [,1] [,2] [,3]
#> [1,]    1    2    4
#> [2,]    0    3    5
#> [3,]    0    0    6
matrix_diagonal_indices(n, triangular = "upper")
#> [1] 1 3 6
```
