# Cholesky root of covariance matrix

These functions compute the Cholesky root elements of a covariance
matrix and, conversely, build a covariance matrix from its Cholesky root
elements.

## Usage

``` r
cov_to_chol(cov, unique = TRUE)

chol_to_cov(chol)

unique_chol(chol)
```

## Arguments

- cov:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  A covariance matrix.

  It can also be the zero matrix, in which case the Cholesky root is
  defined as the zero matrix.

- unique:

  \[`logical(1)`\]  
  Ensure that the Cholesky decomposition is unique by restricting the
  diagonal elements to be positive?

- chol:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Cholesky root elements.

## Value

For `cov_to_chol` a `numeric` `vector` of Cholesky root elements.

For `chol_to_cov` a covariance `matrix`.

## See also

Other matrix helpers:
[`check_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/check_correlation_matrix.md),
[`check_covariance_matrix()`](http://loelschlaeger.de/oeli/reference/check_covariance_matrix.md),
[`check_one_hot_matrix()`](http://loelschlaeger.de/oeli/reference/check_one_hot_matrix.md),
[`check_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/check_transition_probability_matrix.md),
[`diff_cov()`](http://loelschlaeger.de/oeli/reference/diff_cov.md),
[`insert_matrix_column()`](http://loelschlaeger.de/oeli/reference/insert_matrix_column.md),
[`matrix_diagonal_indices()`](http://loelschlaeger.de/oeli/reference/matrix_diagonal_indices.md),
[`matrix_indices()`](http://loelschlaeger.de/oeli/reference/matrix_indices.md),
[`sample_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/sample_correlation_matrix.md),
[`sample_covariance_matrix()`](http://loelschlaeger.de/oeli/reference/sample_covariance_matrix.md),
[`sample_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/sample_transition_probability_matrix.md),
[`stationary_distribution()`](http://loelschlaeger.de/oeli/reference/stationary_distribution.md)

## Examples

``` r
cov <- sample_covariance_matrix(4)
chol <- cov_to_chol(cov)
all.equal(cov, chol_to_cov(chol))
#> [1] TRUE
```
