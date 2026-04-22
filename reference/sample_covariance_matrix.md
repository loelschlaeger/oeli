# Sample covariance matrix

This function samples a covariance matrix from an inverse Wishart
distribution.

## Usage

``` r
sample_covariance_matrix(dim, df = dim, scale = diag(dim), diag = FALSE)
```

## Arguments

- dim:

  \[`integer(1)`\]  
  The dimension.

- df:

  \[`integer(1)`\]  
  The degrees of freedom of the inverse Wishart distribution greater or
  equal `dim`.

- scale:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  The scale covariance matrix of the inverse Wishart distribution of
  dimension `dim`.

- diag:

  \[`logical(1)`\]  
  Diagonal matrix?

## Value

A covariance `matrix`.

## See also

Other matrix helpers:
[`check_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/check_correlation_matrix.md),
[`check_covariance_matrix()`](http://loelschlaeger.de/oeli/reference/check_covariance_matrix.md),
[`check_one_hot_matrix()`](http://loelschlaeger.de/oeli/reference/check_one_hot_matrix.md),
[`check_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/check_transition_probability_matrix.md),
[`cov_to_chol()`](http://loelschlaeger.de/oeli/reference/cov_to_chol.md),
[`diff_cov()`](http://loelschlaeger.de/oeli/reference/diff_cov.md),
[`insert_matrix_column()`](http://loelschlaeger.de/oeli/reference/insert_matrix_column.md),
[`matrix_diagonal_indices()`](http://loelschlaeger.de/oeli/reference/matrix_diagonal_indices.md),
[`matrix_indices()`](http://loelschlaeger.de/oeli/reference/matrix_indices.md),
[`sample_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/sample_correlation_matrix.md),
[`sample_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/sample_transition_probability_matrix.md),
[`stationary_distribution()`](http://loelschlaeger.de/oeli/reference/stationary_distribution.md)

## Examples

``` r
sample_covariance_matrix(dim = 3)
#>             [,1]       [,2]       [,3]
#> [1,]  15.7407402 -25.438378 -0.7518486
#> [2,] -25.4383783  42.235540  1.3456640
#> [3,]  -0.7518486   1.345664  0.2780764
```
