# Sample transition probability matrices

This function returns a random, squared matrix of dimension `dim` that
fulfills the properties of a transition probability matrix.

## Usage

``` r
sample_transition_probability_matrix(dim, state_persistent = TRUE)
```

## Arguments

- dim:

  \[`integer(1)`\]  
  The dimension.

- state_persistent:

  \[`logical(1)`\]  
  Put more probability on the diagonal?

## Value

A transition probability `matrix`.

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
[`sample_covariance_matrix()`](http://loelschlaeger.de/oeli/reference/sample_covariance_matrix.md),
[`stationary_distribution()`](http://loelschlaeger.de/oeli/reference/stationary_distribution.md)

## Examples

``` r
sample_transition_probability_matrix(dim = 3)
#>            [,1]      [,2]      [,3]
#> [1,] 0.63131883 0.1291792 0.2395020
#> [2,] 0.11764052 0.6424736 0.2398859
#> [3,] 0.06983393 0.3921604 0.5380057
```
