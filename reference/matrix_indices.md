# Get `matrix` indices

This function returns `matrix` indices as `character`.

## Usage

``` r
matrix_indices(x, prefix = "", exclude_diagonal = FALSE)
```

## Arguments

- x:

  \[`matrix`\]  
  A `matrix`.

- prefix:

  \[`character(1)`\]  
  A prefix for the indices.

- exclude_diagonal:

  \[`logical(1)`\]  
  Exclude indices where row equals column?

## Value

A `character` `vector`.

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
[`sample_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/sample_correlation_matrix.md),
[`sample_covariance_matrix()`](http://loelschlaeger.de/oeli/reference/sample_covariance_matrix.md),
[`sample_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/sample_transition_probability_matrix.md),
[`stationary_distribution()`](http://loelschlaeger.de/oeli/reference/stationary_distribution.md)

## Examples

``` r
M <- diag(3)
matrix_indices(M)
#> [1] "11" "21" "31" "12" "22" "32" "13" "23" "33"
matrix_indices(M, "M_")
#> [1] "M_11" "M_21" "M_31" "M_12" "M_22" "M_32" "M_13" "M_23" "M_33"
matrix_indices(M, "M_", TRUE)
#> [1] "M_21" "M_31" "M_12" "M_32" "M_13" "M_23"
```
