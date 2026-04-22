# Check covariance matrix

These functions check whether the input fulfills the properties of a
covariance matrix.

## Usage

``` r
check_covariance_matrix(x, dim = NULL, tolerance = sqrt(.Machine$double.eps))

assert_covariance_matrix(
  x,
  dim = NULL,
  tolerance = sqrt(.Machine$double.eps),
  .var.name = checkmate::vname(x),
  add = NULL
)

test_covariance_matrix(x, dim = NULL, tolerance = sqrt(.Machine$double.eps))
```

## Arguments

- x:

  \[`any`\]  
  Object to check.

- dim:

  \[`integer(1)`\]  
  The matrix dimension.

- tolerance:

  \[`numeric(1)`\]  
  A non-negative tolerance value.

- .var.name:

  \[`character(1)`\]  
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

- add:

  \[`AssertCollection`\]  
  Collection to store assertion messages. See
  [`AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

## Value

Same as documented in
[`check_matrix`](https://mllg.github.io/checkmate/reference/checkMatrix.html).

## See also

Other matrix helpers:
[`check_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/check_correlation_matrix.md),
[`check_one_hot_matrix()`](http://loelschlaeger.de/oeli/reference/check_one_hot_matrix.md),
[`check_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/check_transition_probability_matrix.md),
[`cov_to_chol()`](http://loelschlaeger.de/oeli/reference/cov_to_chol.md),
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
M <- matrix(c(1, 2, 3, 2, 1, 2, 3, 2, 1), nrow = 3)
check_covariance_matrix(M)
#> [1] "Must have positive eigenvalues only"
test_covariance_matrix(M)
#> [1] FALSE
if (FALSE) { # \dontrun{
assert_covariance_matrix(M)
} # }
```
