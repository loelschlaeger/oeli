# Difference and un-difference covariance matrix

These functions difference and un-difference random vectors and
covariance matrices.

## Usage

``` r
diff_cov(cov, ref = 1)

undiff_cov(cov_diff, ref = 1)

delta(ref = 1, dim)

M(ranking = seq_len(dim), dim)
```

## Arguments

- cov, cov_diff:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  A (differenced) covariance matrix of dimension `dim` (or `dim` - 1,
  respectively).

- ref:

  \[`integer(1)`\]  
  The reference row between `1` and `dim` for differencing that maps
  `cov` to `cov_diff`, see details.

- dim:

  \[`integer(1)`\]  
  The matrix dimension.

- ranking:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The integers `1` to `dim` in arbitrary order.

## Value

A (differenced or un-differenced) covariance `matrix`.

## Details

Assume \\x \sim N(0, \Sigma)\\ is a multivariate normally distributed
random vector of dimension \\n\\. We may want to consider the
differenced vector \$\$\tilde x = (x_1 - x_k, x_2 - x_k, \dots, x_n -
x_k)',\$\$ excluding the \\k\\th element (hence, \\\tilde x\\ is of
dimension \\(n - 1) \times 1\\). Formally, \\\tilde x = \Delta_k x\\,
where \\\Delta_k\\ is a difference operator that depends on the
reference row \\k\\. More precise, \\\Delta_k\\ is the identity matrix
of dimension \\n\\ without row \\k\\ and with \\-1\\s in column \\k\\.
The difference operator \\\Delta_k\\ can be computed via
`delta(ref = k, dim = n)`.

Then, \\\tilde x \sim N(0, \tilde \Sigma)\\, where \$\$\tilde{\Sigma} =
\Delta_k \Sigma \Delta_k'\$\$ is the differenced covariance matrix with
respect to row \\k = 1,\dots,n\\. The differenced covariance matrix
\\\tilde \Sigma\\ can be computed via `diff_delta(Sigma, ref = k)`.

Since \\\Delta_k\\ is a non-bijective mapping, \\\Sigma\\ cannot be
uniquely restored from \\\tilde \Sigma\\. However, it is possible to
compute a non-unique solution \\\Sigma_0\\, such that \\\Delta_k
\Sigma_0 \Delta_k = \tilde \Sigma\\. For such a non-unique solution, we
add a column and a row of zeros at column and row number \\k\\ to
\\\tilde{\Sigma}\\, respectively. An "un-differenced" covariance matrix
\\\Sigma_0\\ can be computed via `undiff_delta(Sigma_diff, ref = k)`.

As a alternative to \\\Delta_k\\, the function `M()` returns a matrix
for taking differences such that the resulting vector is negative.

## See also

Other matrix helpers:
[`check_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/check_correlation_matrix.md),
[`check_covariance_matrix()`](http://loelschlaeger.de/oeli/reference/check_covariance_matrix.md),
[`check_one_hot_matrix()`](http://loelschlaeger.de/oeli/reference/check_one_hot_matrix.md),
[`check_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/check_transition_probability_matrix.md),
[`cov_to_chol()`](http://loelschlaeger.de/oeli/reference/cov_to_chol.md),
[`insert_matrix_column()`](http://loelschlaeger.de/oeli/reference/insert_matrix_column.md),
[`matrix_diagonal_indices()`](http://loelschlaeger.de/oeli/reference/matrix_diagonal_indices.md),
[`matrix_indices()`](http://loelschlaeger.de/oeli/reference/matrix_indices.md),
[`sample_correlation_matrix()`](http://loelschlaeger.de/oeli/reference/sample_correlation_matrix.md),
[`sample_covariance_matrix()`](http://loelschlaeger.de/oeli/reference/sample_covariance_matrix.md),
[`sample_transition_probability_matrix()`](http://loelschlaeger.de/oeli/reference/sample_transition_probability_matrix.md),
[`stationary_distribution()`](http://loelschlaeger.de/oeli/reference/stationary_distribution.md)

## Examples

``` r
n <- 4
Sigma <- sample_covariance_matrix(dim = n)
k <- 2
x <- c(1, 3, 2, 4)

# build difference operator
delta_k <- delta(ref = k, dim = n)

# difference vector
delta_k %*% x
#>      [,1]
#> [1,]   -2
#> [2,]   -1
#> [3,]    1

# difference Sigma
(Sigma_diff <- diff_cov(Sigma, ref = k))
#>             [,1]        [,2]      [,3]
#> [1,]  182.956274 -215.548168  7.890516
#> [2,] -215.548168  263.979088 -7.872328
#> [3,]    7.890516   -7.872328  1.825093

# un-difference Sigma
(Sigma_0 <- undiff_cov(Sigma_diff, ref = k))
#>             [,1] [,2]        [,3]      [,4]
#> [1,]  182.956274    0 -215.548168  7.890516
#> [2,]    0.000000    0    0.000000  0.000000
#> [3,] -215.548168    0  263.979088 -7.872328
#> [4,]    7.890516    0   -7.872328  1.825093

# difference again
Sigma_diff_2 <- diff_cov(Sigma_0, ref = k)
all.equal(Sigma_diff, Sigma_diff_2)
#> [1] TRUE

# difference such that the resulting vector is negative
M(ranking = order(x, decreasing = TRUE), dim = n) %*% x
#>      [,1]
#> [1,]   -1
#> [2,]   -1
#> [3,]   -1
```
