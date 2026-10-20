# Simulate correlated regressor values

This function simulates regressor values from various marginal
distributions with custom correlations.

## Usage

``` r
correlated_regressors(
  labels,
  n = 100,
  marginals = list(),
  correlation = diag(length(labels)),
  verbose = FALSE
)
```

## Arguments

- labels:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Unique labels for the regressors.

- n:

  \[`integer(1)`\]  
  The number of values per regressor.

- marginals:

  \[[`list()`](https://rdrr.io/r/base/list.html)\]  
  Optionally marginal distributions for regressors. If not specified,
  standard normal marginal distributions are used.

  Each list entry must be named according to a regressor label, and the
  following distributions are currently supported:

  discrete distributions

  :   - Poisson: `list(type = "poisson", lambda = ...)`

      - categorical: `list(type = "categorical", p = c(...))`

  continuous distributions

  :   - normal: `list(type = "normal", mean = ..., sd = ...)`

      - uniform: `list(type = "uniform", min = ..., max = ...)`

- correlation:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  A correlation matrix of dimension `length(labels)`, where the
  `(p, q)`-th entry defines the correlation between regressor
  `labels[p]` and `labels[q]`.

- verbose:

  \[`logical(1)`\]  
  Print information about the simulated regressors?

## Value

A `data.frame` with `n` rows and `length(labels)` columns.

## References

This function heavily depends on the `{SimMultiCorrData}` package.

## See also

Other simulation helpers:
[`Simulator`](http://loelschlaeger.de/oeli/reference/Simulator.md),
[`ddirichlet_cpp()`](http://loelschlaeger.de/oeli/reference/ddirichlet.md),
[`dmixnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmixnorm.md),
[`dmvnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md),
[`dtnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dtnorm.md),
[`dwishart_cpp()`](http://loelschlaeger.de/oeli/reference/dwishart.md),
[`gaussian_tv()`](http://loelschlaeger.de/oeli/reference/gaussian_tv.md),
[`simulate_markov_chain()`](http://loelschlaeger.de/oeli/reference/simulate_markov_chain.md)

## Examples

``` r
labels <- c("P", "C", "N1", "N2", "U")
n <- 100
marginals <- list(
  "P" = list(type = "poisson", lambda = 2),
  "C" = list(type = "categorical", p = c(0.3, 0.2, 0.5)),
  "N1" = list(type = "normal", mean = -1, sd = 2),
  "U" = list(type = "uniform", min = -2, max = -1)
)
correlation <- matrix(
  c(1, -0.3, -0.1, 0, 0.5,
    -0.3, 1, 0.3, -0.5, -0.7,
    -0.1, 0.3, 1, -0.3, -0.3,
    0, -0.5, -0.3, 1, 0.1,
    0.5, -0.7, -0.3, 0.1, 1),
  nrow = 5, ncol = 5
)
data <- correlated_regressors(
  labels = labels, n = n, marginals = marginals, correlation = correlation
)
head(data)
#>   P C         N1         N2         U
#> 1 1 3  0.2303533 -0.6720559 -1.966467
#> 2 3 3 -1.4359753 -1.7488395 -1.243314
#> 3 3 1  1.4865290 -0.7830588 -1.050264
#> 4 2 3 -0.7761238  0.1553673 -1.687554
#> 5 1 2 -0.7579894  0.8989081 -1.632314
#> 6 2 2 -4.0991499  1.0853635 -1.663881
```
