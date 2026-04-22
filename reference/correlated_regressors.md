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
#> 1 1 1 -2.6373250  0.2665329 -1.172598
#> 2 1 3 -0.4739260 -0.9327829 -1.974470
#> 3 2 3 -2.1400109 -0.6313114 -1.576500
#> 4 0 2  0.4310060  1.1888854 -1.815358
#> 5 1 3  0.5392228 -0.8698426 -1.785320
#> 6 3 1 -0.8406857  1.0096034 -1.236701
```
