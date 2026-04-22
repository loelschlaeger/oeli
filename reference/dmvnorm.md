# Multivariate normal distribution

The function `dmvnorm()` computes the density of a multivariate normal
distribution.

The function `pmvnorm()` computes the cumulative distribution function
of a multivariate normal distribution.

The function `rmvnorm()` samples from a multivariate normal
distribution.

The functions with suffix `_cpp` perform no input checks, hence are
faster.

The univariate normal distribution is available as the special case
`p = 1`.

## Usage

``` r
dmvnorm_cpp(x, mean, Sigma, log = FALSE)

pmvnorm_cpp(x, mean, Sigma, abseps = 0.001)

rmvnorm_cpp(mean, Sigma, log = FALSE)

dmvnorm(x, mean, Sigma, log = FALSE)

pmvnorm(x, mean, Sigma, abseps = 0.001)

rmvnorm(n = 1, mean, Sigma, log = FALSE)
```

## Arguments

- x:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A quantile vector of length `p`.

- mean:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  The mean vector of length `p`.

  For the functions without suffix `_cpp`, it can also be of length `1`
  for convenience, then `rep(mean, p)` is considered.

- Sigma:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  The covariance matrix of dimension `p`.

  For `rmvnorm()`, arbitrary dimensions (i.e., full rows and
  corresponding columns) of `Sigma` can be `0`.

  For the functions without suffix `_cpp` and if `p = 1`, it can also be
  a single `numeric` for convenience. Note that `Sigma` is this case is
  a variance, which is a different format than in
  [`stats::dnorm()`](https://rdrr.io/r/stats/Normal.html) or
  [`stats::rnorm`](https://rdrr.io/r/stats/Normal.html), which require a
  standard deviation.

- log:

  \[`logical(1)`\]  
  Consider the log-normal distribution?

- abseps:

  \[`numeric(1)`\]  
  The absolute error tolerance.

- n:

  \[`integer(1)`\]  
  The number of requested samples.

## Value

For `dmvnorm()`: The density value.

For `pmvnorm()`: The value of the distribution function.

For `rmvnorm()`: If `n = 1` a `vector` of length `p` (note that it is a
column vector for `rmvnorm_cpp()`), else a `matrix` of dimension `n`
times `p` with samples as rows.

## Details

`pmvnorm()` just calls
[`mvtnorm::pmvnorm`](https://rdrr.io/pkg/mvtnorm/man/pmvnorm.html) with
the randomized Quasi-Monte-Carlo procedure by Genz and Bretz. The
argument `abseps` controls the accuracy of the Gaussian integral
approximation.

## See also

Other simulation helpers:
[`Simulator`](http://loelschlaeger.de/oeli/reference/Simulator.md),
[`correlated_regressors()`](http://loelschlaeger.de/oeli/reference/correlated_regressors.md),
[`ddirichlet_cpp()`](http://loelschlaeger.de/oeli/reference/ddirichlet.md),
[`dmixnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmixnorm.md),
[`dtnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dtnorm.md),
[`dwishart_cpp()`](http://loelschlaeger.de/oeli/reference/dwishart.md),
[`gaussian_tv()`](http://loelschlaeger.de/oeli/reference/gaussian_tv.md),
[`simulate_markov_chain()`](http://loelschlaeger.de/oeli/reference/simulate_markov_chain.md)

## Examples

``` r
x <- c(0, 0)
mean <- c(0, 0)
Sigma <- diag(2)

# compute density
dmvnorm(x = x, mean = mean, Sigma = Sigma)
#> [1] 0.1591549
dmvnorm(x = x, mean = mean, Sigma = Sigma, log = TRUE)
#> [1] -1.837877

# compute CDF
pmvnorm(x = x, mean = mean, Sigma = Sigma)
#> [1] 0.25

# sample
rmvnorm(n = 3, mean = mean, Sigma = Sigma)
#>            [,1]      [,2]
#> [1,] -1.5160239 1.6708361
#> [2,] -2.1268142 0.2473205
#> [3,] -0.3317292 0.6739188
rmvnorm(mean = mean, Sigma = Sigma, log = TRUE)
#> [1] 7.417526 0.299742
```
