# Mixture of normal distributions

The function `dmixnorm()` computes the density of a mixture of
multivariate normal distribution.

The function `pmixnorm()` computes the cumulative distribution function
of a mixture of multivariate normal distribution.

The function `rmixnorm()` samples from a mixture of multivariate normal
distribution.

The functions with suffix `_cpp` perform no input checks, hence are
faster.

The univariate normal mixture is available as the special case `p = 1`.

## Usage

``` r
dmixnorm_cpp(x, mean, Sigma, proportions, log = FALSE)

pmixnorm_cpp(
  x,
  mean,
  Sigma,
  proportions,
  abseps = 0.001,
  lower = NULL,
  method = "genz",
  draws = 500L
)

rmixnorm_cpp(mean, Sigma, proportions, log = FALSE)

dmixnorm(x, mean, Sigma, proportions, log = FALSE)

pmixnorm(
  x,
  mean,
  Sigma,
  proportions,
  abseps = 0.001,
  lower = NULL,
  method = "genz",
  draws = 500
)

rmixnorm(n = 1, mean, Sigma, proportions, log = FALSE)
```

## Arguments

- x:

  \[`numeric(p)`\]  
  A quantile vector of length `p`, where `p` is the dimension.

- mean:

  \[`matrix(nrow = p, ncol = c)`\]  
  The mean vectors for each component in columns.

- Sigma:

  \[`matrix(nrow = p^2, ncol = c)`\]  
  The vectorized covariance matrices for each component in columns.

- proportions:

  \[`numeric(c)`\]  
  The non-negative mixing proportions for each components.

  If proportions do not sum to unity, they are rescaled to do so.

- log:

  \[`logical(1)`\]  
  For `dmixnorm()`, return the logarithm of the density value?

  For `rmixnorm()`, return the exponential of the draw, which is a draw
  from the mixture of log-normal distributions?

- abseps:

  \[`numeric(1)`\]  
  The absolute error tolerance for `method = "genz"`.

- lower:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html) \| `NULL`\]  
  Optionally lower limits of length `p`, where `NULL` corresponds to
  `-Inf`.

  For the functions without suffix `_cpp`, it can also be of length `1`
  for convenience, then `rep(lower, p)` is considered.

- method:

  \[`character(1)`\]  
  Either `"genz"` or `"ghk"`, see the details.

- draws:

  \[`integer(1)`\]  
  The number of Halton points for `method = "ghk"`.

- n:

  \[`integer(1)`\]  
  The number of requested samples.

## Value

For `dmixnorm()`: The density value.

For `pmixnorm()`: The value of the distribution function or the
rectangle probability.

For `rmixnorm()`: If `n = 1` a `vector` of length `p` (note that it is a
column vector for `rmixnorm_cpp()`), else a `matrix` of dimension `n`
times `p` with samples as rows.

## Details

`pmixnorm()` is based on
[`pmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md), which
is exact for `p <= 3` and approximates the probability by the method
selected in `method` otherwise.

## See also

Other simulation helpers:
[`Simulator`](http://loelschlaeger.de/oeli/reference/Simulator.md),
[`correlated_regressors()`](http://loelschlaeger.de/oeli/reference/correlated_regressors.md),
[`ddirichlet_cpp()`](http://loelschlaeger.de/oeli/reference/ddirichlet.md),
[`dmvnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md),
[`dtnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dtnorm.md),
[`dwishart_cpp()`](http://loelschlaeger.de/oeli/reference/dwishart.md),
[`gaussian_tv()`](http://loelschlaeger.de/oeli/reference/gaussian_tv.md),
[`simulate_markov_chain()`](http://loelschlaeger.de/oeli/reference/simulate_markov_chain.md)

## Examples

``` r
x <- c(0, 0)
mean <- matrix(c(-1, -1, 0, 0), ncol = 2)
Sigma <- matrix(c(diag(2), diag(2)), ncol = 2)
proportions <- c(0.7, 0.3)

# compute density
dmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions)
#> [1] 0.08873136
dmixnorm(
  x = x, mean = mean, Sigma = Sigma, proportions = proportions, log = TRUE
)
#> [1] -2.422142

# compute CDF
pmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions)
#> [1] 0.5705027

# compute rectangle probability
pmixnorm(
  x = x, mean = mean, Sigma = Sigma, proportions = proportions, lower = -1
)
#> [1] 0.1165162

# sample
rmixnorm(n = 3, mean = mean, Sigma = Sigma, proportions = proportions)
#>            [,1]      [,2]
#> [1,] -1.3107073 -1.520797
#> [2,] -0.7407926 -1.594475
#> [3,]  1.6442855 -1.360091
```
