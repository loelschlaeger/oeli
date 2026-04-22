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
dmixnorm_cpp(x, mean, Sigma, proportions)

pmixnorm_cpp(x, mean, Sigma, proportions, abseps = 0.001)

rmixnorm_cpp(mean, Sigma, proportions)

dmixnorm(x, mean, Sigma, proportions)

pmixnorm(x, mean, Sigma, proportions, abseps = 0.001)

rmixnorm(n = 1, mean, Sigma, proportions)
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

- abseps:

  \[`numeric(1)`\]  
  The absolute error tolerance.

- n:

  \[`integer(1)`\]  
  The number of requested samples.

## Value

For `dmixnorm()`: The density value.

For `pmixnorm()`: The value of the distribution function.

For `rmixnorm()`: If `n = 1` a `vector` of length `p` (note that it is a
column vector for `rmixnorm_cpp()`), else a `matrix` of dimension `n`
times `p` with samples as rows.

## Details

`pmixnorm()` is based on
[`mvtnorm::pmvnorm`](https://rdrr.io/pkg/mvtnorm/man/pmvnorm.html) with
the randomized Quasi-Monte-Carlo procedure by Genz and Bretz. The
argument `abseps` controls the accuracy of the Gaussian integral
approximation.

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

# compute CDF
pmixnorm(x = x, mean = mean, Sigma = Sigma, proportions = proportions)
#> [1] 0.5705027

# sample
rmixnorm(n = 3, mean = mean, Sigma = Sigma, proportions = proportions)
#>            [,1]       [,2]
#> [1,]  0.3211793 -0.8718302
#> [2,] -2.5205032 -1.1316242
#> [3,] -1.3094168 -0.4523907
```
