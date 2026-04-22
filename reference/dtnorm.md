# Truncated normal distribution

The function `dtnorm()` computes the density of a truncated normal
distribution.

The function `rtnorm()` samples from a truncated normal distribution.

The function `dttnorm()` and `rttnorm()` compute the density and sample
from a two-sided truncated normal distribution, respectively.

The functions with suffix `_cpp` perform no input checks, hence are
faster.

## Usage

``` r
dtnorm_cpp(x, mean, sd, point, above, log = FALSE)

dttnorm_cpp(x, mean, sd, lower, upper, log = FALSE)

rtnorm_cpp(mean, sd, point, above, log = FALSE)

rttnorm_cpp(mean, sd, lower, upper, log = FALSE)

dtnorm(x, mean, sd, point, above, log = FALSE)

dttnorm(x, mean, sd, lower, upper, log = FALSE)

rtnorm(mean, sd, point, above, log = FALSE)

rttnorm(mean, sd, lower, upper, log = FALSE)
```

## Arguments

- x:

  \[`numeric(1)`\]  
  A quantile.

- mean:

  \[`numeric(1)`\]  
  The mean.

- sd:

  \[`numeric(1)`\]  
  The non-negative standard deviation.

- point, lower, upper:

  \[`numeric(1)`\]  
  The truncation point.

- above:

  \[`logical(1)`\]  
  Truncate from above? Else, from below.

- log:

  \[`logical(1)`\]  
  Return the logarithm of the density value?

## Value

For `dtnorm()` and `dttnorm()`: The density value.

For `rtnorm()` and `rttnorm()`: The random draw

## See also

Other simulation helpers:
[`Simulator`](http://loelschlaeger.de/oeli/reference/Simulator.md),
[`correlated_regressors()`](http://loelschlaeger.de/oeli/reference/correlated_regressors.md),
[`ddirichlet_cpp()`](http://loelschlaeger.de/oeli/reference/ddirichlet.md),
[`dmixnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmixnorm.md),
[`dmvnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md),
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

# sample
rmvnorm(n = 3, mean = mean, Sigma = Sigma)
#>            [,1]       [,2]
#> [1,] -2.1739333  1.5957503
#> [2,]  1.4067293 -0.3436295
#> [3,] -0.2064392  0.5327578
rmvnorm(mean = mean, Sigma = Sigma, log = TRUE)
#> [1] 1.530003 4.872182
```
