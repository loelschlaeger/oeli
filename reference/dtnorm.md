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

rtnorm(n = 1, mean, sd, point, above, log = FALSE)

rttnorm(n = 1, mean, sd, lower, upper, log = FALSE)
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
  For `dtnorm()` and `dttnorm()`, return the logarithm of the density
  value?

  For `rtnorm()` and `rttnorm()`, return the exponential of the draw,
  which is a draw from the truncated log-normal distribution?

- n:

  \[`integer(1)`\]  
  The number of requested samples.

## Value

For `dtnorm()` and `dttnorm()`: The density value.

For `rtnorm()` and `rttnorm()`: A `numeric` of length `n` with the
random draws.

## Details

`rtnorm()` draws by the rejection methods of Robert (1995), and
`rttnorm()` inverts the distribution function of the truncated tail, so
that both remain accurate when a truncation point lies far in the tail.

## References

Robert, C. P. (1995). Simulation of truncated normal variables.
Statistics and Computing, 5(2), 121-125.

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
# compute density
dtnorm(x = 1, mean = 0, sd = 1, point = 0, above = FALSE)
#> [1] 0.4839414
dttnorm(x = 0, mean = 0, sd = 1, lower = -1, upper = 1, log = TRUE)
#> [1] -0.5372234

# sample
rtnorm(n = 3, mean = 0, sd = 1, point = 0, above = FALSE)
#> [1] 1.152508 1.376613 2.007539
rttnorm(mean = 0, sd = 1, lower = -1, upper = 1)
#> [1] 0.470705
```
