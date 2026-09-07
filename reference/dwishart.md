# Wishart distribution

The function `dwishart()` computes the density of a Wishart
distribution.

The function `rwishart()` samples from a Wishart distribution.

The functions with suffix `_cpp` perform no input checks, hence are
faster.

## Usage

``` r
dwishart_cpp(x, df, scale, log = FALSE, inv = FALSE)

rwishart_cpp(df, scale, inv = FALSE)

dwishart(x, df, scale, log = FALSE, inv = FALSE)

rwishart(n = 1, df, scale, inv = FALSE)
```

## Arguments

- x:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  A covariance matrix of dimension `p`.

- df:

  \[`numeric(1)`\]  
  The degrees of freedom, at least `p`.

- scale:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  The scale covariance matrix of dimension `p`.

- log:

  \[`logical(1)`\]  
  Return the logarithm of the density value?

- inv:

  \[`logical(1)`\]  
  Use this inverse Wishart distribution?

- n:

  \[`integer(1)`\]  
  The number of requested samples.

## Value

For `dwishart()`: The density value.

For `rwishart()`: If `n = 1` a `matrix` of dimension `p` times `p`, else
an `array` of dimension `p` times `p` times `n` with the draws as
slices.

## See also

Other simulation helpers:
[`Simulator`](http://loelschlaeger.de/oeli/reference/Simulator.md),
[`correlated_regressors()`](http://loelschlaeger.de/oeli/reference/correlated_regressors.md),
[`ddirichlet_cpp()`](http://loelschlaeger.de/oeli/reference/ddirichlet.md),
[`dmixnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmixnorm.md),
[`dmvnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md),
[`dtnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dtnorm.md),
[`gaussian_tv()`](http://loelschlaeger.de/oeli/reference/gaussian_tv.md),
[`simulate_markov_chain()`](http://loelschlaeger.de/oeli/reference/simulate_markov_chain.md)

## Examples

``` r
x <- diag(2)
df <- 6
scale <- matrix(c(1, -0.3, -0.3, 0.8), ncol = 2)

# compute density
dwishart(x = x, df = df, scale = scale)
#> [1] 0.002607893
dwishart(x = x, df = df, scale = scale, log = TRUE)
#> [1] -5.949213
dwishart(x = x, df = df, scale = scale, inv = TRUE)
#> [1] 0.0004824907

# sample
rwishart(df = df, scale = scale)
#>           [,1]      [,2]
#> [1,] 10.916213 -3.044349
#> [2,] -3.044349  6.055640
rwishart(df = df, scale = scale, inv = TRUE)
#>            [,1]       [,2]
#> [1,]  0.6896157 -0.1497311
#> [2,] -0.1497311  0.1339354

# expectation of Wishart is df * scale
apply(rwishart(n = 100, df = df, scale = scale), 1:2, mean)
#>           [,1]      [,2]
#> [1,]  5.450034 -1.778992
#> [2,] -1.778992  4.881211
df * scale
#>      [,1] [,2]
#> [1,]  6.0 -1.8
#> [2,] -1.8  4.8

# expectation of inverse Wishart is scale / (df - p - 1)
apply(rwishart(n = 100, df = df, scale = scale, inv = TRUE), 1:2, mean)
#>             [,1]        [,2]
#> [1,]  0.39347712 -0.09653709
#> [2,] -0.09653709  0.25599598
scale / (df - 2 - 1)
#>            [,1]       [,2]
#> [1,]  0.3333333 -0.1000000
#> [2,] -0.1000000  0.2666667
```
