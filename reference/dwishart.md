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

rwishart(df, scale, inv = FALSE)
```

## Arguments

- x:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  A covariance matrix of dimension `p`.

- df:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The degrees of freedom greater of equal `p`.

- scale:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  The scale covariance matrix of dimension `p`.

- log:

  \[`logical(1)`\]  
  Return the logarithm of the density value?

- inv:

  \[`logical(1)`\]  
  Use this inverse Wishart distribution?

## Value

For `dwishart()`: The density value.

For `rwishart()`: A `matrix`, the random draw.

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
#> [1,]  7.968892 -1.492046
#> [2,] -1.492046  3.997400
rwishart(df = df, scale = scale, inv = TRUE)
#>              [,1]         [,2]
#> [1,] 0.1070309704 0.0001531447
#> [2,] 0.0001531447 0.0658702943

# expectation of Wishart is df * scale
n <- 100
replicate(n, rwishart(df = df, scale = scale), simplify = FALSE) |>
  Reduce(f = "+") / n
#>           [,1]      [,2]
#> [1,]  6.175526 -1.499178
#> [2,] -1.499178  4.580922
df * scale
#>      [,1] [,2]
#> [1,]  6.0 -1.8
#> [2,] -1.8  4.8

# expectation of inverse Wishart is scale / (df - p - 1)
n <- 100
replicate(n, rwishart(df = df, scale = scale, TRUE), simplify = FALSE) |>
  Reduce(f = "+") / n
#>             [,1]        [,2]
#> [1,]  0.33451489 -0.08341875
#> [2,] -0.08341875  0.26996849
scale / (df - 2 - 1)
#>            [,1]       [,2]
#> [1,]  0.3333333 -0.1000000
#> [2,] -0.1000000  0.2666667
```
