# Dirichlet distribution

The function `ddirichlet()` computes the density of a Dirichlet
distribution.

The function `rdirichlet()` samples from a Dirichlet distribution.

The functions with suffix `_cpp` perform no input checks, hence are
faster.

## Usage

``` r
ddirichlet_cpp(x, concentration, log = FALSE)

rdirichlet_cpp(concentration)

ddirichlet(x, concentration, log = FALSE)

rdirichlet(n = 1, concentration)
```

## Arguments

- x:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A probability vector.

- concentration:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A concentration vector of the same length as `x`.

- log:

  \[`logical(1)`\]  
  Return the logarithm of the density value?

- n:

  \[`integer(1)`\]  
  The number of samples.

## Value

For `ddirichlet()`: The density value.

For `rdirichlet()`: If `n = 1` a `vector` of length `p`, else a `matrix`
of dimension `n` times `p` with samples as rows.

## See also

Other simulation helpers:
[`Simulator`](http://loelschlaeger.de/oeli/reference/Simulator.md),
[`correlated_regressors()`](http://loelschlaeger.de/oeli/reference/correlated_regressors.md),
[`dmixnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmixnorm.md),
[`dmvnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md),
[`dtnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dtnorm.md),
[`dwishart_cpp()`](http://loelschlaeger.de/oeli/reference/dwishart.md),
[`gaussian_tv()`](http://loelschlaeger.de/oeli/reference/gaussian_tv.md),
[`simulate_markov_chain()`](http://loelschlaeger.de/oeli/reference/simulate_markov_chain.md)

## Examples

``` r
x <- c(0.5, 0.3, 0.2)
concentration <- 1:3

# compute density
ddirichlet(x = x, concentration = concentration)
#> [1] 0.72
ddirichlet(x = x, concentration = concentration, log = TRUE)
#> [1] -0.3285041

# sample
rdirichlet(concentration = 1:3)
#> [1] 0.01265446 0.76488204 0.22246350
rdirichlet(n = 4, concentration = 1:2)
#>           [,1]      [,2]
#> [1,] 0.2754070 0.7245930
#> [2,] 0.1520073 0.8479927
#> [3,] 0.4372869 0.5627131
#> [4,] 0.1567506 0.8432494
```
