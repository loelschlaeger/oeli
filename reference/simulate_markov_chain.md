# Simulate Markov chain

This function simulates a Markov chain.

## Usage

``` r
simulate_markov_chain(Gamma, T, delta = oeli::stationary_distribution(Gamma))
```

## Arguments

- Gamma:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  A transition probability matrix.

- T:

  \[`integer(1)`\]  
  The length of the Markov chain.

- delta:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A probability vector, the initial distribution.

  The stationary distribution is used by default.

## Value

A `numeric` vector of length `T` with states.

## See also

Other simulation helpers:
[`Simulator`](http://loelschlaeger.de/oeli/reference/Simulator.md),
[`correlated_regressors()`](http://loelschlaeger.de/oeli/reference/correlated_regressors.md),
[`ddirichlet_cpp()`](http://loelschlaeger.de/oeli/reference/ddirichlet.md),
[`dmixnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmixnorm.md),
[`dmvnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md),
[`dtnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dtnorm.md),
[`dwishart_cpp()`](http://loelschlaeger.de/oeli/reference/dwishart.md),
[`gaussian_tv()`](http://loelschlaeger.de/oeli/reference/gaussian_tv.md)

## Examples

``` r
Gamma <- matrix(c(0.8, 0.2, 0.3, 0.7), byrow = TRUE, nrow = 2)
delta <- c(0.6, 0.4)
simulate_markov_chain(Gamma = Gamma, T = 20, delta = delta)
#>  [1] 2 1 1 1 1 1 1 1 1 2 1 1 2 1 1 1 1 1 2 2
```
