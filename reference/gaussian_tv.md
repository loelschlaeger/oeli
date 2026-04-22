# Gaussian total variation

Computes the total variation (TV) between two multivariate Gaussian
distributions \\f_1,f_2\\: \$\$\mathrm{TV}(f_1, f_2) = \tfrac{1}{2}
\int\_{\mathbb{R}^p} \lvert f_1(x) - f_2(x) \rvert \\ dx.\$\$ The value
ranges from 0 (identical distributions) to 1 (no overlap).

## Usage

``` r
gaussian_tv(
  mean1,
  mean2,
  Sigma1,
  Sigma2,
  method = c("auto", "mc", "cubature"),
  n = 10000,
  tol_equal = 1e-06,
  eps = 1e-06
)
```

## Arguments

- mean1, mean2:

  \[`numeric(p)`\]  
  The mean vectors.

- Sigma1, Sigma2:

  \[`matrix(nrow = p, ncol = p)`\]  
  The covariance matrices.

- method:

  \[`character(1)`\]  
  Computation method. One of:

  - `"auto"`: use closed-form formula when covariances are equal,
    otherwise use `"cubature"` for \\p \le 2\\ and `"mc"` for higher
    dimensions.

  - `"mc"`: estimate via Monte Carlo importance sampling from the
    mixture \\0.5 (f_1 + f_2)\\.

  - `"cubature"`: compute overlap via adaptive cubature integration over
    a bounding box, then convert to TV. Exact but slow for \\p \ge 2\\.

- n:

  \[`integer(1)`\]  
  Number of Monte Carlo samples to draw.

- tol_equal:

  \[`numeric(1)`\]  
  Numerical tolerance used to decide whether `Sigma1` and `Sigma2` are
  considered equal (enabling the closed-form formula in `"auto"` mode).

- eps:

  \[`numeric(1)`\]  
  Only used when `method = "cubature"`. Specifies the total probability
  mass allowed to lie outside the integration hyper-rectangle across all
  dimensions. This determines the numerical integration bounds: the
  function chooses limits so that the probability of a point from either
  Gaussian falling outside the box is at most `eps`. The bound is split
  evenly across dimensions via a union bound, so the per-dimension tail
  probability is approximately `eps / p`. Smaller values produce wider
  bounds (slower but more accurate integration), while larger values
  yield narrower bounds (faster but potentially less accurate).

## Value

The total variation in \[0, 1\].

## See also

Other simulation helpers:
[`Simulator`](http://loelschlaeger.de/oeli/reference/Simulator.md),
[`correlated_regressors()`](http://loelschlaeger.de/oeli/reference/correlated_regressors.md),
[`ddirichlet_cpp()`](http://loelschlaeger.de/oeli/reference/ddirichlet.md),
[`dmixnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmixnorm.md),
[`dmvnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md),
[`dtnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dtnorm.md),
[`dwishart_cpp()`](http://loelschlaeger.de/oeli/reference/dwishart.md),
[`simulate_markov_chain()`](http://loelschlaeger.de/oeli/reference/simulate_markov_chain.md)

## Examples

``` r
### univariate case
mean1 <- 0
mean2 <- 1
Sigma1 <- Sigma2 <- matrix(1)
gaussian_tv(mean1, mean2, Sigma1, Sigma2)
#> [1] 0.3829249

### bivariate case
mean1 <- c(0, 0)
mean2 <- c(1, 1)
Sigma1 <- matrix(c(1, 0.2, 0.2, 1), ncol = 2)
Sigma2 <- matrix(c(1.5, -0.3, -0.3, 1), ncol = 2)
gaussian_tv(mean1, mean2, Sigma1, Sigma2, method = "mc", n = 1e3)
#> [1] 0.5346443
#> attr(,"se")
#> [1] 0.009136482
```
