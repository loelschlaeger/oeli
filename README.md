
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Utilities for developing R code <a href="https://loelschlaeger.de/oeli/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/oeli)](https://CRAN.R-project.org/package=oeli)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-month/oeli)](https://CRAN.R-project.org/package=oeli)
[![R-CMD-check](https://github.com/loelschlaeger/oeli/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/loelschlaeger/oeli/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/oeli/branch/master/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/oeli?branch=master)
<!-- badges: end -->

This [R](https://CRAN.R-project.org) package provides helper functions I
found useful when developing R code - perhaps you will too! The released
package version can be installed via:

``` r
install.packages("oeli")
```

The following shows some demos. Click the headings for references on all
available helpers in each category.

### [Distributions](https://loelschlaeger.de/oeli/reference/index.html#distribution)

The package has density and sampling functions for some distributions
not included in base R, like the Dirichlet:

``` r
ddirichlet(x = c(0.2, 0.3, 0.5), concentration = 1:3)
#> [1] 4.5
rdirichlet(concentration = 1:3)
#> [1] 0.2219166 0.2998792 0.4782041
```

For faster computation, [Rcpp](https://www.rcpp.org) implementations are
also available:

``` r
microbenchmark::microbenchmark(
  "R"    = rmvnorm(mean = c(0, 0, 0), Sigma = diag(3)),
  "Rcpp" = rmvnorm_cpp(mean = c(0, 0, 0), Sigma = diag(3))
)
#> Unit: microseconds
#>  expr     min       lq     mean   median      uq      max neval
#>     R 254.001 271.4505 347.1850 282.7515 316.901 2248.201   100
#>  Rcpp   2.701   3.2005   4.6491   4.4010   5.151   14.802   100
```

### [Function helpers](https://loelschlaeger.de/oeli/reference/index.html#functional)

Retrieving default arguments of a `function`:

``` r
f <- function(a, b = 1, c = "", ...) { }
function_defaults(f)
#> $b
#> [1] 1
#> 
#> $c
#> [1] ""
```

### [Indexing helpers](https://loelschlaeger.de/oeli/reference/index.html#indexing)

Create all possible permutations of vector elements:

``` r
permutations(LETTERS[1:3])
#> [[1]]
#> [1] "A" "B" "C"
#> 
#> [[2]]
#> [1] "A" "C" "B"
#> 
#> [[3]]
#> [1] "B" "A" "C"
#> 
#> [[4]]
#> [1] "B" "C" "A"
#> 
#> [[5]]
#> [1] "C" "A" "B"
#> 
#> [[6]]
#> [1] "C" "B" "A"
```

### [Package helpers](https://loelschlaeger.de/oeli/reference/index.html#packaging)

Quickly have a basic logo for your new package:

``` r
logo <- package_logo("my_package", brackets = TRUE)
print(logo)
```

<img src="man/figures/README-package_logo-1.png" width="50%" style="display: block; margin: auto;" />

How to print a `matrix` without filling up the entire console?

``` r
x <- matrix(rnorm(10000), ncol = 100, nrow = 100)
print_matrix(x, rowdots = 4, coldots = 4, digits = 2, label = "what a big matrix")
#> what a big matrix : 100 x 100 matrix of doubles 
#>         [,1]  [,2]  [,3] ... [,100]
#> [1,]   -1.65 -0.68 -1.17 ...  -1.12
#> [2,]    0.25  0.07 -0.72 ...  -1.63
#> [3,]   -0.94  0.24  2.24 ...   0.03
#> ...      ...   ...   ... ...    ...
#> [100,] -0.78  0.97   2.1 ...   1.38
```

And what about a `data.frame`?

``` r
x <- data.frame(x = rnorm(1000), y = LETTERS[1:10])
print_data.frame(x, rows = 7, digits = 0)
#>      x  y
#> 1     0 A
#> 2     1 B
#> 3    -1 C
#> 4    -2 D
#> <993 rows hidden>
#>          
#> 998   1 H
#> 999   1 I
#> 1000  1 J
```

### [Simulation helpers](https://loelschlaeger.de/oeli/reference/index.html#simulation)

Let’s simulate correlated regressor values from different marginal
distributions:

``` r
labels <- c("P", "C", "N1", "N2", "U")
n <- 100
marginals <- list(
  "P" = list(type = "poisson", lambda = 2),
  "C" = list(type = "categorical", p = c(0.3, 0.2, 0.5)),
  "N1" = list(type = "normal", mean = -1, sd = 2),
  "U" = list(type = "uniform", min = -2, max = -1)
)
correlation <- matrix(
  c(1, -0.3, -0.1, 0, 0.5,
    -0.3, 1, 0.3, -0.5, -0.7,
    -0.1, 0.3, 1, -0.3, -0.3,
    0, -0.5, -0.3, 1, 0.1,
    0.5, -0.7, -0.3, 0.1, 1),
  nrow = 5, ncol = 5
)
data <- correlated_regressors(
  labels = labels, n = n, marginals = marginals, correlation = correlation
)
head(data)
#>   P C         N1          N2         U
#> 1 4 1 -3.3191431  0.31658348 -1.049335
#> 2 1 3 -2.6607111 -0.82624230 -1.577239
#> 3 2 3 -1.3887395 -2.76968068 -1.542392
#> 4 2 2 -0.7360741 -0.09171994 -1.305831
#> 5 2 3  3.6731239 -0.67047586 -1.866220
#> 6 4 2  0.7742342  1.36074573 -1.631845
cor(data)
#>              P          C         N1          N2          U
#> P   1.00000000 -0.2742909 -0.1021209  0.04795651  0.4767508
#> C  -0.27429087  1.0000000  0.2713539 -0.50344066 -0.7408584
#> N1 -0.10212093  0.2713539  1.0000000 -0.30000000 -0.3098568
#> N2  0.04795651 -0.5034407 -0.3000000  1.00000000  0.0715843
#> U   0.47675075 -0.7408584 -0.3098568  0.07158430  1.0000000
```

### [Transformation helpers](https://loelschlaeger.de/oeli/reference/index.html#transformation)

The `group_data.frame()` function groups a given `data.frame` based on
the values in a specified column:

``` r
df <- data.frame("label" = c("A", "B"), "number" = 1:10)
group_data.frame(df = df, by = "label")
#> $A
#>   label number
#> 1     A      1
#> 3     A      3
#> 5     A      5
#> 7     A      7
#> 9     A      9
#> 
#> $B
#>    label number
#> 2      B      2
#> 4      B      4
#> 6      B      6
#> 8      B      8
#> 10     B     10
```

### [Validation helpers](https://loelschlaeger.de/oeli/reference/index.html#validation)

Is my matrix a proper transition probability matrix?

``` r
matrix <- diag(4)
matrix[1, 2] <- 1
check_transition_probability_matrix(matrix)
#> [1] "Must have row sums equal to 1"
```
