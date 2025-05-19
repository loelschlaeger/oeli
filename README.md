
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
#> [1] 0.2832878 0.5086812 0.2080309
```

For faster computation, [Rcpp](https://www.rcpp.org) implementations are
also available:

``` r
microbenchmark::microbenchmark(
  "R"    = rmvnorm(mean = c(0, 0, 0), Sigma = diag(3)),
  "Rcpp" = rmvnorm_cpp(mean = c(0, 0, 0), Sigma = diag(3))
)
#> Unit: microseconds
#>  expr   min     lq    mean median     uq    max neval
#>     R 275.3 295.65 345.847  310.1 332.45 2386.5   100
#>  Rcpp   2.7   3.20   6.245    4.8   5.40  164.2   100
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
#> [1,]    0.79 -0.43 -0.87 ...  -2.12
#> [2,]   -1.11  1.98 -2.42 ...   1.65
#> [3,]    1.66  1.76  0.25 ...  -2.97
#> ...      ...   ...   ... ...    ...
#> [100,]  0.44  0.28  0.53 ...  -1.75
```

And what about a `data.frame`?

``` r
x <- data.frame(x = rnorm(1000), y = LETTERS[1:10])
print_data.frame(x, rows = 7, digits = 0)
#>      x  y
#> 1     0 A
#> 2    -1 B
#> 3     0 C
#> 4     0 D
#> <993 rows hidden>
#>          
#> 998   0 H
#> 999   0 I
#> 1000  0 J
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
#>   P C        N1          N2         U
#> 1 1 2 -3.619643  1.24813328 -1.782100
#> 2 1 3 -4.117207  0.19133009 -1.585383
#> 3 2 1  2.146791 -0.08796485 -1.290140
#> 4 2 3 -3.501855  0.60817726 -1.688658
#> 5 1 3  2.707852 -2.17507050 -1.912338
#> 6 2 1 -2.222701  2.28324260 -1.646795
cor(data)
#>              P          C          N1          N2           U
#> P   1.00000000 -0.3164384 -0.08426915 -0.03743832  0.54776279
#> C  -0.31643843  1.0000000  0.19326415 -0.50596805 -0.75090001
#> N1 -0.08426915  0.1932641  1.00000000 -0.30000000 -0.26643345
#> N2 -0.03743832 -0.5059680 -0.30000000  1.00000000  0.09397231
#> U   0.54776279 -0.7509000 -0.26643345  0.09397231  1.00000000
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
