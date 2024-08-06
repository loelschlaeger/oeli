
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Utilities for developing R software <a href="https://loelschlaeger.de/oeli/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/oeli)](https://CRAN.R-project.org/package=oeli)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-month/oeli)](https://CRAN.R-project.org/package=oeli)
[![R-CMD-check](https://github.com/loelschlaeger/oeli/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/loelschlaeger/oeli/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/oeli/branch/master/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/oeli?branch=master)
<!-- badges: end -->

The `{oeli}` package offers a collection of handy functions that I found
useful while developing R packages. Perhaps you’ll find them helpful
too!

## Installation

The released package version can be installed from
[CRAN](https://CRAN.R-project.org) via:

``` r
install.packages("oeli")
```

## Demos

The package includes helpers for various tasks and objects. Some demos
are shown below. Click the headings for reference pages with
documentation on all available helpers in each category.

### [Distributions](https://loelschlaeger.de/oeli/reference/index.html#distribution)

The package has density and sampling functions for distributions not in
base R, such as Dirichlet, multivariate normal, truncated normal, and
Wishart. For faster computation, an Rcpp implementation is also
available.

``` r
ddirichlet(x = c(0.2, 0.3, 0.5), concentration = 1:3)
#> [1] 4.5
```

``` r
rdirichlet(concentration = 1:3)
#> [1] 0.2032900 0.2310928 0.5656171
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
package_logo("my_package", brackets = TRUE, use_logo = FALSE)
```

<img src="man/figures/README-package_logo-1.png" width="50%" style="display: block; margin: auto;" />

How to print a matrix without filling up the entire console?

``` r
x <- matrix(rnorm(10000), ncol = 100, nrow = 100)
print_matrix(x, rowdots = 4, coldots = 4, digits = 2, label = "what a big matrix")
#> what a big matrix : 100 x 100 matrix of doubles 
#>         [,1]  [,2]  [,3] ... [,100]
#> [1,]    0.01  0.59 -1.02 ...   1.73
#> [2,]   -0.11 -0.01  2.37 ...   0.22
#> [3,]   -1.87  0.09 -1.24 ...  -0.93
#> ...      ...   ...   ... ...    ...
#> [100,]   0.6 -0.99 -0.26 ...   -0.9
```

### [Simulation helpers](https://loelschlaeger.de/oeli/reference/index.html#simulation)

Let’s simulate a Markov chain:

``` r
Gamma <- sample_transition_probability_matrix(dim = 3)
simulate_markov_chain(Gamma = Gamma, T = 20)
#>  [1] 2 2 3 2 1 2 2 2 2 1 2 2 1 1 2 3 3 1 1 1
```

### [Transformation helpers](https://loelschlaeger.de/oeli/reference/index.html#transformation)

The `group_data_frame()` function groups a given `data.frame` based on
the values in a specified column:

``` r
df <- data.frame("label" = c("A", "B"), "number" = 1:10)
group_data_frame(df = df, by = "label")
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
