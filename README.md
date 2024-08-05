
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

You can install the released package version from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("oeli")
```

## Demos

The package includes helpers for different tasks and objects. Below are
some demos. Clicking the headings will take you to the corresponding
vignette, where you’ll find a documentation on all the helpers currently
available in that category.

### [`data.frame` helpers](https://loelschlaeger.de/oeli/articles/dataframe_helpers.html)

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

### [`function` helpers](https://loelschlaeger.de/oeli/articles/function_helpers.html)

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

### [`list` helpers](https://loelschlaeger.de/oeli/articles/list_helpers.html)

The following merges two or more `list` objects by unique element names:

``` r
merge_lists(list("a" = 1, "b" = 2), list("b" = 3, "c" = 4, "d" = NULL))
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 4
#> 
#> $d
#> NULL
```

### [`matrix` helpers](https://loelschlaeger.de/oeli/articles/matrix_helpers.html)

Checking if a `matrix` is a proper transition probability matrix:

``` r
matrix <- diag(4)
matrix[1, 2] <- 1
check_transition_probability_matrix(matrix)
#> [1] "Must have row sums equal to 1"
```

### [package helpers](https://loelschlaeger.de/oeli/articles/package_helpers.html)

Quickly having a basic package logo:

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
#> [1,]    0.09 -0.43   0.2 ...   -0.6
#> [2,]    -0.3  0.44  0.88 ...   0.14
#> [3,]   -0.04 -2.39  1.69 ...    2.1
#> ...      ...   ...   ... ...    ...
#> [100,]  0.83  0.32 -0.05 ...   1.77
```

### [simulation helpers](https://loelschlaeger.de/oeli/articles/simulation_helpers.html)

Simulate a Markov chain:

``` r
Gamma <- sample_transition_probability_matrix(dim = 3)
simulate_markov_chain(Gamma = Gamma, T = 20)
#>  [1] 3 3 2 2 3 3 2 3 1 1 1 1 1 1 1 1 1 1 1 1
```

### [`vector` helpers](https://loelschlaeger.de/oeli/articles/vector_helpers.html)

Create all possible permutations:

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
