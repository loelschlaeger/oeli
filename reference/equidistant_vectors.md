# Generate equidistant vectors in Euclidean space

This function constructs the coordinates of vertices of a regular
simplex in \\\mathbb{R}^{\code{dim}}\\ and returns the first `n` of
them,

- scaled so that the pairwise Euclidean distance between any two
  vertices equals `dist`,

- and centered so their centroid is at `center`.

## Usage

``` r
equidistant_vectors(dim, n = dim + 1, dist = 1, center = rep(0, dim))
```

## Arguments

- dim:

  \[`integer(1)`\]  
  The dimension.

- n:

  \[`integer(1)`\]  
  The number of vertices to return. Cannot be larger than `dim + 1`.

- dist:

  \[`numeric(1)`\]  
  Desired pairwise Euclidean distance between any two vertices.

- center:

  \[`numeric(dim)`\]  
  Desired center.

## Value

A matrix, where each column is a vertex of the simplex.

## See also

Other vector helpers:
[`check_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md),
[`check_probability_vector()`](http://loelschlaeger.de/oeli/reference/check_probability_vector.md),
[`chunk_vector()`](http://loelschlaeger.de/oeli/reference/chunk_vector.md),
[`insert_vector_entry()`](http://loelschlaeger.de/oeli/reference/insert_vector_entry.md),
[`map_indices()`](http://loelschlaeger.de/oeli/reference/map_indices.md),
[`match_numerics()`](http://loelschlaeger.de/oeli/reference/match_numerics.md),
[`permutations()`](http://loelschlaeger.de/oeli/reference/permutations.md),
[`split_vector_at()`](http://loelschlaeger.de/oeli/reference/split_vector_at.md),
[`subsets()`](http://loelschlaeger.de/oeli/reference/subsets.md),
[`vector_occurrence()`](http://loelschlaeger.de/oeli/reference/vector_occurrence.md)

## Examples

``` r
dim <- n <- 3
(dist <- runif(1))
#> [1] 0.9856864
(center <- rnorm(dim))
#> [1] 0.3624964 0.2253315 0.4828696
(V <- equidistant_vectors(dim = dim, n = n, dist = dist, center = center))
#>           [,1]       [,2]        [,3]
#> [1,] 0.8990361 0.09422654  0.09422654
#> [2,] 0.1304838 0.69957012 -0.15405934
#> [3,] 0.3185885 0.31858850  0.81143170
rowMeans(V)
#> [1] 0.3624964 0.2253315 0.4828696
dist(t(V))
#>           1         2
#> 2 0.9856864          
#> 3 0.9856864 0.9856864
```
