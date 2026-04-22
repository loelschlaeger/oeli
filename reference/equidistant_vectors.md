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
#> [1] 0.8905559
(center <- rnorm(dim))
#> [1]  1.2421389 -1.0992053  0.7454228
(V <- equidistant_vectors(dim = dim, n = n, dist = dist, center = center))
#>            [,1]       [,2]       [,3]
#> [1,]  1.7268961  0.9997603  0.9997603
#> [2,] -1.1848991 -0.6707364 -1.4419805
#> [3,]  0.5969969  0.5969969  1.0422748
rowMeans(V)
#> [1]  1.2421389 -1.0992053  0.7454228
dist(t(V))
#>           1         2
#> 2 0.8905559          
#> 3 0.8905559 0.8905559
```
