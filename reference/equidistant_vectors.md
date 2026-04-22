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
#> [1] 0.8043711
(center <- rnorm(dim))
#> [1] 0.6882699 0.9908185 0.4487030
(V <- equidistant_vectors(dim = dim, n = n, dist = dist, center = center))
#>           [,1]      [,2]      [,3]
#> [1,] 1.1261140 0.4693478 0.4693478
#> [2,] 0.9134179 1.3778217 0.6812160
#> [3,] 0.3146411 0.3146411 0.7168266
rowMeans(V)
#> [1] 0.6882699 0.9908185 0.4487030
dist(t(V))
#>           1         2
#> 2 0.8043711          
#> 3 0.8043711 0.8043711
```
