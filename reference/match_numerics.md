# Best-possible match of two numeric vectors

This function matches the indices of two numeric vectors as good as
possible (that means with the smallest possible sum of deviations).

## Usage

``` r
match_numerics(x, y)
```

## Arguments

- x, y:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Two vectors of the same length.

## Value

An `integer` vector of length `length(x)` with the positions of `y` in
`x`.

## See also

Other vector helpers:
[`check_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md),
[`check_probability_vector()`](http://loelschlaeger.de/oeli/reference/check_probability_vector.md),
[`chunk_vector()`](http://loelschlaeger.de/oeli/reference/chunk_vector.md),
[`equidistant_vectors()`](http://loelschlaeger.de/oeli/reference/equidistant_vectors.md),
[`insert_vector_entry()`](http://loelschlaeger.de/oeli/reference/insert_vector_entry.md),
[`map_indices()`](http://loelschlaeger.de/oeli/reference/map_indices.md),
[`permutations()`](http://loelschlaeger.de/oeli/reference/permutations.md),
[`split_vector_at()`](http://loelschlaeger.de/oeli/reference/split_vector_at.md),
[`subsets()`](http://loelschlaeger.de/oeli/reference/subsets.md),
[`vector_occurrence()`](http://loelschlaeger.de/oeli/reference/vector_occurrence.md)

## Examples

``` r
x <- c(-1, 0, 1)
y <- c(0.1, 1.5, -1.2)
match_numerics(x, y)
#> [1] 2 3 1
```
