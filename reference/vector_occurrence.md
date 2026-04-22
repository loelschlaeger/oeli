# Find the positions of first or last occurrence of unique vector elements

This function finds the positions of first or last occurrence of unique
vector elements.

## Usage

``` r
vector_occurrence(x, type = "first")
```

## Arguments

- x:

  \[`atomic()`\]  
  A `vector`.

- type:

  \[`character(1)`\]  
  Either `"first"` for the first or `"last"` for the last occurrence.

## Value

An `integer` `vector`, the positions of the unique vector elements. The
ordering corresponds to `unique(x)`, i.e., the \\i\\-th element in the
output is the (first or last) occurrence of the \\i\\-th element from
`unique(x)`.

## See also

Other vector helpers:
[`check_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md),
[`check_probability_vector()`](http://loelschlaeger.de/oeli/reference/check_probability_vector.md),
[`chunk_vector()`](http://loelschlaeger.de/oeli/reference/chunk_vector.md),
[`equidistant_vectors()`](http://loelschlaeger.de/oeli/reference/equidistant_vectors.md),
[`insert_vector_entry()`](http://loelschlaeger.de/oeli/reference/insert_vector_entry.md),
[`map_indices()`](http://loelschlaeger.de/oeli/reference/map_indices.md),
[`match_numerics()`](http://loelschlaeger.de/oeli/reference/match_numerics.md),
[`permutations()`](http://loelschlaeger.de/oeli/reference/permutations.md),
[`split_vector_at()`](http://loelschlaeger.de/oeli/reference/split_vector_at.md),
[`subsets()`](http://loelschlaeger.de/oeli/reference/subsets.md)

## Examples

``` r
x <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
unique(x)
#> [1] 1 2 3
vector_occurrence(x, "first")
#> [1] 1 4 7
vector_occurrence(x, "last")
#> [1] 3 6 9
```
