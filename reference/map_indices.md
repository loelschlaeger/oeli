# Map indices

This function maps indices from an input vector to corresponding
sequences of grouped indices. Each element from the input specifies a
group to be mapped from the sequence, determined by the grouping size
`n`.

## Usage

``` r
map_indices(indices, n)
```

## Arguments

- indices:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  An index vector, where each element specifies a group to be mapped
  from the sequence.

- n:

  \[`integer`\]  
  The size of each group of consecutive indices.

## Value

An `integer` `vector`, containing the mapped indices according to the
specified group size.

## Details

This function is useful when working with indices arranged in fixed-size
groups, where each group can be referenced by a single index. For
example, if indices are structured in chunks of 3, calling this function
with `n = 3` will map the corresponding groups of 3 consecutive indices
for the given input indices, see the examples.

## See also

Other vector helpers:
[`check_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md),
[`check_probability_vector()`](http://loelschlaeger.de/oeli/reference/check_probability_vector.md),
[`chunk_vector()`](http://loelschlaeger.de/oeli/reference/chunk_vector.md),
[`equidistant_vectors()`](http://loelschlaeger.de/oeli/reference/equidistant_vectors.md),
[`insert_vector_entry()`](http://loelschlaeger.de/oeli/reference/insert_vector_entry.md),
[`match_numerics()`](http://loelschlaeger.de/oeli/reference/match_numerics.md),
[`permutations()`](http://loelschlaeger.de/oeli/reference/permutations.md),
[`split_vector_at()`](http://loelschlaeger.de/oeli/reference/split_vector_at.md),
[`subsets()`](http://loelschlaeger.de/oeli/reference/subsets.md),
[`vector_occurrence()`](http://loelschlaeger.de/oeli/reference/vector_occurrence.md)

## Examples

``` r
# Example: Map indices based on groups of 3
map_indices(c(1, 3, 5), 3)
#> [1]  1  2  3  7  8  9 13 14 15
```
