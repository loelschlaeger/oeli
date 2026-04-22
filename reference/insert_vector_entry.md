# Insert entry in vector

This function inserts a value into a vector.

## Usage

``` r
insert_vector_entry(v, x, p)
```

## Arguments

- v:

  \[`atomic()`\]  
  A `vector`.

- x:

  \[`atomic(1)`\]  
  The entry to be added.

- p:

  \[`integer())`\]  
  The position(s) where to add the value, one or more of:

  - `p = 0` appends the value left

  - `p = length(v)` appends the value right

  - `p = n` inserts the value between the `n`-th and `(n + 1)`-th entry
    of `v`.

## Value

A `vector`.

## See also

Other vector helpers:
[`check_numeric_vector()`](http://loelschlaeger.de/oeli/reference/check_numeric_vector.md),
[`check_probability_vector()`](http://loelschlaeger.de/oeli/reference/check_probability_vector.md),
[`chunk_vector()`](http://loelschlaeger.de/oeli/reference/chunk_vector.md),
[`equidistant_vectors()`](http://loelschlaeger.de/oeli/reference/equidistant_vectors.md),
[`map_indices()`](http://loelschlaeger.de/oeli/reference/map_indices.md),
[`match_numerics()`](http://loelschlaeger.de/oeli/reference/match_numerics.md),
[`permutations()`](http://loelschlaeger.de/oeli/reference/permutations.md),
[`split_vector_at()`](http://loelschlaeger.de/oeli/reference/split_vector_at.md),
[`subsets()`](http://loelschlaeger.de/oeli/reference/subsets.md),
[`vector_occurrence()`](http://loelschlaeger.de/oeli/reference/vector_occurrence.md)

## Examples

``` r
v <- 1:3
x <- 0
insert_vector_entry(v, x, 0)
#> [1] 0 1 2 3
insert_vector_entry(v, x, 1)
#> [1] 1 0 2 3
insert_vector_entry(v, x, 2)
#> [1] 1 2 0 3
insert_vector_entry(v, x, 3)
#> [1] 1 2 3 0

### also multiple positions
insert_vector_entry(v, x, 0:3)
#> [1] 0 1 0 2 0 3 0

### also trivial case
insert_vector_entry(integer(), integer(), integer())
#> integer(0)
```
