# Storage R6 Object

Provides a simple indexing interface for list elements based on R6.
Basically, it allows to store items in a list and to regain them based
on identifiers defined by the user.

## Value

The output depends on the method:

- `$new()` returns a `Storage` object.

- `$add()`, `$remove()`, and `$print()` invisibly return the `Storage`
  object (to allow for method chaining)

- `$get()` returns the requested element(s)

- `$number()` returns an `integer`

- `$indices()` return an `integer` `vector`

## Setting identifiers

An identifier is a `character`, typically a binary property. Identifiers
can be negated by placing an exclamation mark (`"!"`) in front of them.
Identifiers that have been assigned to other elements previously do not
need to be specified again for new elements; instead, a default value
can be used. This default value can be defined either globally for all
cases (via the `$missing_identifier` field) or separately for each
specific case (via the method argument).

## User confirmation

If desired, the user can be asked for confirmation when adding,
extracting, or removing elements using identifiers. This behavior can be
set globally through the `$confirm` field or customized separately for
each specific case via the method argument.

## See also

Other package helpers:
[`Dictionary`](http://loelschlaeger.de/oeli/reference/Dictionary.md),
[`check_missing()`](http://loelschlaeger.de/oeli/reference/check_missing.md),
[`find_namespace_calls()`](http://loelschlaeger.de/oeli/reference/find_namespace_calls.md),
[`identical_structure()`](http://loelschlaeger.de/oeli/reference/identical_structure.md),
[`input_check_response()`](http://loelschlaeger.de/oeli/reference/input_check_response.md),
[`match_arg()`](http://loelschlaeger.de/oeli/reference/match_arg.md),
[`package_logo()`](http://loelschlaeger.de/oeli/reference/package_logo.md),
[`print_data.frame()`](http://loelschlaeger.de/oeli/reference/print_data.frame.md),
[`print_matrix()`](http://loelschlaeger.de/oeli/reference/print_matrix.md),
[`system_information()`](http://loelschlaeger.de/oeli/reference/system_information.md),
[`unexpected_error()`](http://loelschlaeger.de/oeli/reference/unexpected_error.md),
[`user_confirm()`](http://loelschlaeger.de/oeli/reference/user_confirm.md)

## Active bindings

- `identifier`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  The identifiers used.

- `confirm`:

  \[`logical(1)`\]  
  The default value for confirmations.

- `missing_identifier`:

  \[`logical(1)`\]  
  The default value for not specified identifiers.

- `hide_warnings`:

  \[`logical(1)`\]  
  Hide warnings (for example if unknown identifiers are selected)?

## Methods

### Public methods

- [`Storage$new()`](#method-Storage-new)

- [`Storage$add()`](#method-Storage-add)

- [`Storage$get()`](#method-Storage-get)

- [`Storage$remove()`](#method-Storage-remove)

- [`Storage$number()`](#method-Storage-number)

- [`Storage$indices()`](#method-Storage-indices)

- [`Storage$print()`](#method-Storage-print)

------------------------------------------------------------------------

### Method `new()`

Initializing a `Storage` object.

#### Usage

    Storage$new()

------------------------------------------------------------------------

### Method `add()`

Adding an element.

#### Usage

    Storage$add(
      x,
      identifier,
      confirm = interactive() & self$confirm,
      missing_identifier = self$missing_identifier
    )

#### Arguments

- `x`:

  \[`any`\]  
  An object to be saved.

- `identifier`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Pne or more identifiers (the identifier `"all"` is reserved to select
  all elements).

- `confirm`:

  \[`logical(1)`\]  
  Prompted for confirmation?

- `missing_identifier`:

  \[`logical(1)` \| NA\]  
  The value for not specified identifiers.

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Getting elements.

#### Usage

    Storage$get(
      identifier = character(),
      ids = integer(),
      logical = "and",
      confirm = interactive() & self$confirm,
      missing_identifier = self$missing_identifier,
      id_names = FALSE
    )

#### Arguments

- `identifier`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Pne or more identifiers (the identifier `"all"` is reserved to select
  all elements).

- `ids`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  One or more ids.

- `logical`:

  \[`character(1)`\]  
  In the case that multiple identifiers are selected, how should they be
  combined? Options are:

  - `"and"` (the default): the identifiers are combined with logical and
    (all identifiers must be `TRUE`)

  - `"or"`: the identifiers are combined with logical or (at least one
    identifier must be `TRUE`)

- `confirm`:

  \[`logical(1)`\]  
  Prompted for confirmation?

- `missing_identifier`:

  \[`logical(1)` \| NA\]  
  The value for not specified identifiers.

- `id_names`:

  \[`logical(1)`\]  
  Name the elements according to their ids?

------------------------------------------------------------------------

### Method [`remove()`](https://rdrr.io/r/base/rm.html)

removing elements

#### Usage

    Storage$remove(
      identifier = character(),
      ids = integer(),
      logical = "and",
      confirm = interactive() & self$confirm,
      missing_identifier = self$missing_identifier,
      shift_ids = TRUE
    )

#### Arguments

- `identifier`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Pne or more identifiers (the identifier `"all"` is reserved to select
  all elements).

- `ids`:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  One or more ids.

- `logical`:

  \[`character(1)`\]  
  In the case that multiple identifiers are selected, how should they be
  combined? Options are:

  - `"and"` (the default): the identifiers are combined with logical and
    (all identifiers must be `TRUE`)

  - `"or"`: the identifiers are combined with logical or (at least one
    identifier must be `TRUE`)

- `confirm`:

  \[`logical(1)`\]  
  Prompted for confirmation?

- `missing_identifier`:

  \[`logical(1)` \| NA\]  
  The value for not specified identifiers.

- `shift_ids`:

  \[`logical(1)`\]  
  Shift ids when in-between elements are removed?

------------------------------------------------------------------------

### Method `number()`

Computing the number of identified elements.

#### Usage

    Storage$number(
      identifier = "all",
      missing_identifier = self$missing_identifier,
      logical = "and",
      confirm = FALSE
    )

#### Arguments

- `identifier`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Pne or more identifiers (the identifier `"all"` is reserved to select
  all elements).

- `missing_identifier`:

  \[`logical(1)` \| NA\]  
  The value for not specified identifiers.

- `logical`:

  \[`character(1)`\]  
  In the case that multiple identifiers are selected, how should they be
  combined? Options are:

  - `"and"` (the default): the identifiers are combined with logical and
    (all identifiers must be `TRUE`)

  - `"or"`: the identifiers are combined with logical or (at least one
    identifier must be `TRUE`)

- `confirm`:

  \[`logical(1)`\]  
  Prompted for confirmation?

------------------------------------------------------------------------

### Method `indices()`

Returning indices based on defined identifiers.

#### Usage

    Storage$indices(
      identifier = "all",
      logical = "and",
      confirm = interactive() & self$confirm
    )

#### Arguments

- `identifier`:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Pne or more identifiers (the identifier `"all"` is reserved to select
  all elements).

- `logical`:

  \[`character(1)`\]  
  In the case that multiple identifiers are selected, how should they be
  combined? Options are:

  - `"and"` (the default): the identifiers are combined with logical and
    (all identifiers must be `TRUE`)

  - `"or"`: the identifiers are combined with logical or (at least one
    identifier must be `TRUE`)

- `confirm`:

  \[`logical(1)`\]  
  Prompted for confirmation?

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printing details of the saved elements.

#### Usage

    Storage$print(...)

#### Arguments

- `...`:

  Currently not used.

## Examples

``` r
### 1. Create a `Storage` object:
my_storage <- Storage$new()

# 2. Add elements along with identifiers:
my_storage$
  add(42, c("number", "rational"))$
  add(pi, c("number", "!rational"))$
  add("fear of black cats", c("text", "!rational"))$
  add("wearing a seat belt", c("text", "rational"))$
  add(mean, "function")

# 3. What elements are stored?
print(my_storage)
#> number of elements: 5 
#> identifiers used: number rational text function 

# 4. Extract elements based on identifiers:
my_storage$get("rational")
#> [[1]]
#> [1] 42
#> 
#> [[2]]
#> [1] "wearing a seat belt"
#> 
my_storage$get("!rational")
#> [[1]]
#> [1] 3.141593
#> 
#> [[2]]
#> [1] "fear of black cats"
#> 
my_storage$get(c("text", "!rational"))
#> [[1]]
#> [1] "fear of black cats"
#> 
my_storage$get("all") # get all elements
#> [[1]]
#> [1] 42
#> 
#> [[2]]
#> [1] 3.141593
#> 
#> [[3]]
#> [1] "fear of black cats"
#> 
#> [[4]]
#> [1] "wearing a seat belt"
#> 
#> [[5]]
#> function (x, ...) 
#> UseMethod("mean")
#> <bytecode: 0x5640da8e0aa0>
#> <environment: namespace:base>
#> 
my_storage$get(c("text", "!text"))
#> list()
my_storage$get(c("text", "!text"), logical = "or")
#> [[1]]
#> [1] 42
#> 
#> [[2]]
#> [1] 3.141593
#> 
#> [[3]]
#> [1] "fear of black cats"
#> 
#> [[4]]
#> [1] "wearing a seat belt"
#> 
#> [[5]]
#> function (x, ...) 
#> UseMethod("mean")
#> <bytecode: 0x5640da8e0aa0>
#> <environment: namespace:base>
#> 

# 5. Extract elements based on ids:
my_storage$get(ids = 4:5)
#> [[1]]
#> [1] "wearing a seat belt"
#> 
#> [[2]]
#> function (x, ...) 
#> UseMethod("mean")
#> <bytecode: 0x5640da8e0aa0>
#> <environment: namespace:base>
#> 
my_storage$get(ids = 4:5, id_names = TRUE) # add the ids as names
#> $`4`
#> [1] "wearing a seat belt"
#> 
#> $`5`
#> function (x, ...) 
#> UseMethod("mean")
#> <bytecode: 0x5640da8e0aa0>
#> <environment: namespace:base>
#> 
```
