# Simulator R6 Object

Creates a simulation setup, where a function `f` is evaluated `runs`
times, optionally at each combination of input values.

Provides some convenience (see below for more details):

- Simulation results can be restored from a backup if the R session
  crashes.

- More simulation runs can be conducted after the initial simulation,
  failed simulation cases can be re-run.

- Parallel computation and progress updates are supported.

## Details

### Backup

Simulation results can be saved to disk, allowing you to restore the
results if the R session is interrupted or crashes before the simulation
completes. To enable backup, set `backup = TRUE` in the `$go()` method,
which will create a backup directory at the location specified by
`path`. To restore, use `Simulator$initialize(use_backup = path)`.

### More runs and re-run

If additional simulation runs are needed, simply call the `$go()` method
again. Any cases that were not successfully completed in previous runs
will be attempted again.

### Parallel computation

By default, simulations run sequentially. But since they are
independent, they can be parallelized to decrease computation time. To
enable parallel computation, use the [`{future}`
framework](https://future.futureverse.org/). For example, run

    future::plan(future::multisession, workers = 4)

in advance for computation in 4 parallel R sessions.

### Progress updates

Use the [`{progressr}` framework](https://progressr.futureverse.org/) to
get progress updates. For example, run the following in advance:

    progressr::handlers(global = TRUE)
    progressr::handlers(
      progressr::handler_progress(format = ">> :percent, :eta to go :message")
    )

## See also

Other simulation helpers:
[`correlated_regressors()`](http://loelschlaeger.de/oeli/reference/correlated_regressors.md),
[`ddirichlet_cpp()`](http://loelschlaeger.de/oeli/reference/ddirichlet.md),
[`dmixnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmixnorm.md),
[`dmvnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md),
[`dtnorm_cpp()`](http://loelschlaeger.de/oeli/reference/dtnorm.md),
[`dwishart_cpp()`](http://loelschlaeger.de/oeli/reference/dwishart.md),
[`gaussian_tv()`](http://loelschlaeger.de/oeli/reference/gaussian_tv.md),
[`simulate_markov_chain()`](http://loelschlaeger.de/oeli/reference/simulate_markov_chain.md)

## Active bindings

- `results`:

  \[`tibble`, read-only\]  
  The simulation results.

- `cases`:

  \[`tibble`, read-only\]  
  The simulation cases.

## Methods

### Public methods

- [`Simulator$new()`](#method-Simulator-new)

- [`Simulator$print()`](#method-Simulator-print)

- [`Simulator$define()`](#method-Simulator-define)

- [`Simulator$go()`](#method-Simulator-go)

------------------------------------------------------------------------

### Method `new()`

Initialize a `Simulator` object, either a new one or from backup.

#### Usage

    Simulator$new(
      use_backup = NULL,
      verbose = getOption("verbose", default = FALSE)
    )

#### Arguments

- `use_backup`:

  \[`NULL` \| `character(1)`\]  
  Optionally a path to a backup folder previously used in `$go()`.

- `verbose`:

  \[`logical(1)`\]  
  Provide info? Does not include progress updates. For that, see
  details.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method.

#### Usage

    Simulator$print()

------------------------------------------------------------------------

### Method `define()`

Define function and arguments for a new `Simulator` object.

#### Usage

    Simulator$define(f, ...)

#### Arguments

- `f`:

  \[`function`\]  
  A `function` to evaluate.

- `...`:

  Arguments for `f`. Each value must be

  1.  named after an argument of `f`, and

  2.  a `list`, where each element is a variant of that argument for
      `f`.

------------------------------------------------------------------------

### Method `go()`

Run simulations.

#### Usage

    Simulator$go(
      runs = 0,
      backup = FALSE,
      path = paste0("backup_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
    )

#### Arguments

- `runs`:

  \[`integer(1)`\]  
  The number of (additional) simulation runs.

  If `runs = 0`, only pending cases (if any) are solved.

- `backup`:

  \[`logical(1)`\]  
  Create a backup under `path`?

- `path`:

  \[`character(1)`\]  
  Only relevant, if `backup = TRUE`.

  In this case, a path for a new folder, which does not yet exist and
  allows reading and writing.

## Examples

``` r
# 1. Initialize a new simulation setup:
object <- Simulator$new(verbose = TRUE)
#> Created <Simulator>, call `$define()` next.

# 2. Define function `f` and arguments (if any):
f <- function(x, y = 1) {
  Sys.sleep(runif(1)) # to see progress updates
  x + y
}
x_args <- list(1, 2)
object$define(f = f, x = x_args)
#> Simulation details defined, call `$go()` next to run simulations.
print(object)
#> • Total cases: 0
#> → Pending cases: 0
#> ✔ Successful cases: 0
#> ✖ Failed cases: 0

# 3. Define 'future' and 'progress' (optional):
if (FALSE) { # \dontrun{
future::plan(future::sequential)
progressr::handlers(global = TRUE)} # }

# 4. Evaluate `f` `runs` times at each parameter combination (backup is optional):
path <- file.path(tempdir(), paste0("backup_", format(Sys.time(), "%Y-%m-%d-%H-%M-%S")))
object$go(runs = 2, backup = TRUE, path = path)
#> Saving backup to path /tmp/RtmpWxT7KY/backup_2026-04-22-22-09-05.
#> Started simulation with 4 cases...
#> Simulation complete, 0 cases failed.

# 5. Access the results:
object$results
#> # A tibble: 4 × 4
#>   .case .seconds       .input           .output  
#>   <int> <drtn>         <list>           <list>   
#> 1     1 0.5169051 secs <named list [1]> <dbl [1]>
#> 2     2 0.2302802 secs <named list [1]> <dbl [1]>
#> 3     3 0.6744037 secs <named list [1]> <dbl [1]>
#> 4     4 0.6663687 secs <named list [1]> <dbl [1]>

# 6. Check if cases are pending or if an error occurred:
object$cases
#> # A tibble: 4 × 5
#>   .case .pending .error  .run x        
#>   <int> <lgl>    <lgl>  <int> <list>   
#> 1     1 FALSE    FALSE      1 <dbl [1]>
#> 2     2 FALSE    FALSE      1 <dbl [1]>
#> 3     3 FALSE    FALSE      2 <dbl [1]>
#> 4     4 FALSE    FALSE      2 <dbl [1]>

# 7. Restore simulation results from backup:
object_restored <- Simulator$new(use_backup = path)
#> Loaded <Simulator> and 4 cases from backup.
print(object_restored)
#> • Total cases: 4
#> → Pending cases: 0
#> ✔ Successful cases: 4
#> ✖ Failed cases: 0
if (FALSE) all.equal(object, object_restored) # \dontrun{}

# 8. Run more simulations and pending simulations (if any):
object_restored$go(runs = 2)
#> Started simulation with 4 cases...
#> Simulation complete, 0 cases failed.
```
