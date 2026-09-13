# Set crossover of turbine layouts

Combine two layouts of `n` unique grid-cell IDs. Shared sites are kept;
remaining sites are sampled from the parents' exclusive cells and, with
rate `p_inject`, from grid cells that neither parent uses. Identical
parents still get unused cells injected so the search does not freeze.
Every child has exactly `n` turbines.

## Usage

``` r
set_crossover(
  ids,
  grid_ids,
  uplimit = 300,
  seed = NULL,
  verbose = FALSE,
  p_inject = NULL,
  grid_xy = NULL,
  visit = NULL,
  p_spatial = NULL
)
```

## Arguments

- ids:

  Integer matrix with `n` rows (turbines) and one column per parent

- grid_ids:

  All valid grid cell IDs

- uplimit:

  Maximum number of children. Default is 300

- seed:

  Set a seed for comparability. Default is `NULL`

- verbose:

  If `TRUE`, print the number of children

- p_inject:

  Fraction of non-shared slots filled from unused grid cells. Default is
  `getOption("windfarmGA.crossover_inject")` (0.25). At least one unused
  cell is injected when any are available.

- grid_xy:

  Optional matrix/data.frame with columns `ID`, `X`, `Y`. If given, a
  spatial half-plane crossover is used with probability `p_spatial`.

- visit:

  Named visit counts per grid ID (undersampled cells preferred)

- p_spatial:

  Probability of spatial (vs set) crossover when `grid_xy` is given.
  Default is `getOption("windfarmGA.spatial_crossover")` (0.5)

## Value

Integer matrix of unique grid IDs (`n` x children)

## See also

Other Genetic Algorithm Functions:
[`crossover()`](https://YsoSirius.github.io/windfarmGA/reference/crossover.md),
[`fitness()`](https://YsoSirius.github.io/windfarmGA/reference/fitness.md),
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md),
[`init_population()`](https://YsoSirius.github.io/windfarmGA/reference/init_population.md),
[`mutation()`](https://YsoSirius.github.io/windfarmGA/reference/mutation.md),
[`selection()`](https://YsoSirius.github.io/windfarmGA/reference/selection.md),
[`swap_mutation()`](https://YsoSirius.github.io/windfarmGA/reference/swap_mutation.md),
[`trimton()`](https://YsoSirius.github.io/windfarmGA/reference/trimton.md)

## Examples

``` r
ids <- cbind(c(1, 3, 5, 7), c(1, 4, 5, 9))
set_crossover(ids, grid_ids = 1:20, uplimit = 4, seed = 1)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    1    1    1
#> [2,]    2    5    4    5
#> [3,]    4    9    5    6
#> [4,]    5   19   17    9
```
