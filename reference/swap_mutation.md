# Swap mutation of turbine layouts

Replace occupied grid cells with unused ones. The number of swaps is
`max(min_swaps, Binomial(n, p))`, so each individual explores at least
`min_swaps` new cells (default 1). The number of turbines stays `n`.

## Usage

``` r
swap_mutation(ids, grid_ids, p, seed = NULL, min_swaps = NULL, visit = NULL)
```

## Arguments

- ids:

  Integer matrix with `n` rows (turbines) and one column per individual

- grid_ids:

  All valid grid cell IDs

- p:

  Mutation probability per turbine

- seed:

  Set a seed for comparability. Default is `NULL`

- min_swaps:

  Minimum number of swaps per individual. Default is
  `getOption("windfarmGA.min_swaps")` (1)

- visit:

  Named visit counts per grid ID. Free cells with fewer visits are more
  likely to be chosen. Default is `NULL` (uniform)

## Value

Integer matrix of unique grid IDs, same dimension as `ids`

## See also

Other Genetic Algorithm Functions:
[`crossover()`](https://YsoSirius.github.io/windfarmGA/reference/crossover.md),
[`fitness()`](https://YsoSirius.github.io/windfarmGA/reference/fitness.md),
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md),
[`init_population()`](https://YsoSirius.github.io/windfarmGA/reference/init_population.md),
[`mutation()`](https://YsoSirius.github.io/windfarmGA/reference/mutation.md),
[`selection()`](https://YsoSirius.github.io/windfarmGA/reference/selection.md),
[`set_crossover()`](https://YsoSirius.github.io/windfarmGA/reference/set_crossover.md),
[`trimton()`](https://YsoSirius.github.io/windfarmGA/reference/trimton.md)

## Examples

``` r
ids <- cbind(c(1, 3, 5, 7), c(2, 4, 6, 8))
swap_mutation(ids, grid_ids = 1:20, p = 0.5, seed = 1)
#>      [,1] [,2]
#> [1,]    1   19
#> [2,]    3   15
#> [3,]    5    3
#> [4,]   11    8
```
