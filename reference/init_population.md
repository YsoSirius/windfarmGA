# Create a random initial Population

Create `n_start` random sub-selections from the indexed grid and assign
binary variable 1 to selected grids. This function initiates the genetic
algorithm with a first random population and will only be needed in the
first iteration.

## Usage

``` r
init_population(grid, n, n_start = 100)
```

## Arguments

- grid:

  Indexed grid from
  [`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md)
  (X, Y, cell IDs).

- n:

  A numeric value indicating the amount of required turbines.

- n_start:

  A numeric indicating the amount of randomly generated initial
  individuals. Default is 100.

## Value

Returns a list of `n_start` initial individuals, each consisting of `n`
turbines. Resulting list has the x and y coordinates, the grid cell ID
and a binary variable of 1, indicating a turbine in the grid cell.

## See also

Other Genetic Algorithm Functions:
[`crossover()`](https://YsoSirius.github.io/windfarmGA/reference/crossover.md),
[`fitness()`](https://YsoSirius.github.io/windfarmGA/reference/fitness.md),
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md),
[`mutation()`](https://YsoSirius.github.io/windfarmGA/reference/mutation.md),
[`selection()`](https://YsoSirius.github.io/windfarmGA/reference/selection.md),
[`set_crossover()`](https://YsoSirius.github.io/windfarmGA/reference/set_crossover.md),
[`swap_mutation()`](https://YsoSirius.github.io/windfarmGA/reference/swap_mutation.md),
[`trimton()`](https://YsoSirius.github.io/windfarmGA/reference/trimton.md)

## Examples

``` r
library(sf)
## Exemplary input Polygon with 2km x 2km:
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))

Grid <- grid_area(area, 200, 1, TRUE)



## Create 5 individuals with 10 wind turbines each.
firstPop <- init_population(grid = Grid[[1]], n = 10, n_start = 5)
```
