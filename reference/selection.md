# Selection Method

Select a certain amount of individuals and recombine them to parental
teams. Add the mean fitness value of both parents to the parental team.
Depending on the selected `selection_mode`, the algorithm will either
take always 50 percent or a variable percentage of the current
population. The variable percentage depends on the evolution of the
populations fitness values. With `elitism = TRUE` the best individuals
are always included in the mating pool.

## Usage

``` r
selection(
  fit,
  grid,
  share,
  elitism = TRUE,
  n_elite = 3,
  selection_mode = "VAR",
  verbose = FALSE
)
```

## Arguments

- fit:

  A list of all fitness-evaluated individuals

- grid:

  Indexed grid from
  [`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md)

- share:

  Selection divisor: parents are about `nrow / share` of the population
  (`2` ≈ 50 %).

- elitism:

  Archive the best layout and breed elite children.

- n_elite:

  Base elite count (grows/shrinks with search phase).

- selection_mode:

  `"VAR"` (parent share follows fitness) or `"FIX"` (50 %).

- verbose:

  If TRUE, will print out further information.

## Value

Returns a list with 2 elements. Element 1 is an integer matrix of
selected layouts (`n` turbines × selected individuals), each column a
set of unique grid cell IDs. Element 2 is the fitness of each selected
individual.

## See also

Other Genetic Algorithm Functions:
[`crossover()`](https://YsoSirius.github.io/windfarmGA/reference/crossover.md),
[`fitness()`](https://YsoSirius.github.io/windfarmGA/reference/fitness.md),
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md),
[`init_population()`](https://YsoSirius.github.io/windfarmGA/reference/init_population.md),
[`mutation()`](https://YsoSirius.github.io/windfarmGA/reference/mutation.md),
[`set_crossover()`](https://YsoSirius.github.io/windfarmGA/reference/set_crossover.md),
[`swap_mutation()`](https://YsoSirius.github.io/windfarmGA/reference/swap_mutation.md),
[`trimton()`](https://YsoSirius.github.io/windfarmGA/reference/trimton.md)

## Examples

``` r
# \donttest{
## Exemplary input Polygon with 2km x 2km:
library(sf)
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))

## Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
Grid1 <- grid_area(area = area, size = 200, prop = 1)
Grid <- Grid1[[1]]
AmountGrids <- nrow(Grid)

startsel <- init_population(Grid, 10, 20)
wind <- as.data.frame(cbind(ws = 12, wd = 0))
wind <- list(wind, probab = 100)
fit <- fitness(
  population = startsel, reference_height = 100, rotor_height = 100,
  surface_roughness = 0.3, area = area, rotor = 20, wind = wind,
  terrain = FALSE
)
allparks <- do.call("rbind", fit)
## SELECTION
## print the amount of Individuals selected. Check if the amount
## of Turbines is as requested.
selec6best <- selection(fit, Grid, 2, TRUE, 6, "VAR")
selec6best <- selection(fit, Grid, 2, TRUE, 6, "FIX")
selec6best <- selection(fit, Grid, 4, FALSE, 6, "FIX")
# }
```
