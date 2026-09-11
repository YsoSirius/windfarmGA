# Adjust the amount of turbines per windfarm

Legacy repair for binary chromosomes. The GA loop encodes layouts as `n`
unique grid IDs, so this function is not called there. It remains
exported for the old 0/1 pipeline
([`crossover`](https://YsoSirius.github.io/windfarmGA/reference/crossover.md)
/
[`mutation`](https://YsoSirius.github.io/windfarmGA/reference/mutation.md)).

## Usage

``` r
trimton(mut, nturb, allparks, nGrids, trimForce, seed)
```

## Arguments

- mut:

  A binary matrix with the mutated individuals

- nturb:

  A numeric value indicating the amount of required turbines

- allparks:

  A data.frame consisting of all individuals of the current generation

- nGrids:

  A numeric value indicating the total amount of grid cells

- trimForce:

  If `TRUE`, add or drop turbines using fitness-weighted probabilities.
  If `FALSE`, choose cells at random.

- seed:

  Set a seed for comparability. Default is NULL

## Value

Returns a binary matrix with the correct amount of turbines per
individual

## See also

Other Genetic Algorithm Functions:
[`crossover()`](https://YsoSirius.github.io/windfarmGA/reference/crossover.md),
[`fitness()`](https://YsoSirius.github.io/windfarmGA/reference/fitness.md),
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md),
[`init_population()`](https://YsoSirius.github.io/windfarmGA/reference/init_population.md),
[`mutation()`](https://YsoSirius.github.io/windfarmGA/reference/mutation.md),
[`selection()`](https://YsoSirius.github.io/windfarmGA/reference/selection.md),
[`set_crossover()`](https://YsoSirius.github.io/windfarmGA/reference/set_crossover.md),
[`swap_mutation()`](https://YsoSirius.github.io/windfarmGA/reference/swap_mutation.md)

## Examples

``` r
# \donttest{
## Create a random rectangular shapefile
library(sf)
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(0, 0, 2000, 2000, 0),
    c(0, 2000, 2000, 0, 0)
  ))),
  crs = 3035
))

## Create a uniform and unidirectional wind data.frame and plots the
## resulting wind rose
## Uniform wind speed and single wind direction
data.in <- as.data.frame(cbind(ws = 12, wd = 0))

## Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
Grid1 <- grid_area(area = area, size = 200, prop = 1)
Grid <- Grid1[[1]]
AmountGrids <- nrow(Grid)

startsel <- init_population(Grid, 10, 20)
wind <- as.data.frame(cbind(ws = 12, wd = 0))
wind <- list(wind, probab = 100)
fit <- fitness(
  population = startsel, reference_height = 100, rotor_height = 100,
  surface_roughness = 0.3, area = area, rotor = 20,
  wind = wind, terrain = FALSE
)
allparks <- do.call("rbind", fit)
## selection() returns ID matrices; crossover()/trimton() expect 0/1.
sel <- selection(fit, Grid, 2, TRUE, 6, "FIX")
ids <- sel[[1]]
bins <- matrix(0, nrow(Grid), ncol(ids))
for (j in seq_len(ncol(ids))) {
  bins[match(ids[, j], Grid[, "ID"]), j] <- 1
}
selec6best <- list(
  data.frame(ID = Grid[, "ID"], bins),
  data.frame(ID = 1, t(sel[[2]]))
)
crossOut <- crossover(selec6best, 2, uplimit = 300, crossPart = "RAN")
mut <- mutation(a = crossOut, p = 0.3, NULL)
mut1 <- trimton(
  mut = mut, nturb = 10, allparks = allparks, nGrids = AmountGrids,
  trimForce = FALSE
)
colSums(mut)
#>  [1] 30 31 28 32 36 41 34 34 35 37 35 26 39 32 32 38 38 34 37 36 22 34 36 41 37
#> [26] 35 32 23 36 37 39 36
colSums(mut1)
#>  [1] 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
#> [26] 10 10 10 10 10 10 10
# }
```
