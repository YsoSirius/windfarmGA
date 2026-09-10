# Adjust the amount of turbines per windfarm

Adjust the mutated individuals to the required amount of turbines.

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

  If `TRUE` the algorithm will use a probabilistic approach to correct
  the windfarms to the desired amount of turbines. If `FALSE` the
  adjustment will be random. Default is `FALSE`

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
[`selection()`](https://YsoSirius.github.io/windfarmGA/reference/selection.md)

## Examples

``` r
# \donttest{
## Create a random rectangular shapefile
library(sf)
Polygon1 <- sf::st_as_sf(sf::st_sfc(
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
Grid1 <- grid_area(shape = Polygon1, size = 200, prop = 1)
Grid <- Grid1[[1]]
AmountGrids <- nrow(Grid)

startsel <- init_population(Grid, 10, 20)
wind <- as.data.frame(cbind(ws = 12, wd = 0))
wind <- list(wind, probab = 100)
fit <- fitness(
  selection = startsel, referenceHeight = 100, RotorHeight = 100,
  SurfaceRoughness = 0.3, Polygon = Polygon1, resol1 = 200, rot = 20,
  dirspeed = wind, srtm_crop = "", topograp = FALSE, cclRaster = ""
)
allparks <- do.call("rbind", fit)
## SELECTION
## print the amount of Individuals selected.
## Check if the amount of Turbines is as requested.
selec6best <- selection(fit, Grid, 2, TRUE, 6, "VAR")
selec6best <- selection(fit, Grid, 2, TRUE, 6, "FIX")
selec6best <- selection(fit, Grid, 4, FALSE, 6, "FIX")
## CROSSOVER
## u determines the amount of crossover points,
## crossPart determines the method used (Equal/Random),
## uplimit is the maximum allowed permutations
crossOut <- crossover(selec6best, 2, uplimit = 300, crossPart = "RAN")
crossOut <- crossover(selec6best, 7, uplimit = 500, crossPart = "RAN")
crossOut <- crossover(selec6best, 3, uplimit = 300, crossPart = "EQU")
## MUTATION
## Variable Mutation Rate is activated if more than 2 individuals represent
## the current best solution.
mut <- mutation(a = crossOut, p = 0.3, NULL)
## TRIMTON
## After Crossover and Mutation, the amount of turbines in a windpark change and have to be
## corrected to the required amount of turbines.
mut1 <- trimton(
  mut = mut, nturb = 10, allparks = allparks, nGrids = AmountGrids,
  trimForce = FALSE
)
colSums(mut)
#>  [1] 30 28 31 35 28 41 33 43 33 37 35 31 27 32 32 35 32 33 28 36 27 34 29 36 28
#> [26] 30 52 35 40 33 40 22 43 41 41 30 25 39 39 33 35 37 36 34 31 41 35 35 35 34
#> [51] 38 31 39 39 30 31 34 32 32 35 28 30 34 38
colSums(mut1)
#>  [1] 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
#> [26] 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
#> [51] 10 10 10 10 10 10 10 10 10 10 10 10 10 10
# }
```
