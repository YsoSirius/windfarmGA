# Evaluate the Individual Fitness values

The fitness of all individuals in the current population is calculated
after their energy output has been evaluated in
[`calculate_energy`](https://YsoSirius.github.io/windfarmGA/reference/calculate_energy.md).
This function reduces the resulting energy outputs to a single fitness
value for each individual.

## Usage

``` r
fitness(
  population,
  reference_height,
  rotor_height,
  surface_roughness,
  area,
  rotor,
  wind,
  elevation = NULL,
  terrain = FALSE,
  ccl_raster = NULL,
  weibull = FALSE,
  parallel = FALSE,
  n_cluster = 2
)
```

## Arguments

- population:

  A list of individuals (layouts with X/Y and cell IDs).

- reference_height:

  Height at which `wind$ws` was measured.

- rotor_height:

  Hub height in metres.

- surface_roughness:

  Roughness length in metres. Per-cell when `terrain` is on.

- area:

  Site polygon (`sf`, SpatialPolygons, or coordinate matrix). Must be
  projected in metres.

- rotor:

  Rotor radius in metres.

- wind:

  Wind data as returned by
  [`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)
  (`list(df, probab)`).

- elevation:

  Terrain list from
  [`terrain_model()`](https://YsoSirius.github.io/windfarmGA/reference/terrain_model.md)
  (elevation, orography, roughness). Unused when `terrain` is `FALSE`.

- terrain:

  Terrain model (elevation + land cover). `TRUE` downloads a DEM via
  `elevatr`. Pass a DEM raster to skip the download. Per-cell values are
  computed once and stored in the result as `terrainModel` for
  [`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md)
  /
  [`random_search()`](https://YsoSirius.github.io/windfarmGA/reference/random_search.md).

- ccl_raster:

  Land-cover roughness raster from
  [`terrain_model()`](https://YsoSirius.github.io/windfarmGA/reference/terrain_model.md).

- weibull:

  Raster of estimated wind speeds, or `FALSE`.

- parallel:

  Parallel fitness (`parallel` + `doParallel`).

- n_cluster:

  Worker count when `parallel` is `TRUE`.

## Value

Returns a list with every individual, consisting of X & Y coordinates,
rotor radii, the runs and the selected grid cell IDs, and the resulting
energy outputs, efficiency rates and fitness values.

## See also

Other Genetic Algorithm Functions:
[`crossover()`](https://YsoSirius.github.io/windfarmGA/reference/crossover.md),
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md),
[`init_population()`](https://YsoSirius.github.io/windfarmGA/reference/init_population.md),
[`mutation()`](https://YsoSirius.github.io/windfarmGA/reference/mutation.md),
[`selection()`](https://YsoSirius.github.io/windfarmGA/reference/selection.md),
[`set_crossover()`](https://YsoSirius.github.io/windfarmGA/reference/set_crossover.md),
[`swap_mutation()`](https://YsoSirius.github.io/windfarmGA/reference/swap_mutation.md),
[`trimton()`](https://YsoSirius.github.io/windfarmGA/reference/trimton.md)

## Examples

``` r
# \donttest{
## Create a random rectangular shapefile
library(sf)
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))

## Create a uniform and unidirectional wind data.frame and plots the
## resulting wind rose
## Uniform wind speed and single wind direction
wind <- data.frame(ws = 12, wd = 0)
# windrosePlot <- plot_windrose(data = wind, spd = wind$ws,
#                dir = wind$wd, dirres=10, spdmax=20)

## Calculate a Grid and an indexed data.frame with coordinates and
## grid cell IDs.
Grid1 <- grid_area(area = area, size = 200, prop = 1)
Grid <- Grid1[[1]]
AmountGrids <- nrow(Grid)

wind <- list(wind, probab = 100)
startsel <- init_population(Grid, 10, 20)
fit <- fitness(
  population = startsel, reference_height = 100, rotor_height = 100,
  surface_roughness = 0.3, area = area, rotor = 20,
  wind = wind, terrain = FALSE, parallel = FALSE
)
# }
```
