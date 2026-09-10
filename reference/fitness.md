# Evaluate the Individual Fitness values

The fitness of all individuals in the current population is calculated
after their energy output has been evaluated in
[`calculate_energy`](https://YsoSirius.github.io/windfarmGA/reference/calculate_energy.md).
This function reduces the resulting energy outputs to a single fitness
value for each individual.

## Usage

``` r
fitness(
  selection,
  referenceHeight,
  RotorHeight,
  SurfaceRoughness,
  Polygon,
  resol1,
  rot,
  dirspeed,
  srtm_crop,
  topograp,
  cclRaster,
  weibull,
  Parallel,
  numCluster
)
```

## Arguments

- selection:

  A list containing all individuals of the current population.

- referenceHeight:

  The height at which the incoming wind speeds were measured. Default is
  `RotorHeight`

- RotorHeight:

  The height of the turbine hub

- SurfaceRoughness:

  A surface roughness length in meters. With the terrain effect model, a
  surface roughness is calculated for every grid cell using the
  elevation and land cover data. Default is `0.3`

- Polygon:

  The considered area as shapefile.

- resol1:

  The resolution of the grid in meter.

- rot:

  The desired rotor radius in meter.

- dirspeed:

  The wind data as list.

- srtm_crop:

  A list of 3 raster, with 1) the elevation, 2) an orographic and 3) a
  terrain raster. Calculated in
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- topograp:

  Boolean value, which indicates if the terrain effect model should be
  enabled or not. Default is `FALSE`

- cclRaster:

  A Corine Land Cover raster, that has to be adapted previously by hand
  with the surface roughness length for every land cover type. Is only
  used, when the terrain effect model is activated.

- weibull:

  A raster representing the estimated wind speeds

- Parallel:

  A boolean value, indicating whether parallel processing should be
  used. The *parallel* and *doParallel* packages are used for parallel
  processing. Default is `FALSE`

- numCluster:

  If `Parallel` is TRUE, this variable defines the number of clusters to
  be used. Default is `2`

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
[`trimton()`](https://YsoSirius.github.io/windfarmGA/reference/trimton.md)

## Examples

``` r
# \donttest{
## Create a random rectangular shapefile
library(sf)
Polygon1 <- sf::st_as_sf(sf::st_sfc(
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
Grid1 <- grid_area(shape = Polygon1, size = 200, prop = 1)
Grid <- Grid1[[1]]
AmountGrids <- nrow(Grid)

wind <- list(wind, probab = 100)
startsel <- init_population(Grid, 10, 20)
fit <- fitness(
  selection = startsel, referenceHeight = 100, RotorHeight = 100,
  SurfaceRoughness = 0.3, Polygon = Polygon1, resol1 = 200, rot = 20,
  dirspeed = wind, srtm_crop = "", topograp = FALSE, cclRaster = "",
  Parallel = FALSE
)
# }
```
