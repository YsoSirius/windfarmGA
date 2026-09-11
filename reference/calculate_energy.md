# Calculate Energy Outputs of Individuals

Calculate the energy output and efficiency rates of an individual in the
current population under all given wind directions and speeds. If the
terrain effect model is activated, the main calculations to model those
effects will be done in this function.

## Usage

``` r
calculate_energy(
  layout,
  reference_height,
  rotor_height,
  surface_roughness,
  wake_angle,
  wake_distance,
  area,
  rotor,
  wind,
  elevation = NULL,
  terrain = FALSE,
  ccl_raster = NULL,
  weibull = FALSE,
  park_center = NULL,
  plot = FALSE
)
```

## Arguments

- layout:

  One individual: matrix with X/Y (and typically cell IDs).

- reference_height:

  Height at which `wind$ws` was measured.

- rotor_height:

  Hub height in metres.

- surface_roughness:

  Roughness length in metres. Per-cell when `terrain` is on.

- wake_angle:

  Angle (degrees) beyond which wake influence is ignored.

- wake_distance:

  Distance (metres) beyond which wake effects are ignored.

- area:

  Site polygon (`sf`, SpatialPolygons, or coordinate matrix). Must be
  projected in metres.

- rotor:

  Rotor radius in metres.

- wind:

  Wind data.frame with `ws`, `wd` and optional `probab`. See
  [`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md).

- elevation:

  Terrain list from
  [`terrain_model()`](https://YsoSirius.github.io/windfarmGA/reference/terrain_model.md).
  Unused when `terrain` is `FALSE`.

- terrain:

  Terrain model (elevation + land cover).

- ccl_raster:

  Land-cover roughness raster from
  [`terrain_model()`](https://YsoSirius.github.io/windfarmGA/reference/terrain_model.md).

- weibull:

  If `TRUE`, hub-height speed comes from Weibull rasters; `wind$ws` is
  ignored.

- park_center:

  Optional numeric of length 2 (`x`, `y`) used as rotation origin.
  Computed from the polygon bounding box when missing.

- plot:

  If `TRUE`, the process will be plotted.

## Value

Returns a list of an individual of the current generation with resulting
wake effects, energy outputs, efficiency rates for every wind direction.
The length of the list corresponds to the number of different wind
directions.

## See also

Other Wind Energy Calculation Functions:
[`barometric_height()`](https://YsoSirius.github.io/windfarmGA/reference/barometric_height.md),
[`circle_intersection()`](https://YsoSirius.github.io/windfarmGA/reference/circle_intersection.md),
[`get_dist_angles()`](https://YsoSirius.github.io/windfarmGA/reference/get_dist_angles.md),
[`turbine_influences()`](https://YsoSirius.github.io/windfarmGA/reference/turbine_influences.md)

## Examples

``` r
# \donttest{
## Create a random Polygon
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))

## Create a uniform and unidirectional wind data.frame and plot the
## resulting wind rose
data.in <- data.frame(ws = 12, wd = 0)
windrosePlot <- plot_windrose(
  data = data.in, spd = data.in$ws,
  dir = data.in$wd, dirres = 10, spdmax = 20
)


## Assign the rotor radius and a factor of the radius for grid spacing.
rotor <- 50
fcr <- 3
resGrid <- grid_area(
  area = area, size = rotor * fcr, prop = 1,
  plot_grid = TRUE
)


## Assign the indexed data frame to new variable. Element 2 of the list
## is the grid, saved as Simple Feature Polygons.
resGrid1 <- resGrid[[1]]

## Create an initial population with the indexed Grid, 15 turbines and
## 100 individuals.
initpop <- init_population(grid = resGrid1, n = 15, n_start = 100)

## Calculate the expected energy output of the first individual of the
## population.
par(mfrow = c(1, 2))
plot(area)
points(initpop[[1]][, "X"], initpop[[1]][, "Y"], pch = 20, cex = 2)
plot(resGrid[[2]], add = TRUE)

resCalcEn <- calculate_energy(
  layout = initpop[[1]], reference_height = 50,
  rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
  wake_distance = 100000, wind = data.in,
  rotor = 50, area = area, terrain = FALSE,
  weibull = FALSE
)
resCalcEn <- as.data.frame(resCalcEn)
plot(area, main = resCalcEn[, "Energy_Output_Red"][[1]])
points(x = resCalcEn[, "Bx"], y = resCalcEn[, "By"], pch = 20)



## Create a variable and multidirectional wind data.frame and plot the
## resulting wind rose
data.in10 <- data.frame(ws = runif(10, 1, 25), wd = runif(10, 0, 360))
windrosePlot <- plot_windrose(
  data = data.in10, spd = data.in10$ws,
  dir = data.in10$wd, dirres = 10, spdmax = 20
)


## Calculate the energy outputs for the first individual with more than one
## wind direction.
resCalcEn <- calculate_energy(
  layout = initpop[[1]], reference_height = 50,
  rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
  wake_distance = 100000, wind = data.in10,
  rotor = 50, area = area, terrain = FALSE,
  weibull = FALSE
)
# }
```
