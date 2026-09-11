# Calculate distances and angles of possibly influencing turbines

Calculate distances and angles for a turbine and all it's potentially
influencing turbines.

## Usage

``` r
as_xy_matrix(t)
```

## Arguments

- t:

  A data.frame of the current individual with X and Y coordinates

- o:

  A numeric value indicating the index of the current turbine

## Value

Returns a matrix with the distances, angles and heights of potentially
influencing turbines

## See also

Other Wind Energy Calculation Functions:
[`barometric_height()`](https://YsoSirius.github.io/windfarmGA/reference/barometric_height.md),
[`calculate_energy()`](https://YsoSirius.github.io/windfarmGA/reference/calculate_energy.md),
[`circle_intersection()`](https://YsoSirius.github.io/windfarmGA/reference/circle_intersection.md),
[`turbine_influences()`](https://YsoSirius.github.io/windfarmGA/reference/turbine_influences.md)

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

## Create a random windfarm with 10 turbines
t <- st_coordinates(st_sample(area, 10))
t <- cbind(t, "Z" = 1)
wnkl <- 20
dist <- 100000

## Evaluate and plot for every turbine all other potentially influencing turbines
potInfTur <- list()
for (i in 1:(length(t[, 1]))) {
  potInfTur[[i]] <- get_dist_angles(
    t = t, o = i, wnkl = wnkl,
    dist = dist, area = area, plot_angles = TRUE
  )
}
#> Error in get_dist_angles(t = t, o = i, wnkl = wnkl, dist = dist, area = area,     plot_angles = TRUE): could not find function "get_dist_angles"
potInfTur
#> list()
```
