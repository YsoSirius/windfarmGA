# Find potentially influencing turbines

Find all turbines that could potentially influence another turbine and
save them to a list.

## Usage

``` r
turbine_influences(t, wnkl, dist, area, dirct, plot_angles = FALSE)
```

## Arguments

- t:

  A data.frame of the current individual with X and Y coordinates

- wnkl:

  Wake opening angle in degrees. Turbines outside this cone are ignored.

- dist:

  A numeric value indicating the distance, after which the wake effects
  are considered to be eliminated.

- area:

  Site polygon

- dirct:

  Current wind direction

- plot_angles:

  Plot distances and angles

## Value

Returns a list of all individuals of the current generation which could
potentially influence other turbines. List includes the relevant
coordinates, the distances and angles in between and assigns the Point
ID.

## See also

Other Wind Energy Calculation Functions:
[`barometric_height()`](https://YsoSirius.github.io/windfarmGA/reference/barometric_height.md),
[`calculate_energy()`](https://YsoSirius.github.io/windfarmGA/reference/calculate_energy.md),
[`circle_intersection()`](https://YsoSirius.github.io/windfarmGA/reference/circle_intersection.md),
[`get_dist_angles()`](https://YsoSirius.github.io/windfarmGA/reference/get_dist_angles.md)

## Examples

``` r
## Exemplary input Polygon with 2km x 2km:
library(sf)

area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(0, 0, 2000, 2000, 0),
    c(0, 2000, 2000, 0, 0)
  ))),
  crs = 3035
))

t <- st_coordinates(st_sample(area, 10))
t <- cbind(t, "Z" = 1)
wnkl <- 20
dist <- 100000
dirct <- 0

res <- turbine_influences(t, wnkl, dist, area, dirct, plot_angles = TRUE)










```
