# Make a grid from a Simple Feature Polygon

Create a grid from a given polygon with a certain resolution and
proportionality. The grid cell centroids represent possible wind turbine
locations.

## Usage

``` r
grid_area(shape, size = 500, prop = 1, plotGrid = FALSE)
```

## Arguments

- shape:

  Simple Feature Polygon of the considered area

- size:

  The cellsize of the grid in meters. Default is 500

- prop:

  A factor used for grid calculation. It determines the minimum
  percentage that a grid cell must cover the area. Default is 1

- plotGrid:

  Logical value indicating whether the results should be plotted.
  Default is `FALSE`

## Value

Returns a list with 2 elements. List element 1 will have the grid cell
IDS, and the X and Y coordinates of the centers of each grid cell. List
element 2 is the grid as Simple Feature Polygons, which is used for
plotting purposes.

## Note

The grid of the genetic algorithm will have a resolution of
`Rotor * fcrR`. See the arguments of
[`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

## See also

Other Helper Functions:
[`get_grids()`](https://YsoSirius.github.io/windfarmGA/reference/get_grids.md),
[`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md),
[`isSpatial()`](https://YsoSirius.github.io/windfarmGA/reference/isSpatial.md),
[`permutations()`](https://YsoSirius.github.io/windfarmGA/reference/permutations.md),
[`readinteger()`](https://YsoSirius.github.io/windfarmGA/reference/readinteger.md),
[`readintegerSel()`](https://YsoSirius.github.io/windfarmGA/reference/readintegerSel.md),
[`splitAt()`](https://YsoSirius.github.io/windfarmGA/reference/splitAt.md),
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)

## Examples

``` r
# \donttest{
## Exemplary input Polygon with 2km x 2km:
library(sf)
Polygon1 <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(0, 0, 2000, 2000, 0),
    c(0, 2000, 2000, 0, 0)
  ))),
  crs = 3035
))

## Create a Grid
grid_area(Polygon1, 200, 1, TRUE)


grid_area(Polygon1, 400, 1, TRUE)



## Examplary irregular input Polygon
Polygon1 <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(0, 0, 2000, 3000, 0),
    c(20, 200, 2000, 0, 20)
  ))),
  crs = 3035
))

## Create a Grid
grid_area(Polygon1, 200, 1, TRUE)


grid_area(Polygon1, 200, 0.1, TRUE)


grid_area(Polygon1, 400, 1, TRUE)


grid_area(Polygon1, 400, 0.1, TRUE)


# }
```
