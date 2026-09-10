# Polygon to Hexagonal Grids

The function takes a Simple Feature Polygon and a size argument and
creates a list with an indexed matrix with coordinates and a Simple
Feature object, that consists of hexagonal grids.

## Usage

``` r
hexa_area(shape, size = 500, plotGrid = FALSE)
```

## Arguments

- shape:

  Simple Feature Polygon of the considered area

- size:

  The cellsize of the grid in meters. Default is 500

- plotGrid:

  Logical value indicating whether the results should be plotted.
  Default is `FALSE`

## Value

Returns a list with 2 elements. List element 1 will have the grid cell
IDS, and the X and Y coordinates of the centers of each grid cell. List
element 2 is the grid as Simple Feature Polygons, which is used for
plotting purposes.

## See also

Other Helper Functions:
[`get_grids()`](https://YsoSirius.github.io/windfarmGA/reference/get_grids.md),
[`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md),
[`isSpatial()`](https://YsoSirius.github.io/windfarmGA/reference/isSpatial.md),
[`permutations()`](https://YsoSirius.github.io/windfarmGA/reference/permutations.md),
[`readinteger()`](https://YsoSirius.github.io/windfarmGA/reference/readinteger.md),
[`readintegerSel()`](https://YsoSirius.github.io/windfarmGA/reference/readintegerSel.md),
[`splitAt()`](https://YsoSirius.github.io/windfarmGA/reference/splitAt.md),
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)

## Examples

``` r
library(sf)
## Exemplary input Polygon with 2km x 2km:
Poly <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))
HexGrid <- hexa_area(Poly, 100, TRUE)


```
