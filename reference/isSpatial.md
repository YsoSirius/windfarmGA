# Transform to Simple Feature Polygons

Helper Function, which transforms SpatialPolygons or coordinates in
matrix/data.frame - form to a Simple Feature Polygon

## Usage

``` r
isSpatial(area, crs = NULL)
```

## Arguments

- area:

  Site as SpatialPolygon, Simple Feature polygon, or coordinates as
  matrix/data.frame

- crs:

  CRS assigned to matrix / data.frame coordinates

## Value

A Simple Feature Polygon

## Details

If the columns are named, it will look for common abbreviation to match
x/y or long/lat columns. If the columns are not named, the first 2
numeric columns are taken.

## See also

Other Helper Functions:
[`get_grids()`](https://YsoSirius.github.io/windfarmGA/reference/get_grids.md),
[`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md),
[`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md),
[`permutations()`](https://YsoSirius.github.io/windfarmGA/reference/permutations.md),
[`read_power_curve()`](https://YsoSirius.github.io/windfarmGA/reference/read_power_curve.md),
[`splitAt()`](https://YsoSirius.github.io/windfarmGA/reference/splitAt.md),
[`wind_from_series()`](https://YsoSirius.github.io/windfarmGA/reference/wind_from_series.md),
[`wind_from_uv()`](https://YsoSirius.github.io/windfarmGA/reference/wind_from_uv.md),
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)

## Examples

``` r
# \donttest{
library(sf)
df <- rbind(
  c(4498482, 2668272), c(4498482, 2669343),
  c(4499991, 2669343), c(4499991, 2668272)
)
isSpatial(df)
#> Geometry set for 1 feature 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 4498482 ymin: 2668272 xmax: 4499991 ymax: 2669343
#> CRS:           NA
#> POLYGON ((4498482 2668272, 4498482 2669343, 449...

area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))
isSpatial(st_coordinates(area), 3035)
#> Geometry set for 1 feature 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 4498482 ymin: 2668272 xmax: 4499991 ymax: 2669343
#> Projected CRS: ETRS89-extended / LAEA Europe
#> POLYGON ((4498482 2668272, 4498482 2669343, 449...
# }
```
