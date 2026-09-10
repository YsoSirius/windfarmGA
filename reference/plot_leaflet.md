# Plot a wind warm with leaflet

Plot a resulting wind farm on a leaflet map.

## Usage

``` r
plot_leaflet(result, Polygon1, which = 1, orderitems = TRUE, GridPol)
```

## Arguments

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- Polygon1:

  The considered area as SpatialPolygon, SimpleFeature Polygon or
  coordinates as matrix/data.frame

- which:

  A numeric value, indicating which individual to plot. The default
  is 1. Combined with `orderitems = TRUE` this will show the best
  performing wind farm.

- orderitems:

  A logical value indicating whether the results should be ordered by
  energy values `TRUE` or chronologically `FALSE`

- GridPol:

  By default, the grid will be calculated based on the inputs of
  `result` and the `Polygon1`. But another spatial object or the output
  of the
  [`grid_area`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md)
  or
  [`hexa_area`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md)
  functions can also be

## Value

Returns a leaflet map.

## Examples

``` r
if (FALSE) { # \dontrun{
## Plot the best wind farm on a leaflet map (ordered by energy values)
plot_leaflet(result = resulthex, Polygon1 = sp_polygon, which = 1)

## Plot the last wind farm (ordered by chronology).
plot_leaflet(
  result = resulthex, Polygon1 = sp_polygon, orderitems = FALSE,
  which = 1
)

## Plot the best wind farm on a leaflet map with the rectangular Grid
Grid <- grid_area(sp_polygon, size = 150, prop = 0.4)
plot_leaflet(
  result = resultrect, Polygon1 = sp_polygon, which = 1,
  GridPol = Grid[[2]]
)

## Plot the last wind farm with hexagonal Grid
Grid <- hexa_area(sp_polygon, size = 75)
plot_leaflet(
  result = resulthex, Polygon1 = sp_polygon, which = 1,
  GridPol = Grid[[2]]
)
} # }
```
