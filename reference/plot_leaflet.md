# Plot a wind warm with leaflet

Plot a resulting wind farm on a leaflet map.

## Usage

``` r
plot_leaflet(result, area, which = 1, orderitems = TRUE, grid = NULL)
```

## Arguments

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- area:

  Site polygon (`sf`, SpatialPolygons, or coordinate matrix). Must be
  projected in metres.

- which:

  A numeric value, indicating which individual to plot. The default
  is 1. Combined with `orderitems = TRUE` this will show the best
  performing wind farm.

- orderitems:

  A logical value indicating whether the results should be ordered by
  energy values `TRUE` or chronologically `FALSE`

- grid:

  Optional grid polygons. By default they are rebuilt from `result` and
  `area`. You can pass the polygon element of
  [`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md)
  or
  [`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md).

## Value

Returns a leaflet map.

## Examples

``` r
if (FALSE) { # \dontrun{
## Plot the best wind farm on a leaflet map (ordered by energy values)
plot_leaflet(result = resulthex, area = sp_polygon, which = 1)

## Plot the last wind farm (ordered by chronology).
plot_leaflet(
  result = resulthex, area = sp_polygon, orderitems = FALSE,
  which = 1
)

## Plot the best wind farm on a leaflet map with the rectangular Grid
Grid <- grid_area(sp_polygon, size = 150, prop = 0.4)
plot_leaflet(
  result = resultrect, area = sp_polygon, which = 1,
  grid = Grid[[2]]
)

## Plot the last wind farm with hexagonal Grid
Grid <- hexa_area(sp_polygon, size = 75)
plot_leaflet(
  result = resulthex, area = sp_polygon, which = 1,
  grid = Grid[[2]]
)
} # }
```
