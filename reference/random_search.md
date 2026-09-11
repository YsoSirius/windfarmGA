# Randomize the output of the Genetic Algorithm

Jitter the best GA layouts inside their grid cells and re-evaluate
energy. Use this as a short post-search after
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md).
Terrain and Weibull follow the GA flags when `terrain` / `weibull` are
`NULL`; pass `weibull_src` again because rasters are not stored in
`result`.

## Usage

``` r
random_search(
  result,
  area,
  n = 20,
  best = 1,
  plot = FALSE,
  max_dist = 2.2,
  terrain = NULL,
  weibull = NULL,
  weibull_src = NULL,
  ccl = NULL,
  ccl_roughness = NULL
)
```

## Arguments

- result:

  The resulting matrix of the function
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- area:

  Site polygon (`sf`, SpatialPolygons, or coordinate matrix). Must be
  projected in metres.

- n:

  The number of random searches to be performed. Default is 20.

- best:

  Which best individuals should be the starting conditions for a random
  search. The default is 1.

- plot:

  Draw the random-search layouts

- max_dist:

  A numeric value multiplied by the rotor radius to perform collision
  checks. Default is `2.2`

- terrain:

  `NULL` follows the GA `Topographie` flag. `TRUE` (or a DEM raster)
  rebuilds elevation + land cover via
  [`terrain_model()`](https://YsoSirius.github.io/windfarmGA/reference/terrain_model.md).
  `FALSE` skips terrain even if the GA used it.

- weibull:

  `NULL` follows the GA `Active Weibull` flag. A speed raster is used
  as-is. `TRUE` needs `weibull_src`. The GA does not store rasters in
  `result`.

- weibull_src:

  `list(k, a)` shape and scale rasters (e.g. Global Wind Atlas
  `combined-Weibull-k` / `combined-Weibull-A`).

- ccl:

  Path to a Corine Land Cover raster when `terrain` is on.

- ccl_roughness:

  Path to the CLC legend CSV (`Rauhigkeit_z` column).

## Value

Returns a list.

## See also

Other Randomization:
[`plot_random_search()`](https://YsoSirius.github.io/windfarmGA/reference/plot_random_search.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
# \donttest{
new <- random_search(resultrect, sp_polygon, n = 20, best = 4)
plot_random_search(resultRS = new, result = resultrect, area = sp_polygon, best = 2)




# }
```
