# Randomize the output of the Genetic Algorithm

Jitter the best GA layouts inside their grid cells and re-evaluate
energy. Use this as a short post-search after
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md).
Terrain and Weibull follow the GA flags when `terrain` / `weibull` are
`NULL`. Terrain rasters from the GA are reused when stored in `result`;
pass a DEM to rebuild. Weibull rasters are not stored; pass
`weibull_src` again if needed.

## Usage

``` r
random_search(
  result,
  area,
  runs = 20,
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

- runs:

  How many jittered layouts to try per `best` start. Default is 20.

- best:

  How many distinct best layouts to refine. Default is 1.

- plot:

  Draw the random-search layouts

- max_dist:

  A numeric value multiplied by the rotor radius to perform collision
  checks. Default is `2.2`

- terrain:

  `NULL` (default) follows the GA and reuses `result$terrainModel`.
  `TRUE` downloads only if nothing is stored. A DEM rebuilds the model.
  `FALSE` skips terrain.

- weibull:

  `NULL` follows the GA flag. Weibull rasters are not stored; pass
  `weibull_src` (or a speed raster as `weibull`) again. Giving
  `weibull_src` is enough; you do not also need `weibull = TRUE`.

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
new <- random_search(resultrect, sp_polygon, runs = 20, best = 4)
plot_random_search(resultRS = new, result = resultrect, area = sp_polygon, best = 2)


# }
```
