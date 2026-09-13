# Randomize the location of a single turbine

Perform a random search for a single turbine, to further optimize the
output of the wind farm layout.

## Usage

``` r
random_search_single(
  result,
  area,
  runs = 20,
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

- plot:

  Draw the random-search layouts

- max_dist:

  A numeric value multiplied by the rotor radius to perform collision
  checks. Default is 2.2

- terrain:

  `NULL` (default) follows the GA and reuses `result$terrainModel`.
  `TRUE` downloads only if nothing is stored. A DEM rebuilds the model.
  `FALSE` skips terrain.

- weibull:

  `NULL` follows the GA flag. Weibull rasters are not stored — pass
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

Returns a list

## See also

Other Randomization:
[`plot_random_search()`](https://YsoSirius.github.io/windfarmGA/reference/plot_random_search.md),
[`random_search()`](https://YsoSirius.github.io/windfarmGA/reference/random_search.md)

Other Plotting Functions:
[`generation_layouts()`](https://YsoSirius.github.io/windfarmGA/reference/generation_layouts.md),
[`plot_cell_heatmap()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cell_heatmap.md),
[`plot_cloud()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cloud.md),
[`plot_development()`](https://YsoSirius.github.io/windfarmGA/reference/plot_development.md),
[`plot_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_evolution.md),
[`plot_generation()`](https://YsoSirius.github.io/windfarmGA/reference/plot_generation.md),
[`plot_parkfitness()`](https://YsoSirius.github.io/windfarmGA/reference/plot_parkfitness.md),
[`plot_population()`](https://YsoSirius.github.io/windfarmGA/reference/plot_population.md),
[`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md),
[`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md),
[`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md),
[`population_census()`](https://YsoSirius.github.io/windfarmGA/reference/population_census.md)
