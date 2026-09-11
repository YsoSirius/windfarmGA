# Plot the best results

Draw the best layout(s) on the site. Turbine labels are the total wake
in percent. Default is the single best energy layout.

## Usage

``` r
plot_result(
  result,
  area,
  best = 1,
  plot_en = 1,
  terrain = FALSE,
  plot_grid = TRUE,
  ccl_roughness = NULL,
  ccl = NULL,
  weibull_src = NULL
)
```

## Arguments

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- area:

  Site polygon (`sf`, SpatialPolygons, or coordinate matrix). Must be
  projected in metres.

- best:

  How many distinct best layouts to draw. Default is 1.

- plot_en:

  A numeric value that indicates if the best energy or efficiency output
  should be plotted. `1` plots the best energy solutions and `2` plots
  the best efficiency solutions

- terrain:

  Draw terrain rasters for the best layout

- plot_grid:

  If `TRUE` (default) the used grid is added. You can also pass another
  Simple Feature object

- ccl_roughness:

  Path to the CLC legend CSV (`Rauhigkeit_z` column).

- ccl:

  Path to a Corine Land Cover raster when `terrain` is on.

- weibull_src:

  `list(k, a)` shape and scale rasters (e.g. Global Wind Atlas
  `combined-Weibull-k` / `combined-Weibull-A`).

## Value

Returns a data.frame of the best (energy/efficiency) individual during
all iterations

## See also

Other Plotting Functions:
[`generation_layouts()`](https://YsoSirius.github.io/windfarmGA/reference/generation_layouts.md),
[`plot_cell_heatmap()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cell_heatmap.md),
[`plot_cloud()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cloud.md),
[`plot_development()`](https://YsoSirius.github.io/windfarmGA/reference/plot_development.md),
[`plot_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_evolution.md),
[`plot_fitness_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_fitness_evolution.md),
[`plot_generation()`](https://YsoSirius.github.io/windfarmGA/reference/plot_generation.md),
[`plot_parkfitness()`](https://YsoSirius.github.io/windfarmGA/reference/plot_parkfitness.md),
[`plot_population()`](https://YsoSirius.github.io/windfarmGA/reference/plot_population.md),
[`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md),
[`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md),
[`population_census()`](https://YsoSirius.github.io/windfarmGA/reference/population_census.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## Add some data examples from the package
library(sf)
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))

## Plot the results of a hexagonal grid optimization
plot_result(resulthex, area, best = 1, plot_en = 1, terrain = FALSE)

## Plot the results of a rectangular grid optimization
plot_result(resultrect, area, best = 1, plot_en = 1, terrain = FALSE)
} # }
```
