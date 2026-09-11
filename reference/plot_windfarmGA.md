# Plot the results of an optimization run

Draw the useful summary plots of a GA run, one after another: best
layout, fitness, operator rates, population, cells, efficiency, and the
cell heatmap. In an interactive session every page waits for Enter so
nothing is overwritten in the Plots pane.

## Usage

``` r
plot_windfarmGA(
  result,
  area,
  which_plot = "all",
  best = 1,
  plot_en = 1,
  weibull_src = NULL,
  ask = NULL,
  plotly = NULL
)
```

## Arguments

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- area:

  Site polygon (`sf`, SpatialPolygons, or coordinate matrix). Must be
  projected in metres.

- which_plot:

  `"all"` (default) shows `result`, `progress`, `population` and
  `heatmap`. Or a character vector (`"result"`, `"progress"`,
  `"population"`, `"heatmap"`, `"evolution"`) or the numbers 1-4.

- best:

  How many distinct best layouts to draw. Default is 1.

- plot_en:

  A numeric value that indicates if the best energy or efficiency output
  should be plotted. `1` plots the best energy solutions and `2` plots
  the best efficiency solutions

- weibull_src:

  `list(k, a)` shape and scale rasters (e.g. Global Wind Atlas
  `combined-Weibull-k` / `combined-Weibull-A`).

- ask:

  If `TRUE`, wait for Enter between pages. Default is `TRUE` in an
  interactive session.

- plotly:

  If `TRUE`, draw fitness and rates with plotly (hover). Used only when
  `ask` is `FALSE` and plotly is installed.

## Value

Returns NULL. Used for plotting

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
[`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md),
[`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md),
[`population_census()`](https://YsoSirius.github.io/windfarmGA/reference/population_census.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))

plot_windfarmGA(resulthex, area)
plot_windfarmGA(resultrect, area, which_plot = "progress")
} # }
```
