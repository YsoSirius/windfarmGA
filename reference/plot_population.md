# Plot population size, cells and efficiency

Three pages: (1) individuals / parents / elites, (2) unique grid cells
this generation, in the elites, and ever probed, (3) park efficiency.
Each page waits for Enter in an interactive session. If the grid has
e.g. 70 cells and the whole population still touches every cell, "this
generation" and "ever probed" both sit at 70; the elite line is then the
one that shrinks toward the best sites.

## Usage

``` r
plot_population(result, interactive = NULL, ask = NULL)
```

## Arguments

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- interactive:

  Use plotly when `ask` is `FALSE` and plotly is installed.

- ask:

  If `TRUE`, wait for Enter between pages (Plots pane). Default is
  `TRUE` in an interactive session.

## Value

A plotly object, or a list of ggplots, invisibly. The census table is
attached as attribute `census`.

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
[`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md),
[`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md),
[`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md),
[`population_census()`](https://YsoSirius.github.io/windfarmGA/reference/population_census.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
# \donttest{
plot_population(resultrect)
#> Warning: plotly.js does not (yet) support horizontal legend items 
#> You can track progress here: 
#> https://github.com/plotly/plotly.js/issues/53 
#> Warning: plotly.js does not (yet) support horizontal legend items 
#> You can track progress here: 
#> https://github.com/plotly/plotly.js/issues/53 
#> Warning: plotly.js does not (yet) support horizontal legend items 
#> You can track progress here: 
#> https://github.com/plotly/plotly.js/issues/53 
# }
```
