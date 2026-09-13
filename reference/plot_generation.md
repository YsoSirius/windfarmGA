# Plot all layouts of one generation

Occupancy map of one generation, then the distinct layouts as small
maps. In an interactive session each page waits for Enter so you can
step through every distinct layout (`n_show` maps per page).
Non-interactive calls only draw the first `n_show` maps.

## Usage

``` r
plot_generation(
  result,
  area,
  generation = NULL,
  n_show = 6,
  interactive = NULL,
  ask = NULL
)
```

## Arguments

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- area:

  Site polygon (`sf`, SpatialPolygons, or coordinate matrix). Must be
  projected in metres.

- generation:

  Generation index (1 = first). Default is the last generation.

- n_show:

  Distinct layouts per page (and, if `ask` is `FALSE`, how many to draw
  in total). Default is 6. Set to 0 to skip the maps.

- interactive:

  Use plotly for the occupancy map when available.

- ask:

  If `TRUE`, wait for Enter and page through all distinct layouts.
  Default is `TRUE` in an interactive session.

## Value

The list from
[`generation_layouts`](https://YsoSirius.github.io/windfarmGA/reference/generation_layouts.md),
invisibly.

## See also

Other Plotting Functions:
[`generation_layouts()`](https://YsoSirius.github.io/windfarmGA/reference/generation_layouts.md),
[`plot_cell_heatmap()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cell_heatmap.md),
[`plot_cloud()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cloud.md),
[`plot_development()`](https://YsoSirius.github.io/windfarmGA/reference/plot_development.md),
[`plot_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_evolution.md),
[`plot_parkfitness()`](https://YsoSirius.github.io/windfarmGA/reference/plot_parkfitness.md),
[`plot_population()`](https://YsoSirius.github.io/windfarmGA/reference/plot_population.md),
[`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md),
[`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md),
[`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md),
[`population_census()`](https://YsoSirius.github.io/windfarmGA/reference/population_census.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
# \donttest{
plot_generation(resultrect, sp_polygon, generation = 10)

# }
```
