# Fitness and operator rates

Fitness (max / mean / min) and the three operator rates. Rates are
percentages: **Selection** = share of the population used as parents,
**Crossover inject** = unused cells mixed into children, **Mutation** =
chance that a turbine is swapped to a free cell. The legend sits outside
and follows the typical vertical order of the lines (selection highest,
then inject, mutation lowest). Plotly hover is used only when `ask` is
`FALSE` and plotly is installed.

## Usage

``` r
plot_parkfitness(result, spar = 0.1, interactive = NULL, ask = NULL)
```

## Arguments

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- spar:

  Unused, kept so existing calls do not break.

- interactive:

  Use plotly when `ask` is `FALSE` and plotly is installed.

- ask:

  If `TRUE`, wait for Enter between the fitness page and the rates page.
  Default is `TRUE` in an interactive session.

## Value

A plotly object, or a list of ggplots, invisibly.

## See also

Other Plotting Functions:
[`generation_layouts()`](https://YsoSirius.github.io/windfarmGA/reference/generation_layouts.md),
[`plot_cell_heatmap()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cell_heatmap.md),
[`plot_cloud()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cloud.md),
[`plot_development()`](https://YsoSirius.github.io/windfarmGA/reference/plot_development.md),
[`plot_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_evolution.md),
[`plot_generation()`](https://YsoSirius.github.io/windfarmGA/reference/plot_generation.md),
[`plot_population()`](https://YsoSirius.github.io/windfarmGA/reference/plot_population.md),
[`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md),
[`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md),
[`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md),
[`population_census()`](https://YsoSirius.github.io/windfarmGA/reference/population_census.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
# \donttest{
plot_parkfitness(resulthex)
# }
```
