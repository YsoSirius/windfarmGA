# Randomize the location of a single turbine

Perform a random search for a single turbine, to further optimize the
output of the wind farm layout.

## Usage

``` r
random_search_single(result, area, n = 20, plot = FALSE, max_dist = 2.2)
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

- plot:

  Draw the random-search layouts

- max_dist:

  A numeric value multiplied by the rotor radius to perform collision
  checks. Default is 2.2

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
[`plot_fitness_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_fitness_evolution.md),
[`plot_generation()`](https://YsoSirius.github.io/windfarmGA/reference/plot_generation.md),
[`plot_parkfitness()`](https://YsoSirius.github.io/windfarmGA/reference/plot_parkfitness.md),
[`plot_population()`](https://YsoSirius.github.io/windfarmGA/reference/plot_population.md),
[`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md),
[`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md),
[`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md),
[`population_census()`](https://YsoSirius.github.io/windfarmGA/reference/population_census.md)
