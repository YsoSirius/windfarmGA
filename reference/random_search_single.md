# Randomize the location of a single turbine

Perform a random search for a single turbine, to further optimize the
output of the wind farm layout.

## Usage

``` r
random_search_single(result, Polygon1, n = 20, Plot = FALSE, max_dist = 2.2)
```

## Arguments

- result:

  The resulting matrix of the function
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- Polygon1:

  The considered area as SpatialPolygon, SimpleFeature Polygon or
  coordinates as matrix/data.frame

- n:

  The number of random searches to be performed. Default is 20.

- Plot:

  Should the random search be plotted? Default is `FALSE`

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
[`plot_cloud()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cloud.md),
[`plot_development()`](https://YsoSirius.github.io/windfarmGA/reference/plot_development.md),
[`plot_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_evolution.md),
[`plot_fitness_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_fitness_evolution.md),
[`plot_parkfitness()`](https://YsoSirius.github.io/windfarmGA/reference/plot_parkfitness.md),
[`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md),
[`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md),
[`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md)
