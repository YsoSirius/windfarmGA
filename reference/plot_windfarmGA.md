# Plot the results of an optimization run

Plot the results of a genetic algorithm run with given inputs. Several
plots try to show all relevant effects and outcomes of the algorithm. 6
plot methods are available that can be selected individually.

## Usage

``` r
plot_windfarmGA(
  result,
  Polygon1,
  whichPl = "all",
  best = 1,
  plotEn = 1,
  weibullsrc
)
```

## Arguments

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- Polygon1:

  The considered area as SpatialPolygon, SimpleFeature Polygon or
  coordinates as matrix/data.frame

- whichPl:

  Which plots should be shown: 1-6 are possible. The default is "all"
  which shows all available plots

- best:

  A numeric value indicating how many of the best individuals should be
  plotted

- plotEn:

  A numeric value that indicates if the best energy or efficiency output
  should be plotted. `1` plots the best energy solutions and `2` plots
  the best efficiency solutions

- weibullsrc:

  A list of Weibull parameter rasters, where the first list item must be
  the shape parameter raster `k` and the second item must be the scale
  parameter raster `a` of the Weibull distribution. If no list is given,
  then rasters included in the package are used instead, which currently
  only cover Austria. This variable is only used if `weibull = TRUE`.

## Value

Returns NULL. Used for plotting

## See also

Other Plotting Functions:
[`plot_cloud()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cloud.md),
[`plot_development()`](https://YsoSirius.github.io/windfarmGA/reference/plot_development.md),
[`plot_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_evolution.md),
[`plot_fitness_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_fitness_evolution.md),
[`plot_parkfitness()`](https://YsoSirius.github.io/windfarmGA/reference/plot_parkfitness.md),
[`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md),
[`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
Polygon1 <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))

## Plot the results of a hexagonal grid optimization
plot_windfarmGA(resulthex, Polygon1, whichPl = "all", best = 1, plotEn = 1)

## Plot the results of a rectangular grid optimization
plot_windfarmGA(resultrect, Polygon1, whichPl = "all", best = 1, plotEn = 1)
} # }
```
