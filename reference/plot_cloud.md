# Plot outputs of all generations with standard deviations

Plot the fitness, efficiency and energy outputs of all generations and
the corresponding standard deviations.

## Usage

``` r
plot_cloud(result, pl = FALSE)
```

## Arguments

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- pl:

  Should the results be plotted? Default is FALSE

## Value

Returns a data.frame with the values for fitness, efficiency and energy
for all evaluated individuals

## See also

Other Plotting Functions:
[`plot_development()`](https://YsoSirius.github.io/windfarmGA/reference/plot_development.md),
[`plot_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_evolution.md),
[`plot_fitness_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_fitness_evolution.md),
[`plot_parkfitness()`](https://YsoSirius.github.io/windfarmGA/reference/plot_parkfitness.md),
[`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md),
[`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md),
[`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
# \donttest{
## Plot the results of a hexagonal grid optimization
plcdf <- plot_cloud(resulthex, TRUE)

# }
```
