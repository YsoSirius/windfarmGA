# Plot the evolution of fitness values

Plot the evolution of energy outputs and efficiency rates over the whole
generations. Plots min, mean and max values.

## Usage

``` r
plot_evolution(result, ask = TRUE, spar = 0.1)
```

## Arguments

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- ask:

  Should R wait for interaction for subsequent plotting. Default is TRUE

- spar:

  A numeric value determining how exact a spline should be drawn.
  Default is 0.1

## Value

Returns NULL. Used for plotting

## See also

Other Plotting Functions:
[`plot_cloud()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cloud.md),
[`plot_development()`](https://YsoSirius.github.io/windfarmGA/reference/plot_development.md),
[`plot_fitness_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_fitness_evolution.md),
[`plot_parkfitness()`](https://YsoSirius.github.io/windfarmGA/reference/plot_parkfitness.md),
[`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md),
[`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md),
[`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
# \donttest{
## Plot the results of a rectangular grid optimization
plot_evolution(resultrect, ask = TRUE, spar = 0.1)


#> NULL
# }
```
