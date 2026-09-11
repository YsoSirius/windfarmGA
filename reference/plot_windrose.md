# Plot a Windrose

Plot a wind rose of the wind data frame.

## Usage

``` r
plot_windrose(
  data,
  spd,
  dir,
  spdres = 2,
  dirres = 10,
  spdmin = 1,
  spdmax = 30,
  palette = "YlGnBu",
  spdseq = NULL,
  plot = TRUE
)
```

## Arguments

- data:

  A data.frame containing the wind information

- spd:

  The column of the wind speeds in "data"

- dir:

  The column of the wind directions in "data"

- spdres:

  The increment of the wind speed legend. Default is 2

- dirres:

  The size of the wind sectors. Default is 10

- spdmin:

  Minimum wind speed. Default is 1

- spdmax:

  Maximal wind speed. Default is 30

- palette:

  A color palette used for drawing the wind rose

- spdseq:

  A wind speed sequence, that is used for plotting

- plot:

  Deprecated alias for `plot`.

## Value

A ggplot2 wind rose plot, returned invisibly.

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
[`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md),
[`population_census()`](https://YsoSirius.github.io/windfarmGA/reference/population_census.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
## Exemplary Input Wind speed and direction data frame
# Uniform wind speed and single wind direction
data.in <- data.frame(ws = 12, wd = 0)
windrosePlot <- plot_windrose(
  data = data.in, spd = data.in$ws,
  dir = data.in$wd
)


# Random wind speeds and random wind directions
data.in <- data.frame(
  ws = sample(1:25, 10),
  wd = sample(1:260, 10)
)
windrosePlot <- plot_windrose(
  data = data.in, spd = data.in$ws,
  dir = data.in$wd
)

```
