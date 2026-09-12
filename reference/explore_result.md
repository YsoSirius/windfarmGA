# Shiny explorer for a GA result

One-page viewer for a
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)
result: Leaflet map of that generation's best layout (downwind wake
cones, optional terrain layers), a wind rose, and one plotly figure with
fitness, operator rates and population. New fitness maxima are marked;
click a marker to jump to that generation. The cell heatmap is not
included: use
[`plot_cell_heatmap()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cell_heatmap.md)
or
[`plot_generation()`](https://YsoSirius.github.io/windfarmGA/reference/plot_generation.md)
offline. Requires Suggests `shiny`. Plotly is used when installed.

## Usage

``` r
explore_result(result, area)
```

## Arguments

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- area:

  Site polygon (`sf`, SpatialPolygons, or coordinate matrix). Must be
  projected in metres.

## Value

The Shiny app object, invisibly. Called for its side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
explore_result(resultrect, sp_polygon)
} # }
```
