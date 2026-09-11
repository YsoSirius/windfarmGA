# Randomize the output of the Genetic Algorithm

Perform a random search in the grid cells, to further optimize the
output of the wind farm layout.

## Usage

``` r
random_search(result, area, n = 20, best = 1, plot = FALSE, max_dist = 2.2)
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

- best:

  Which best individuals should be the starting conditions for a random
  search. The default is 1.

- plot:

  Draw the random-search layouts

- max_dist:

  A numeric value multiplied by the rotor radius to perform collision
  checks. Default is `2.2`

## Value

Returns a list.

## See also

Other Randomization:
[`plot_random_search()`](https://YsoSirius.github.io/windfarmGA/reference/plot_random_search.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
# \donttest{
new <- random_search(resultrect, sp_polygon, n = 20, best = 4)
plot_random_search(resultRS = new, result = resultrect, area = sp_polygon, best = 2)




# }
```
