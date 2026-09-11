# Plot the result of a randomized output.

Plotting method for the results of
[`random_search_single`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)
and
[`random_search`](https://YsoSirius.github.io/windfarmGA/reference/random_search.md).

## Usage

``` r
plot_random_search(resultRS, result, area, best)
```

## Arguments

- resultRS:

  The result of the random functions
  [`random_search_single`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)
  and
  [`random_search`](https://YsoSirius.github.io/windfarmGA/reference/random_search.md).

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

- area:

  Site polygon (`sf`, SpatialPolygons, or coordinate matrix). Must be
  projected in metres.

- best:

  How many best candidates to plot. Default is 1.

## Value

Returns NULL. Used for plotting

## See also

Other Randomization:
[`random_search()`](https://YsoSirius.github.io/windfarmGA/reference/random_search.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
# \donttest{
library(sf)
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))

Res <- random_search(result = resultrect, area = area)
plot_random_search(resultRS = Res, result = resultrect, area = area, best = 2)




# }
```
