# Plot the result of a randomized output.

Plotting method for the results of
[`random_search_single`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)
and
[`random_search`](https://YsoSirius.github.io/windfarmGA/reference/random_search.md).

## Usage

``` r
plot_random_search(resultRS, result, Polygon1, best)
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

- Polygon1:

  The considered area as SpatialPolygon, SimpleFeature Polygon or
  coordinates as matrix/data.frame

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
Polygon1 <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))

Res <- random_search(result = resultrect, Polygon1 = Polygon1)
plot_random_search(resultRS = Res, result = resultrect, Polygon1 = Polygon1, best = 2)




# }
```
