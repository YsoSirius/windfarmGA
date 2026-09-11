# Wind rose from u/v components

Bin ERA5-style eastward (`u`) and northward (`v`) wind components into
the `ws` / `wd` / `probab` table that
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)
and
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)
expect. Direction is meteorological (where the wind comes from; 0 =
north). Each direction bin gets the mean speed and the hour share.

## Usage

``` r
wind_from_uv(u, v, dir_width = 30)
```

## Arguments

- u:

  Eastward wind component (m/s). Same length as `v`.

- v:

  Northward wind component (m/s).

- dir_width:

  Width of direction bins in degrees. Default is 30.

## Value

A data.frame with `ws`, `wd`, `probab` (probabilities sum to 100).

## See also

Other Helper Functions:
[`get_grids()`](https://YsoSirius.github.io/windfarmGA/reference/get_grids.md),
[`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md),
[`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md),
[`isSpatial()`](https://YsoSirius.github.io/windfarmGA/reference/isSpatial.md),
[`permutations()`](https://YsoSirius.github.io/windfarmGA/reference/permutations.md),
[`read_power_curve()`](https://YsoSirius.github.io/windfarmGA/reference/read_power_curve.md),
[`splitAt()`](https://YsoSirius.github.io/windfarmGA/reference/splitAt.md),
[`wind_from_series()`](https://YsoSirius.github.io/windfarmGA/reference/wind_from_series.md),
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)

## Examples

``` r
set.seed(1)
u <- rnorm(200, 2)
v <- rnorm(200, -3)
wind_from_uv(u, v, dir_width = 30)
#>         ws  wd probab
#> 1 4.375479  15    0.5
#> 2 3.610674 285    6.0
#> 3 3.718157 315   58.0
#> 4 3.706485 345   35.5
```
