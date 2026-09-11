# Wind rose from speed and direction series

Bin a time series of hub-height speed and meteorological direction into
`ws` / `wd` / `probab`.

## Usage

``` r
wind_from_series(ws, wd, dir_width = 30)
```

## Arguments

- ws:

  Wind speed (m/s).

- wd:

  Direction in degrees (0 = north, clockwise, where the wind comes
  from).

- dir_width:

  Width of direction bins in degrees. Default is 30.

## Value

A data.frame with `ws`, `wd`, `probab`.

## See also

Other Helper Functions:
[`get_grids()`](https://YsoSirius.github.io/windfarmGA/reference/get_grids.md),
[`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md),
[`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md),
[`isSpatial()`](https://YsoSirius.github.io/windfarmGA/reference/isSpatial.md),
[`permutations()`](https://YsoSirius.github.io/windfarmGA/reference/permutations.md),
[`read_power_curve()`](https://YsoSirius.github.io/windfarmGA/reference/read_power_curve.md),
[`splitAt()`](https://YsoSirius.github.io/windfarmGA/reference/splitAt.md),
[`wind_from_uv()`](https://YsoSirius.github.io/windfarmGA/reference/wind_from_uv.md),
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)
