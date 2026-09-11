# Read a manufacturer or NREL/IEA power-curve table

Parse a CSV (or data.frame) with wind speed and power in kW. Understands
NREL/IEA headers such as `Wind Speed [m/s]` and `Power [kW]`. Optional
`Ct` is returned as an attribute. Does not ship manufacturer curves;
pass your own file or an open IEA/NREL reference CSV. See
`experimental/climate_helpers.R`.

## Usage

``` r
read_power_curve(file)
```

## Arguments

- file:

  Path to a CSV, or a data.frame.

## Value

A data.frame with `ws` and `power`. Attribute `ct` is a matching
data.frame when a thrust column is present.

## See also

Other Helper Functions:
[`get_grids()`](https://YsoSirius.github.io/windfarmGA/reference/get_grids.md),
[`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md),
[`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md),
[`isSpatial()`](https://YsoSirius.github.io/windfarmGA/reference/isSpatial.md),
[`permutations()`](https://YsoSirius.github.io/windfarmGA/reference/permutations.md),
[`splitAt()`](https://YsoSirius.github.io/windfarmGA/reference/splitAt.md),
[`wind_from_series()`](https://YsoSirius.github.io/windfarmGA/reference/wind_from_series.md),
[`wind_from_uv()`](https://YsoSirius.github.io/windfarmGA/reference/wind_from_uv.md),
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)

## Examples

``` r
curve <- data.frame(
  `Wind Speed [m/s]` = c(3, 8, 12, 25),
  `Power [kW]` = c(0, 1200, 2000, 2000),
  check.names = FALSE
)
read_power_curve(curve)
#>   ws power
#> 1  3     0
#> 2  8  1200
#> 3 12  2000
#> 4 25  2000
```
