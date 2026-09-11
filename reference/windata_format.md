# Transform Winddata

Helper Function, which transforms winddata to an acceptable format

## Usage

``` r
windata_format(df)
```

## Arguments

- df:

  The wind data with speeds, direction and optionally a probability
  column. If not assigned, it will be calculated

## Value

A list of windspeed and probabilities

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
[`wind_from_uv()`](https://YsoSirius.github.io/windfarmGA/reference/wind_from_uv.md)

## Examples

``` r
# \donttest{
wind_df <- data.frame(
  ws = c(12, 30, 45),
  wd = c(0, 90, 150),
  probab = 30:32
)
windata_format(wind_df)
#> [[1]]
#>   ws  wd   probab
#> 1 12   0 32.25806
#> 2 30  90 33.33333
#> 3 45 150 34.40860
#> 
#> [[2]]
#> [1] 32.25806 33.33333 34.40860
#> 

wind_df <- data.frame(
  speed = c(12, 30, 45),
  direction = c(90, 90, 150),
  probab = c(10, 20, 60)
)
windata_format(wind_df)
#> [[1]]
#>   ws  wd   probab
#> 1 24  90 21.73964
#> 3 45 150 78.26036
#> 
#> [[2]]
#> [1] 21.73964 78.26036
#> 

wind_df <- data.frame(
  speed = c(12, 30, 45),
  direction = c(400, 90, 150)
)
windata_format(wind_df)
#> [[1]]
#>   ws  wd   probab
#> 2 30  90 33.33333
#> 3 45 150 33.33333
#> 1 12  40 33.33333
#> 
#> [[2]]
#> [1] 33.33333 33.33333 33.33333
#> 
# }
```
