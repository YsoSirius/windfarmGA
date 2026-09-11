# Map layouts to grid coordinates

Map a population of layouts to grid coordinates. Accepts either an
integer matrix of unique cell IDs (`n` turbines × individuals) or a
legacy binary matrix (`n_gridcells` × individuals).

## Usage

``` r
get_grids(layouts, grid)
```

## Arguments

- layouts:

  Binary matrix (legacy) or integer matrix of grid IDs (`n` turbines ×
  individuals)

- grid:

  Indexed grid from
  [`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md)

## Value

Returns a list of all individuals with X and Y coordinates and the grid
cell ID.

## See also

Other Helper Functions:
[`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md),
[`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md),
[`isSpatial()`](https://YsoSirius.github.io/windfarmGA/reference/isSpatial.md),
[`permutations()`](https://YsoSirius.github.io/windfarmGA/reference/permutations.md),
[`read_power_curve()`](https://YsoSirius.github.io/windfarmGA/reference/read_power_curve.md),
[`splitAt()`](https://YsoSirius.github.io/windfarmGA/reference/splitAt.md),
[`wind_from_series()`](https://YsoSirius.github.io/windfarmGA/reference/wind_from_series.md),
[`wind_from_uv()`](https://YsoSirius.github.io/windfarmGA/reference/wind_from_uv.md),
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)

## Examples

``` r
# \donttest{
## Create a random rectangular shapefile
library(sf)
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(0, 0, 2000, 2000, 0),
    c(0, 2000, 2000, 0, 0)
  ))),
  crs = 3035
))

## Calculate a Grid and an indexed data.frame with coordinates and
## grid cell Ids.
Grid1 <- grid_area(area = area, size = 200, prop = 1)
Grid <- Grid1[[1]]

startsel <- init_population(Grid, 10, 20)
wind <- data.frame(ws = 12, wd = 0)
wind <- list(wind, probab = 100)
fit <- fitness(
  population = startsel, reference_height = 100, rotor_height = 100,
  surface_roughness = 0.3, area = area, rotor = 20,
  wind = wind, terrain = FALSE
)
allparks <- do.call("rbind", fit)

## SELECTION (n unique cell IDs per individual)
selec6best <- selection(fit, Grid, 2, TRUE, 6, "VAR")

## Set-crossover and swap-mutation keep exactly n turbines.
cross_ids <- set_crossover(selec6best[[1]], Grid[, "ID"], uplimit = 20)
mut_ids <- swap_mutation(cross_ids, Grid[, "ID"], p = 0.2)

## Look up XY coordinates for the next fitness evaluation.
getRectV <- get_grids(mut_ids, Grid)
fit <- fitness(
  population = getRectV, reference_height = 100, rotor_height = 100,
  surface_roughness = 0.3, area = area, rotor = 20,
  wind = wind, terrain = FALSE
)
head(fit)
#> $`1,26,28,30,24,84,52,2,67,6`
#>          X    Y EfficAllDir EnergyOverall AbschGesamt Run RotorR Rect_ID
#>  [1,]  100  100    95.70101      5727.813           0   1     20       1
#>  [2,] 1100  500    95.70101      5727.813           0   1     20      26
#>  [3,] 1500  500    95.70101      5727.813           0   1     20      28
#>  [4,] 1900  500    95.70101      5727.813           0   1     20      30
#>  [5,]  700  500    95.70101      5727.813         100   1     20      24
#>  [6,]  700 1700    95.70101      5727.813           0   1     20      84
#>  [7,]  300 1100    95.70101      5727.813           0   1     20      52
#>  [8,]  300  100    95.70101      5727.813         100   1     20       2
#>  [9,] 1300 1300    95.70101      5727.813           0   1     20      67
#> [10,] 1100  100    95.70101      5727.813         100   1     20       6
#>       Parkfitness
#>  [1,]    5481.575
#>  [2,]    5481.575
#>  [3,]    5481.575
#>  [4,]    5481.575
#>  [5,]    5481.575
#>  [6,]    5481.575
#>  [7,]    5481.575
#>  [8,]    5481.575
#>  [9,]    5481.575
#> [10,]    5481.575
#> 
#> $`3,5,29,93,34,52,57,71,98,99`
#>          X    Y EfficAllDir EnergyOverall AbschGesamt Run RotorR Rect_ID
#>  [1,]  500  100    99.18348      5936.242         100   2     20       3
#>  [2,]  900  100    99.18348      5936.242           0   2     20       5
#>  [3,] 1700  500    99.18348      5936.242         100   2     20      29
#>  [4,]  500 1900    99.18348      5936.242           0   2     20      93
#>  [5,]  700  700    99.18348      5936.242           0   2     20      34
#>  [6,]  300 1100    99.18348      5936.242           0   2     20      52
#>  [7,] 1300 1100    99.18348      5936.242           0   2     20      57
#>  [8,]  100 1500    99.18348      5936.242           0   2     20      71
#>  [9,] 1500 1900    99.18348      5936.242           0   2     20      98
#> [10,] 1700 1900    99.18348      5936.242           0   2     20      99
#>       Parkfitness
#>  [1,]    5887.771
#>  [2,]    5887.771
#>  [3,]    5887.771
#>  [4,]    5887.771
#>  [5,]    5887.771
#>  [6,]    5887.771
#>  [7,]    5887.771
#>  [8,]    5887.771
#>  [9,]    5887.771
#> [10,]    5887.771
#> 
#> $`73,16,19,30,36,65,68,70,81,92`
#>          X    Y EfficAllDir EnergyOverall AbschGesamt Run RotorR Rect_ID
#>  [1,]  500 1500     96.0033      5745.905           0   3     20      73
#>  [2,] 1100  300     96.0033      5745.905         100   3     20      16
#>  [3,] 1700  300     96.0033      5745.905           0   3     20      19
#>  [4,] 1900  500     96.0033      5745.905         100   3     20      30
#>  [5,] 1100  700     96.0033      5745.905           0   3     20      36
#>  [6,]  900 1300     96.0033      5745.905           0   3     20      65
#>  [7,] 1500 1300     96.0033      5745.905           0   3     20      68
#>  [8,] 1900 1300     96.0033      5745.905           0   3     20      70
#>  [9,]  100 1700     96.0033      5745.905           0   3     20      81
#> [10,]  300 1900     96.0033      5745.905           0   3     20      92
#>       Parkfitness
#>  [1,]    5516.259
#>  [2,]    5516.259
#>  [3,]    5516.259
#>  [4,]    5516.259
#>  [5,]    5516.259
#>  [6,]    5516.259
#>  [7,]    5516.259
#>  [8,]    5516.259
#>  [9,]    5516.259
#> [10,]    5516.259
#> 
#> $`16,26,30,42,46,51,27,6,92,31`
#>          X    Y EfficAllDir EnergyOverall AbschGesamt Run RotorR Rect_ID
#>  [1,] 1100  300       82.59      4943.104         200   4     20      16
#>  [2,] 1100  500       82.59      4943.104         100   4     20      26
#>  [3,] 1900  500       82.59      4943.104           0   4     20      30
#>  [4,]  300  900       82.59      4943.104         100   4     20      42
#>  [5,] 1100  900       82.59      4943.104           0   4     20      46
#>  [6,]  100 1100       82.59      4943.104           0   4     20      51
#>  [7,] 1300  500       82.59      4943.104           0   4     20      27
#>  [8,] 1100  100       82.59      4943.104         300   4     20       6
#>  [9,]  300 1900       82.59      4943.104           0   4     20      92
#> [10,]  100  700       82.59      4943.104         100   4     20      31
#>       Parkfitness
#>  [1,]    4082.509
#>  [2,]    4082.509
#>  [3,]    4082.509
#>  [4,]    4082.509
#>  [5,]    4082.509
#>  [6,]    4082.509
#>  [7,]    4082.509
#>  [8,]    4082.509
#>  [9,]    4082.509
#> [10,]    4082.509
#> 
#> $`67,46,49,63,58,66,22,87,89,90`
#>          X    Y EfficAllDir EnergyOverall AbschGesamt Run RotorR Rect_ID
#>  [1,] 1300 1300    93.18271      5577.089         100   5     20      67
#>  [2,] 1100  900    93.18271      5577.089         100   5     20      46
#>  [3,] 1700  900    93.18271      5577.089         100   5     20      49
#>  [4,]  500 1300    93.18271      5577.089           0   5     20      63
#>  [5,] 1500 1100    93.18271      5577.089           0   5     20      58
#>  [6,] 1100 1300    93.18271      5577.089           0   5     20      66
#>  [7,]  300  500    93.18271      5577.089           0   5     20      22
#>  [8,] 1300 1700    93.18271      5577.089           0   5     20      87
#>  [9,] 1700 1700    93.18271      5577.089           0   5     20      89
#> [10,] 1900 1700    93.18271      5577.089           0   5     20      90
#>       Parkfitness
#>  [1,]    5196.883
#>  [2,]    5196.883
#>  [3,]    5196.883
#>  [4,]    5196.883
#>  [5,]    5196.883
#>  [6,]    5196.883
#>  [7,]    5196.883
#>  [8,]    5196.883
#>  [9,]    5196.883
#> [10,]    5196.883
#> 
#> $`24,28,33,44,55,58,22,90,95,99`
#>          X    Y EfficAllDir EnergyOverall AbschGesamt Run RotorR Rect_ID
#>  [1,]  700  500    94.25945      5641.533         100   6     20      24
#>  [2,] 1500  500    94.25945      5641.533         100   6     20      28
#>  [3,]  500  700    94.25945      5641.533           0   6     20      33
#>  [4,]  700  900    94.25945      5641.533           0   6     20      44
#>  [5,]  900 1100    94.25945      5641.533         100   6     20      55
#>  [6,] 1500 1100    94.25945      5641.533           0   6     20      58
#>  [7,]  300  500    94.25945      5641.533           0   6     20      22
#>  [8,] 1900 1700    94.25945      5641.533           0   6     20      90
#>  [9,]  900 1900    94.25945      5641.533           0   6     20      95
#> [10,] 1700 1900    94.25945      5641.533           0   6     20      99
#>       Parkfitness
#>  [1,]    5317.679
#>  [2,]    5317.679
#>  [3,]    5317.679
#>  [4,]    5317.679
#>  [5,]    5317.679
#>  [6,]    5317.679
#>  [7,]    5317.679
#>  [8,]    5317.679
#>  [9,]    5317.679
#> [10,]    5317.679
#> 
# }
```
