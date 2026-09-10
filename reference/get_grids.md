# Get the Grid-IDs from binary matrix

Retrieve the grid ID's from the binary matrix, where the binary code
indicates which grid cells are used in the current wind farm
constellation.

## Usage

``` r
get_grids(trimtonOut, Grid)
```

## Arguments

- trimtonOut:

  Input matrix with binary values

- Grid:

  Grid of the considered area

## Value

Returns a list of all individuals with X and Y coordinates and the grid
cell ID.

## See also

Other Helper Functions:
[`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md),
[`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md),
[`isSpatial()`](https://YsoSirius.github.io/windfarmGA/reference/isSpatial.md),
[`permutations()`](https://YsoSirius.github.io/windfarmGA/reference/permutations.md),
[`readinteger()`](https://YsoSirius.github.io/windfarmGA/reference/readinteger.md),
[`readintegerSel()`](https://YsoSirius.github.io/windfarmGA/reference/readintegerSel.md),
[`splitAt()`](https://YsoSirius.github.io/windfarmGA/reference/splitAt.md),
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)

## Examples

``` r
# \donttest{
## Create a random rectangular shapefile
library(sf)
Polygon1 <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(0, 0, 2000, 2000, 0),
    c(0, 2000, 2000, 0, 0)
  ))),
  crs = 3035
))

## Calculate a Grid and an indexed data.frame with coordinates and
## grid cell Ids.
Grid1 <- grid_area(shape = Polygon1, size = 200, prop = 1)
Grid <- Grid1[[1]]
AmountGrids <- nrow(Grid)

startsel <- init_population(Grid, 10, 20)
wind <- data.frame(ws = 12, wd = 0)
wind <- list(wind, probab = 100)
fit <- fitness(
  selection = startsel, referenceHeight = 100, RotorHeight = 100,
  SurfaceRoughness = 0.3, Polygon = Polygon1, resol1 = 200, rot = 20,
  dirspeed = wind, srtm_crop = "", topograp = FALSE, cclRaster = ""
)
allparks <- do.call("rbind", fit)

## SELECTION
## print the amount of Individuals selected.
## Check if the amount of Turbines is as requested.
selec6best <- selection(fit, Grid, 2, TRUE, 6, "VAR")

## CROSSOVER
## u determines the amount of crossover points,
## crossPart determines the method used (Equal/Random),
## uplimit is the maximum allowed permutations
crossOut <- crossover(selec6best, 2, uplimit = 300, crossPart = "RAN")

## MUTATION
## Variable Mutation Rate is activated if more than 2 individuals represent
## the current best solution.
mut <- mutation(a = crossOut, p = 0.3)

## TRIMTON
## After Crossover and Mutation, the amount of turbines in a windpark change
## and have to be corrected to the required amount of turbines.
mut1 <- trimton(
  mut = mut, nturb = 10, allparks = allparks,
  nGrids = AmountGrids, trimForce = FALSE
)

## Get the new Grid-Ids and run a new fitness run.
getRectV <- get_grids(mut1, Grid)
fit <- fitness(
  selection = getRectV, referenceHeight = 100, RotorHeight = 100,
  SurfaceRoughness = 0.3, Polygon = Polygon1, resol1 = 200, rot = 20,
  dirspeed = wind, srtm_crop = "", topograp = FALSE, cclRaster = ""
)
head(fit)
#> $`14,17,25,31,43,52,54,82,91,95`
#>          X    Y EfficAllDir EnergyOverall AbschGesamt Run RotorR Rect_ID
#>  [1,]  700  300    95.95216      7567.792         100   1     20      14
#>  [2,] 1300  300    95.95216      7567.792           0   1     20      17
#>  [3,]  900  500    95.95216      7567.792         100   1     20      25
#>  [4,]  100  700    95.95216      7567.792         100   1     20      31
#>  [5,]  500  900    95.95216      7567.792           0   1     20      43
#>  [6,]  300 1100    95.95216      7567.792         100   1     20      52
#>  [7,]  700 1100    95.95216      7567.792           0   1     20      54
#>  [8,]  300 1700    95.95216      7567.792           0   1     20      82
#>  [9,]  100 1900    95.95216      7567.792           0   1     20      91
#> [10,]  900 1900    95.95216      7567.792           0   1     20      95
#>       Parkfitness
#>  [1,]    7567.792
#>  [2,]    7567.792
#>  [3,]    7567.792
#>  [4,]    7567.792
#>  [5,]    7567.792
#>  [6,]    7567.792
#>  [7,]    7567.792
#>  [8,]    7567.792
#>  [9,]    7567.792
#> [10,]    7567.792
#> 
#> $`31,39,49,52,59,61,83,86,89,99`
#>          X    Y EfficAllDir EnergyOverall AbschGesamt Run RotorR Rect_ID
#>  [1,]  100  700    80.20216      6325.582         100   2     20      31
#>  [2,] 1700  700    80.20216      6325.582         400   2     20      39
#>  [3,] 1700  900    80.20216      6325.582         300   2     20      49
#>  [4,]  300 1100    80.20216      6325.582           0   2     20      52
#>  [5,] 1700 1100    80.20216      6325.582         200   2     20      59
#>  [6,]  100 1300    80.20216      6325.582           0   2     20      61
#>  [7,]  500 1700    80.20216      6325.582           0   2     20      83
#>  [8,] 1100 1700    80.20216      6325.582           0   2     20      86
#>  [9,] 1700 1700    80.20216      6325.582         100   2     20      89
#> [10,] 1700 1900    80.20216      6325.582           0   2     20      99
#>       Parkfitness
#>  [1,]    6325.582
#>  [2,]    6325.582
#>  [3,]    6325.582
#>  [4,]    6325.582
#>  [5,]    6325.582
#>  [6,]    6325.582
#>  [7,]    6325.582
#>  [8,]    6325.582
#>  [9,]    6325.582
#> [10,]    6325.582
#> 
#> $`4,27,30,33,61,65,69,87,93,94`
#>          X    Y EfficAllDir EnergyOverall AbschGesamt Run RotorR Rect_ID
#>  [1,]  700  100    98.40852      7761.526         100   3     20       4
#>  [2,] 1300  500    98.40852      7761.526         100   3     20      27
#>  [3,] 1900  500    98.40852      7761.526           0   3     20      30
#>  [4,]  500  700    98.40852      7761.526         100   3     20      33
#>  [5,]  100 1300    98.40852      7761.526           0   3     20      61
#>  [6,]  900 1300    98.40852      7761.526           0   3     20      65
#>  [7,] 1700 1300    98.40852      7761.526           0   3     20      69
#>  [8,] 1300 1700    98.40852      7761.526           0   3     20      87
#>  [9,]  500 1900    98.40852      7761.526           0   3     20      93
#> [10,]  700 1900    98.40852      7761.526           0   3     20      94
#>       Parkfitness
#>  [1,]    7761.526
#>  [2,]    7761.526
#>  [3,]    7761.526
#>  [4,]    7761.526
#>  [5,]    7761.526
#>  [6,]    7761.526
#>  [7,]    7761.526
#>  [8,]    7761.526
#>  [9,]    7761.526
#> [10,]    7761.526
#> 
#> $`2,3,9,55,56,57,63,65,80,89`
#>          X    Y EfficAllDir EnergyOverall AbschGesamt Run RotorR Rect_ID
#>  [1,]  300  100    93.83756      7401.013           0   4     20       2
#>  [2,]  500  100    93.83756      7401.013         100   4     20       3
#>  [3,] 1700  100    93.83756      7401.013         100   4     20       9
#>  [4,]  900 1100    93.83756      7401.013         100   4     20      55
#>  [5,] 1100 1100    93.83756      7401.013           0   4     20      56
#>  [6,] 1300 1100    93.83756      7401.013           0   4     20      57
#>  [7,]  500 1300    93.83756      7401.013           0   4     20      63
#>  [8,]  900 1300    93.83756      7401.013           0   4     20      65
#>  [9,] 1900 1500    93.83756      7401.013           0   4     20      80
#> [10,] 1700 1700    93.83756      7401.013           0   4     20      89
#>       Parkfitness
#>  [1,]    7401.013
#>  [2,]    7401.013
#>  [3,]    7401.013
#>  [4,]    7401.013
#>  [5,]    7401.013
#>  [6,]    7401.013
#>  [7,]    7401.013
#>  [8,]    7401.013
#>  [9,]    7401.013
#> [10,]    7401.013
#> 
#> $`12,38,48,53,54,62,68,76,91,94`
#>          X    Y EfficAllDir EnergyOverall AbschGesamt Run RotorR Rect_ID
#>  [1,]  300  300    89.86455       7087.66         100   5     20      12
#>  [2,] 1500  700    89.86455       7087.66         200   5     20      38
#>  [3,] 1500  900    89.86455       7087.66         100   5     20      48
#>  [4,]  500 1100    89.86455       7087.66           0   5     20      53
#>  [5,]  700 1100    89.86455       7087.66         100   5     20      54
#>  [6,]  300 1300    89.86455       7087.66           0   5     20      62
#>  [7,] 1500 1300    89.86455       7087.66           0   5     20      68
#>  [8,] 1100 1500    89.86455       7087.66           0   5     20      76
#>  [9,]  100 1900    89.86455       7087.66           0   5     20      91
#> [10,]  700 1900    89.86455       7087.66           0   5     20      94
#>       Parkfitness
#>  [1,]     7087.66
#>  [2,]     7087.66
#>  [3,]     7087.66
#>  [4,]     7087.66
#>  [5,]     7087.66
#>  [6,]     7087.66
#>  [7,]     7087.66
#>  [8,]     7087.66
#>  [9,]     7087.66
#> [10,]     7087.66
#> 
#> $`15,19,23,45,47,66,71,88,94,100`
#>          X    Y EfficAllDir EnergyOverall AbschGesamt Run RotorR Rect_ID
#>  [1,]  900  300    98.25615      7749.509         100   6     20      15
#>  [2,] 1700  300    98.25615      7749.509           0   6     20      19
#>  [3,]  500  500    98.25615      7749.509           0   6     20      23
#>  [4,]  900  900    98.25615      7749.509           0   6     20      45
#>  [5,] 1300  900    98.25615      7749.509           0   6     20      47
#>  [6,] 1100 1300    98.25615      7749.509           0   6     20      66
#>  [7,]  100 1500    98.25615      7749.509           0   6     20      71
#>  [8,] 1500 1700    98.25615      7749.509           0   6     20      88
#>  [9,]  700 1900    98.25615      7749.509           0   6     20      94
#> [10,] 1900 1900    98.25615      7749.509           0   6     20     100
#>       Parkfitness
#>  [1,]    7749.509
#>  [2,]    7749.509
#>  [3,]    7749.509
#>  [4,]    7749.509
#>  [5,]    7749.509
#>  [6,]    7749.509
#>  [7,]    7749.509
#>  [8,]    7749.509
#>  [9,]    7749.509
#> [10,]    7749.509
#> 
# }
```
