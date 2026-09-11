# Calculate distances and angles of possibly influencing turbines

Calculate distances and angles for a turbine and all it's potentially
influencing turbines.

## Usage

``` r
get_dist_angles(t, o, wnkl, dist, area, plot_angles = FALSE)
```

## Arguments

- t:

  A data.frame of the current individual with X and Y coordinates

- o:

  A numeric value indicating the index of the current turbine

- wnkl:

  Wake opening angle in degrees. Turbines outside this cone are ignored.

- dist:

  A numeric value indicating the distance, after which the wake effects
  are considered to be eliminated.

- area:

  Site polygon

- plot_angles:

  Plot distances and angles

## Value

Returns a matrix with the distances, angles and heights of potentially
influencing turbines

## See also

Other Wind Energy Calculation Functions:
[`barometric_height()`](https://YsoSirius.github.io/windfarmGA/reference/barometric_height.md),
[`calculate_energy()`](https://YsoSirius.github.io/windfarmGA/reference/calculate_energy.md),
[`circle_intersection()`](https://YsoSirius.github.io/windfarmGA/reference/circle_intersection.md),
[`turbine_influences()`](https://YsoSirius.github.io/windfarmGA/reference/turbine_influences.md)

## Examples

``` r
library(sf)

## Exemplary input Polygon with 2km x 2km:
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))

## Create a random windfarm with 10 turbines
t <- st_coordinates(st_sample(area, 10))
t <- cbind(t, "Z" = 1)
wnkl <- 20
dist <- 100000

## Evaluate and plot for every turbine all other potentially influencing turbines
potInfTur <- list()
for (i in 1:(length(t[, 1]))) {
  potInfTur[[i]] <- get_dist_angles(
    t = t, o = i, wnkl = wnkl,
    dist = dist, area = area, plot_angles = TRUE
  )
}










potInfTur
#> [[1]]
#>      Ax Ay      Bx      By Cx Cy Laenge_C Laenge_B Laenge_A alpha betha gamma
#> [1,]  0  0 4499324 2669115  0  0        0        0        0     0     0     0
#>      height1 height2
#> [1,]       0       0
#> 
#> [[2]]
#>           Ax      Ay      Bx      By      Cx      Cy Laenge_C Laenge_B Laenge_A
#> [1,] 4499324 2669115 4499223 2668495 4499324 2668495 627.6793    619.5      101
#> [2,] 4499281 2669089 4499223 2668495 4499281 2668495 596.3273    593.5       58
#>         alpha    betha gamma height1 height2
#> [1,] 9.265732 80.73427    90       1       1
#> [2,] 5.588172 84.41183    90       1       1
#> 
#> [[3]]
#>           Ax      Ay      Bx      By      Cx      Cy Laenge_C Laenge_B Laenge_A
#> [1,] 4499870 2669079 4499891 2668958 4499870 2668958 123.1414   121.25     21.5
#>        alpha   betha gamma height1 height2
#> [1,] 10.0502 79.9498    90       1       1
#> 
#> [[4]]
#>      Ax Ay      Bx      By Cx Cy Laenge_C Laenge_B Laenge_A alpha betha gamma
#> [1,]  0  0 4498788 2669304  0  0        0        0        0     0     0     0
#>      height1 height2
#> [1,]       0       0
#> 
#> [[5]]
#>      Ax Ay      Bx      By Cx Cy Laenge_C Laenge_B Laenge_A alpha betha gamma
#> [1,]  0  0 4499281 2669089  0  0        0        0        0     0     0     0
#>      height1 height2
#> [1,]       0       0
#> 
#> [[6]]
#>      Ax Ay      Bx      By Cx Cy Laenge_C Laenge_B Laenge_A alpha betha gamma
#> [1,]  0  0 4499870 2669079  0  0        0        0        0     0     0     0
#>      height1 height2
#> [1,]       0       0
#> 
#> [[7]]
#>      Ax Ay      Bx      By Cx Cy Laenge_C Laenge_B Laenge_A alpha betha gamma
#> [1,]  0  0 4499474 2668859  0  0        0        0        0     0     0     0
#>      height1 height2
#> [1,]       0       0
#> 
#> [[8]]
#>           Ax      Ay      Bx      By      Cx      Cy Laenge_C Laenge_B Laenge_A
#> [1,] 4498788 2669304 4498836 2668846 4498788 2668846 460.7052   458.25     47.5
#>         alpha    betha gamma height1 height2
#> [1,] 5.925308 84.07469    90       1       1
#> 
#> [[9]]
#>           Ax      Ay      Bx      By      Cx      Cy Laenge_C Laenge_B Laenge_A
#> [1,] 4499324 2669115 4499316 2668523 4499324 2668523 592.5684    592.5        9
#> [2,] 4499281 2669089 4499316 2668523 4499281 2668523 567.5194    566.5       34
#>          alpha    betha gamma height1 height2
#> [1,] 0.8385004 89.16150    90       1       1
#> [2,] 3.4649097 86.53509    90       1       1
#> 
#> [[10]]
#>           Ax      Ay      Bx      By      Cx      Cy Laenge_C Laenge_B Laenge_A
#> [1,] 4499870 2669079 4499853 2668944 4499870 2668944 136.5623    135.5       17
#>        alpha    betha gamma height1 height2
#> [1,] 7.15317 82.84683    90       1       1
#> 
```
