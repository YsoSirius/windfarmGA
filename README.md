# windfarmGA

<p align="center">
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/windfarmGA.png" align="right" width="150"/>
</p>

<!-- badges: start -->
[![](https://www.r-pkg.org/badges/version/windfarmGA)](https://www.r-pkg.org/pkg/windfarmGA)
[![cran checks](https://cranchecks.info/badges/worst/windfarmGA)](https://cran.r-project.org/web/checks/check_results_windfarmGA.html)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/windfarmGA?color=brightgreen)](https://www.r-pkg.org/pkg/windfarmGA)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/windfarmGA)](https://www.rpackages.io/package/windfarmGA)
[![R build status](https://github.com/YsoSirius/windfarmGA/workflows/R-CMD-check/badge.svg)](https://github.com/YsoSirius/windfarmGA/actions)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/YsoSirius/windfarmGA?branch=master&svg=true)](https://ci.appveyor.com/project/YsoSirius/windfarmGA)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![codecov](https://codecov.io/gh/YsoSirius/windfarmGA/branch/master/graph/badge.svg)](https://codecov.io/gh/YsoSirius/windfarmGA)

<!-- badges: end -->


Genetic algorithm to optimize the layout of windfarms.
The package is hosted on [CRAN](https://CRAN.R-project.org/package=windfarmGA)

# Installation
The latest version can be installed from GitHub with:
```sh
devtools::install_github("YsoSirius/windfarmGA")
```

and the CRAN-version with:
```sh
install.packages("windfarmGA")
```

# Description
The genetic algorithm is designed to optimize wind farms of any shape.
It requires a predefined number of turbines, a uniform rotor radius and 
an average wind speed per wind direction.
It can include a terrain effect model, which downloads an 
'SRTM' elevation model and a 'Corine Land Cover' raster automatically. The elevation 
model is used to find mountains and valleys and to adjust the 
wind speeds accordingly by 'wind multipliers' and to determine 
the air densities at rotor heights. The land cover raster with an additional elevation
roughness value is used to re-evaluate the surface roughness and to individually
determine the wake-decay constant for each turbine.

To start an optimization, either the function `windfarmGA` or `genetic_algorithm` can 
be used. The function `windfarmGA` checks the user inputs interactively and then 
runs the function `genetic_algorithm`. If the input parameters are already known, an 
optimization can be run directly via `genetic_algorithm`. 

<div style="display: inline-flex;">
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/result2.png" width="430"/>
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/result1.png"  width="430"/>
</div>
<br>

Since version 1.1, hexagonal grid cells are possible, with 
their center points being possible locations for wind turbines. 
Furthermore, rasters can be included, which contain information on the Weibull 
parameters. For Austria this data is already included in the package. 
    
## Create an input Polygon
- Input Polygon by source
```sh
library(sf)
dsn <- "Path to the Shapefile"
layer <- "Name of the Shapefile"
Polygon1 <- sf::st_read(dsn = dsn, layer = layer)
plot(Polygon1, col = "blue")
```

- Or create a random Polygon
```sh
library(sf)
Polygon1 <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(0, 0, 2000, 2000, 0),
    c(0, 2000, 2000, 0, 0)))),
  crs = 3035
))
plot(Polygon1, col = "blue", axes = TRUE)
```

## Create random Wind data 
- Exemplary input Wind data with *uniform* wind speed and *single* wind direction
```sh
wind_df <- data.frame(ws = c(12, 12), wd = c(0, 0), probab = c(25, 25))
windrosePlot <- plot_windrose(data = wind_df, spd = wind_df$ws,
                              dir = wind_df$wd, dirres=10, spdmax = 20)
```

- Exemplary input Wind data with *random* wind speeds and *random* wind directions
```sh
wind_df <- data.frame(ws = sample(1:25, 10), wd = sample(1:260, 10)))
windrosePlot <- plot_windrose(data = wind_df, spd = wind_df$ws,
                              dir = wind_df$wd)
```

## Grid Spacing
### Rectangular Grid Cells
Verify that the grid spacing is appropriate. Adapt the following input variables if necessary:
- *Rotor*: The rotor radius in meters.
- *fcrR*: The grid spacing factor, which should at least be 2, so that a single grid covers at least the whole rotor diameter.
- *prop*: The proportionality factor used for grid calculation. It determines the minimum percentage that a grid cell must cover of the area.

*Make sure that the Polygon is projected in meters.*
```sh
Rotor <- 20
fcrR <- 9
Grid <- grid_area(Polygon1, size = (Rotor * fcrR), prop = 1, plotGrid = TRUE)
str(Grid)
```
### Hexagonal Grid Cells
```sh
Rotor <- 20
fcrR <- 9
HexGrid <- hexa_area(Polygon1, size = (Rotor * fcrR), plotGrid = TRUE)
str(HexGrid)
```
<p align="center">
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/grids.png" width="300"/>
</p>


## Terrain Effect Model
If the input variable **topograp** for the functions `windfarmGA` or `genetic_algorithm` is TRUE, the genetic algorithm will take terrain effects into account. For this purpose an elevation model and a Corine Land Cover raster are downloaded automatically, but can also be given manually. ( [Download a CLC raster](http://www.eea.europa.eu/data-and-maps/data/clc-2006-raster-1) ).

If you want to include your own Land Cover Raster, you must assign the Raster Image path to the input variable **sourceCCL**. The algorithm uses an adapted version of the Raster legend ("clc_legend.csv"), which is stored in the package subdirectory (/extdata). To use own values for the land cover roughness lengths, insert a column named **Rauhigkeit_z** to the .csv file. Assign a surface roughness length to all land cover types. 
Be sure that all rows are filled with numeric values and save the .csv file with ";" delimiter. Assign the .csv file path to the input variable **sourceCCLRoughness**.


## Start an Optimization
An optimization run can be initiated with the following functions: 
- genetic_algorithm
- windfarmGA

### Function calls for windfarmGA
- without terrain effects
```sh
result <- windfarmGA(Polygon1 = Polygon1, n = 12, Rotor = 20, fcrR = 9, iteration = 10,
                     vdirspe = wind_df, crossPart1 = "EQU", selstate = "FIX", mutr = 0.8,
                     Proportionality = 1, SurfaceRoughness = 0.3, topograp = FALSE,
                     elitism =TRUE, nelit = 7, trimForce = TRUE,
                     referenceHeight = 50, RotorHeight = 100)
```

- with terrain effects
```sh
sourceCCL <- "Source of the CCL raster (TIF)"
sourceCCLRoughness <- "Source of the Adaped CCL legend (CSV)"

result <- windfarmGA(Polygon1 = Polygon1, n = 12, Rotor = 20, fcrR = 9, iteration = 10,
                     vdirspe = wind_df, crossPart1 = "EQU", selstate = "FIX", mutr = 0.8,
                     Proportionality = 1, SurfaceRoughness = 0.3, topograp = TRUE,
                     elitism = TRUE, nelit = 7, trimForce = TRUE,
                     referenceHeight = 50, RotorHeight = 100, sourceCCL = sourceCCL,
                     sourceCCLRoughness = sourceCCLRoughness)
```

###  Function calls for genetic_algorithm
- without terrain effects
```sh
result <- genetic_algorithm(Polygon1 = Polygon1, n = 12, Rotor = 20, fcrR = 9, iteration = 10,
                            vdirspe = wind_df, crossPart1 = "EQU", selstate = "FIX", mutr =0.8,
                            Proportionality = 1, SurfaceRoughness = 0.3, topograp = FALSE,
                            elitism = TRUE, nelit = 7, trimForce = TRUE,
                            referenceHeight = 50, RotorHeight = 100)
```

- with terrain effects
```sh
sourceCCL <- "Source of the CCL raster (TIF)"
sourceCCLRoughness <- "Source of the Adaped CCL legend (CSV)"
result <- genetic_algorithm(Polygon1 = Polygon1, n= 12, Rotor = 20, fcrR = 9, iteration = 10,
                            vdirspe = wind_df, crossPart1 = "EQU", selstate = "FIX", mutr = 0.8,
                            Proportionality = 1, SurfaceRoughness = 0.3, topograp = TRUE,
                            elitism = TRUE, nelit = 7, trimForce = TRUE,
                            referenceHeight = 50, RotorHeight = 100, sourceCCL = sourceCCL,
                            sourceCCLRoughness = sourceCCLRoughness)
```

```sh
## Run an optimization with your own Weibull parameter rasters. The shape and scale 
## parameter rasters of the weibull distributions must be added to a list, with the first
## list item being the shape parameter (k) and the second list item being the scale
## parameter (a). Adapt the paths to your raster data and run an optimization.
kraster <- "/..pathto../k_param_raster.tif"
araster <- "/..pathto../a_param_raster.tif"
weibullrasters <- list(raster(kraster), raster(araster))

result_weibull <- genetic_algorithm(Polygon1 = Polygon1, GridMethod ="h", n=12,
                                    fcrR=5, iteration=10, vdirspe = wind_df, crossPart1 = "EQU",
                                    selstate="FIX", mutr=0.8, Proportionality = 1, Rotor=30,
                                    SurfaceRoughness = 0.3, topograp = FALSE,
                                    elitism=TRUE, nelit = 7, trimForce = TRUE,
                                    referenceHeight = 50,RotorHeight = 100,
                                    weibull = TRUE, weibullsrc = weibullrasters)
plot_windfarmGA(result = result_weibull, Polygon1 = Polygon1)
```
The argument **GridMethod**, **weibull**, **weibullsrc** can also be given to the function `windfarmGA`.

#### Plot the Results on a Leaflet Map
```sh
## Plot the best wind farm on a leaflet map (ordered by energy values)
plot_leaflet(result = resulthex, Polygon1, which = 1)

## Plot the last wind farm (ordered by chronology).
plot_leaflet(result = resulthex, Polygon1, orderitems = FALSE, which = 1)
```

## Plotting Methods of the Genetic Algorithm 
Several plotting functions are available:
```sh
 - plot_windfarmGA(result, Polygon1)
 - plot_result(result, Polygon1, best = 1)
 - plot_evolution(result, ask = TRUE, spar = 0.1)
 - plot_development(result)
 - plot_parkfitness(result, spar = 0.1)
 - plot_fitness_evolution(result)
 - plot_cloud(result, pl = TRUE)
 - plot_heatmap(result = result, si = 5)
 - plot_leaflet(result = result, Polygon1 = Polygon1, which = 1)
```

For further information, please check the package [description and examples](https://CRAN.R-project.org/package=windfarmGA/windfarmGA.pdf).
A full documentation of the genetic algorithm is given in my [master thesis](https://homepage.boku.ac.at/jschmidt/TOOLS/Masterarbeit_Gatscha.pdf).

# Shiny Windfarm Optimization
I also made a [Shiny App](https://windfarmga.shinyapps.io/windga_shiny/) for the Genetic Algorithm. 
Unfortunately, as an optimization takes quite some time and the app is currently hosted by shinyapps.io under a public license, there is only 1 R-worker at hand. So only 1 optimization can be run at a time. 

# Full Optimization example:
```sh
library(sf)
library(windfarmGA)

Polygon1 <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4651704, 4651704, 4654475, 4654475, 4651704),
    c(2692925, 2694746, 2694746, 2692925, 2692925)))), 
  crs = 3035
))
plot(Polygon1, col = "blue", axes = TRUE)

wind_df <- data.frame(ws = 12, wd = 0)
windrosePlot <- plot_windrose(data = wind_df, spd = wind_df$ws,
                             dir = wind_df$wd, dirres = 10, spdmax = 20)
Rotor <- 20
fcrR <- 9
Grid <- grid_area(shape = Polygon1, size = (Rotor*fcrR), prop = 1, plotGrid = TRUE)

result <- genetic_algorithm(Polygon1 = sp_polygon, 
                            n = 20,
                            Rotor = Rotor, fcrR = fcrR, 
                            iteration = 50, 
                            vdirspe = wind_df,
                            referenceHeight = 50, RotorHeight = 100)

# The following function will execute all plotting function further below:
plot_windfarmGA(result, Polygon1, whichPl = "all", best = 1, plotEn = 1)

# The plotting functions can also be called individually:
plot_result(result, Polygon1, best = 1, plotEn = 1, topographie = FALSE)
plot_evolution(result, ask = TRUE, spar = 0.1)
plot_parkfitness(result, spar = 0.1)
plot_fitness_evolution(result)
plot_cloud(result, pl = TRUE)
plot_heatmap(result = result, si = 5)
plot_leaflet(result = result, Polygon1 = Polygon1, which = 1)
```
