# windfarmGA

<p align="center">
  <img src="/inst/img/windfarmGA.png" align="right" width="150"/>
</p>

[![](https://www.r-pkg.org/badges/version/windfarmGA)](https://www.r-pkg.org/pkg/windfarmGA)
[![cran checks](https://cranchecks.info/badges/worst/windfarmGA)](https://cran.r-project.org/web/checks/check_results_windfarmGA.html)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/windfarmGA?color=brightgreen)](https://www.r-pkg.org/pkg/windfarmGA)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/windfarmGA)](https://www.rpackages.io/package/windfarmGA)
[![Build Status](https://travis-ci.org/YsoSirius/windfarmGA.svg?branch=master)](https://travis-ci.org/YsoSirius/windfarmGA)
[![codecov](https://codecov.io/gh/YsoSirius/windfarmGA/branch/master/graph/badge.svg)](https://codecov.io/gh/YsoSirius/windfarmGA)



Genetic algorithm to optimize the layout of windfarms.
The package is hosted on CRAN and can be found at https://CRAN.R-project.org/package=windfarmGA

# Installation
The latest version can be installed from GitHub with:
 ```sh
# install.packages("devtools")
devtools::install_github("YsoSirius/windfarmGA")
```
and version 1.2.1 via CRAN with:
 ```sh
install.packages("windfarmGA")
```

# Description
The genetic algorithm is designed to optimize small wind farms. 
The algorithm works with a fixed amount of turbines, a fixed 
rotor radius and a mean wind speed value for every incoming wind direction.
If required it can include a terrain effect model, which downloads an 
'SRTM' elevation model automatically and loads a Corine Land Cover raster, 
which has to be downloaded previously. 

To start an optimization, either the function 'windfarmGA' or 'genAlgo' can 
be used. The function 'windfarmGA' checks the user inputs interactively and then 
runs the function 'genAlgo'. If the input parameters are already known, an 
optimization can be run directly via the function 'genAlgo'. 
Their output is identical.   

<div>
  <img src="/inst/img/result2.png" width="430"/>
  <img src="/inst/img/result1.png"  width="430"/>
</div>
<br>

Since version 1.1, hexagonal grid cells are possible, with 
their center points being possible locations for wind turbines. 
Furthermore, rasters can be included, which contain information on the Weibull 
parameters. For Austria this data is already included in the package. 
    
## Create an input Polygon
- Input Polygon by source
 ```sh
dsn <- "Path to the Shapefile"
layer <- "Name of the Shapefile"
Polygon1 <- rgdal::readOGR(dsn = dsn, layer = layer)
plot(Polygon1, col = "blue")
```
- Or create a random Polygon
 ```sh
library(rgdal); library(sp); library(windfarmGA);
Polygon1 <- Polygon(rbind(c(4651704, 2692925), c(4651704, 2694746), 
                          c(4654475, 2694746), c(4654475, 2692925)))
Polygon1 <- sp::Polygons(list(Polygon1), 1)
Polygon1 <- sp::SpatialPolygons(list(Polygon1))
Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4string(Polygon1) <- CRS(Projection)
plot(Polygon1, col = "blue", axes = TRUE)
```

## Create random Wind data 
- Exemplary input Wind data with uniform wind speed and single wind direction
 ```sh
wind_df <- data.frame(ws = c(12, 12), wd = c(0, 0), probab = c(25, 25))
windrosePlot <- plotWindrose(data = wind_df, spd = wind_df$ws,
                             dir = wind_df$wd, dirres=10, spdmax = 20)
```
- Exemplary input Wind data with random wind speeds and random wind directions
 ```sh
wind_df <- data.frame(ws = sample(1:25, 10), wd = sample(1:260, 10)))
windrosePlot <- plotWindrose(data = wind_df, spd = wind_df$ws,
                             dir = wind_df$wd)
```

## Grid Spacing
### Rectangular Grid Cells
Verify that the grid spacing is appropriate. Adapt the following input variables if desired:
- *Rotor*: The desired rotor radius in meters.
- *fcrR*: The grid spacing factor, which should at least be 2, so that a single grid covers at least the whole rotor diameter.
- *prop*: The proportionality factor used for grid calculation. It determines the percentage a grid has 
to overlay the considered area to be represented as grid cell.

*Make sure that the Polygon is projected in meters.*
 ```sh
Rotor <- 20
fcrR <- 9
# proj4string(Polygon1)
# Polygon1 <- spTransform(Polygon1, CRSobj = CRS(Projection))
Grid <- GridFilter(shape = Polygon1, resol = (Rotor*fcrR), prop = 1, plotGrid = TRUE)
str(Grid)
```
### Hexagonal Grid Cells
```sh
Rotor <- 20
fcrR <- 9
HexGrid <- HexaTex(Polygon1, size = ((Rotor*fcrR)/2), plotTrue = TRUE)
str(HexGrid)
```
<p align="center">
  <img src="/inst/img/hexagon.png" width="300"/>
</p>


## Terrain Effect Model
If the input variable **`topograp`** for the functions 'windfarmGA' or 'genAlgo' is TRUE, then the genetic algorithm 
will take terrain effects into account. For this purpose an elevation model is downloaded automatically by the 'raster' package
and a Corine Land Cover raster must be downloaded and given manually. (Download at: http://www.eea.europa.eu/data-and-maps/data/clc-2006-raster-1).
Download the .zip package with 100 meter resolution. Unzip the downloaded package and assign the source of the Raster Image
"g100_06.tif" to the package input variable **`sourceCCL`**. The algorithm will use an adapted version of the Raster legend
("clc_legend.csv"), which is stored in the package subdirectory (/extdata). To use own values for the land cover roughness
lengths, insert a column named **Rauhigkeit_z** to the .csv file. Assign a surface roughness length to all land cover types. 
Be sure that all rows are filled with numeric values and save the .csv file with ";" delimiter. Assign the source of 
the resulting .csv file to the input variable **`sourceCCLRoughness`** of this function. For further information, see
the examples of the package.


## Start an Optimization
An optimization run can be initiated with the following functions: 
- genAlgo
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

###  Function calls for genAlgo
- without terrain effects
```sh
result <- genAlgo(Polygon1 = Polygon1, n = 12, Rotor = 20, fcrR = 9, iteration = 10,
             vdirspe = wind_df, crossPart1 = "EQU", selstate = "FIX", mutr =0.8,
             Proportionality = 1, SurfaceRoughness = 0.3, topograp = FALSE,
             elitism = TRUE, nelit = 7, trimForce = TRUE,
             referenceHeight = 50, RotorHeight = 100)
```
- with terrain effects
 ```sh
sourceCCL <- "Source of the CCL raster (TIF)"
sourceCCLRoughness <- "Source of the Adaped CCL legend (CSV)"
result <- genAlgo(Polygon1 = Polygon1, n= 12, Rotor = 20, fcrR = 9, iteration = 10,
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

result_weibull <- genAlgo(Polygon1 = Polygon1, GridMethod ="h", n=12,
                  fcrR=5,iteration=10, vdirspe = wind_df, crossPart1 = "EQU",
                  selstate="FIX",mutr=0.8, Proportionality = 1, Rotor=30,
                  SurfaceRoughness = 0.3, topograp = FALSE,
                  elitism=TRUE, nelit = 7, trimForce = TRUE,
                  referenceHeight = 50,RotorHeight = 100,
                  weibull = TRUE, weibullsrc = weibullrasters)
PlotWindfarmGA(result = result_weibull, GridMethod = "h", Polygon1 = Polygon1)
```
The argument **'GridMethod'**, **'weibull'**, **'weibullsrc'** can also be given to the function **'windfarmGA'**.

#### Plot the Results on a Leaflet Map
```sh
## Plot the best wind farm on a leaflet map (ordered by energy values)
leafPlot(result = resulthex, Polygon1 = polygon, which = 1)

## Plot the last wind farm (ordered by chronology).
leafPlot(result = resulthex, Polygon1 = polygon, orderitems = F, which = 1)
```

## Plotting Methods of the Genetic Algorithm 
Several plotting functions are available:
 ```sh
- PlotWindfarmGA(result, Polygon1, whichPl = "all", best = 1, plotEn = 1)
- plotResult(result, Polygon1, best = 1, plotEn = 1, topographie = FALSE, Grid = Grid[[2]])
- plotEvolution(result, ask = TRUE, spar = 0.1)
- plotparkfitness(result, spar = 0.1)
- plotfitnessevolution(result)
- plotCloud(result, pl = TRUE)
- heatmapGA(result = result, si = 5)
- leafPlot(result = result, Polygon1 = polygon, which = 1)
```

For further information, please check the package description and examples. (https://CRAN.R-project.org/package=windfarmGA/windfarmGA.pdf)
A full documentation of the genetic algorithm is given in my master thesis, which can be found at the following link: 
https://homepage.boku.ac.at/jschmidt/TOOLS/Masterarbeit_Gatscha.pdf

# Shiny Windfarm Optimization
I also made a Shiny App for the Genetic Algorithm, which can be found here: https://windfarmga.shinyapps.io/windga_shiny/
Unfortunately, as an optimization takes quite some time and the app is currently hosted by shinyapps.io under a public license, there is only 1 R-worker at hand. So only 1 optimization can be run at a time. 

# Full Optimization example:
 ```sh
library(rgdal); library(sp); library(windfarmGA)
Polygon1 <- Polygon(rbind(c(4651704, 2692925), c(4651704, 2694746), 
                          c(4654475, 2694746), c(4654475, 2692925)))
Polygon1 <- sp::Polygons(list(Polygon1), 1);
Polygon1 <- sp::SpatialPolygons(list(Polygon1))
Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4string(Polygon1) <- CRS(Projection)
plot(Polygon1, col = "blue", axes = TRUE)

wind_df <- data.frame(ws = 12, wd = 0)
windrosePlot <- plotWindrose(data = wind_df, spd = wind_df$ws,
                             dir = wind_df$wd, dirres = 10, spdmax = 20)
Rotor <- 20
fcrR <- 9
Grid <- GridFilter(shape = Polygon1, resol = (Rotor*fcrR), prop = 1, plotGrid = TRUE)

result <- windfarmGA(Polygon1 = Polygon1, n = 12, Rotor = Rotor, fcrR = fcrR, iteration = 10,
                     vdirspe = wind_df, crossPart1 = "EQU", selstate = "FIX", mutr = 0.8,
                     Proportionality = 1, SurfaceRoughness = 0.3, topograp = FALSE,
                     elitism = TRUE, nelit = 7, trimForce = TRUE,
                     referenceHeight = 50, RotorHeight = 100)

# The following function will execute all plotting function further below:
PlotWindfarmGA(result, Polygon1, whichPl = "all", best = 1, plotEn = 1)

# The plotting functions can also be called individually:
plotResult(result, Polygon1, best = 1, plotEn = 1, topographie = FALSE, Grid = Grid[[2]])
plotEvolution(result, ask = TRUE, spar = 0.1)
plotparkfitness(result, spar = 0.1)
plotfitnessevolution(result)
plotCloud(result, pl = TRUE)
heatmapGA(result = result, si = 5)
leafPlot(result = result, Polygon1 = polygon, which = 1)
```
