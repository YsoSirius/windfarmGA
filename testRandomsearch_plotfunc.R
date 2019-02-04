####################################
library(windfarmGA)
load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
RRes <- RandomSearch(resultrect, polygon, n=50, best=2)
RRes <- RandomSearch(resultrect, polygon, Plot = T)
RRes


load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
RandomSearch(resulthex, polygon, Plot = T, GridMethod = "h")
##Points liegen außerhalb des Polygons.. Noch eine Check Methode bei Hexagonen

RandomSearchPlot



########################################
load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))

a <- RandomSearchTurb(result = resultrect, Polygon1 = polygon, n=10)
RandomSearchPlot(resultRS = a, resultGA = resultrect, Polygon1 = polygon, best =2)

##Continue with best config. and continue with next turbine. - manually sequentially

## automatic- move the turbines with the highest wake effects.



library(windfarmGA)
load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))

## Plot the results of a hexagonal grid optimization
result <- resulthex
Polygon1 <- polygon
PlotWindfarmGA(result, GridMethod = "h", Polygon1, whichPl = "all", best = 1, plotEn = 1)

## Plot the results of a rectangular grid optimization
result <- resultrect
Polygon1 <- polygon
PlotWindfarmGA(result, GridMethod = "r", Polygon1, whichPl = "all", best = 1, plotEn = 1)

## Plot the results with hexagonal grid cells and a weibull mean background.
a_param <- readRDS(system.file("extdata/a_weibull.RDS", package = "windfarmGA"))
k_param <- readRDS(system.file("extdata/k_weibull.RDS", package = "windfarmGA"))
weibullsrc <- list(k_param, a_param)
PlotWindfarmGA(result, GridMethod = "h", Polygon1, whichPl = "all",
               best = 1, plotEn = 1, weibullsrc = weibullsrc)


resol <- as.numeric(result[,'inputData'][[1]][,1]['Resolution'][[1]])
prop <- as.numeric(result[,'inputData'][[1]][,1]['Percentage of Polygon'][[1]])
Grid <- GridFilter(shape = Polygon1,resol = resol, prop = prop ,plotGrid=F)
plotResult(result = result, Polygon1 = Polygon1, best = 11 ,plotEn = 2,
           topographie = F,Grid= Grid[[2]]);

plotResult(result = result, Polygon1 = Polygon1, best = 3 ,plotEn = 1,
           topographie = T,Grid= Grid[[2]], weibullsrc = weibullsrc, 
           sourceCCL = "C:/Users/Bobo/Documents/Wing_GA_Pkg/__ExampleFiles/Shapefiles/g100_06.tif");

plotResult(result = result, Polygon1 = Polygon1, best = 2 ,plotEn = 2,
           topographie = T,Grid= Grid[[2]], weibullsrc = weibullsrc, 
           sourceCCL = "C:/Users/Bobo/Documents/Wing_GA_Pkg/__ExampleFiles/Shapefiles/g100_06.tif", 
           sourceCCLRoughness = "C:/Users/Bobo/Documents/Wing_GA_Pkg/__ExampleFiles/Shapefiles/clc_legend.csv");


raster::raster(x = "C:/Users/Bobo/Documents/Wing_GA_Pkg/__ExampleFiles/Shapefiles/g100_06.tif")
## Plot the results of a rectangular grid optimization
library(windfarmGA)
library(sp)
result <- resultrect
Polygon1 <- polygon
Grid <- GridFilter(Polygon1, resol = 175, 1, FALSE)
plotResult(result, Polygon1, best = 1, plotEn = 1, topographie = FALSE,
           Grid = Grid[[2]])







result <- resultrect
Polygon1 <- polygon

GooglePlot(result, Polygon1, 1, 1)
GooglePlot(result, Polygon1, 2, 1, zoom=14, maptype="hybrid",
           col="darkblue", pch=18)
GooglePlot(result, Polygon1, 3, 1, zoom=14, maptype="terrain",
           col="black", pch=20)
GooglePlot(result, Polygon1, 3, 2, zoom=15, maptype="satellite",
           col="red", pch=10)
GooglePlot(result, Polygon1, 1, 1, zoom=14, maptype="mobile",
           col="darkblue", pch=17)
GooglePlot(result, Polygon1, 1, 1, zoom=14, maptype="hybrid",
           col="darkblue", pch=20)
GooglePlot(result, Polygon1, 1, 1, zoom=14, maptype="hybrid",
           col="blue", pch=18, cex= 2)
GooglePlot(result, Polygon1, 1, 1, zoom=14, maptype="hybrid",
           col="blue", pch=18, cex= 2, size=c(320,320))

####################################
## Plotting Function for RandomSearch or calculateEn function.

# load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
# load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
# Grid <- GridFilter(shape = polygon,resol = 175, prop = 1, plotGrid = T)
# Polygon1 <- polygon
# resultRS <- RRes
# best=3
a <- RandomSearchPlot(resultRS = RRes, result = resultrect, Polygon1 = polygon, best = 3)
a


load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))

Res = RandomSearchTurb(result = resultrect, Polygon1 = polygon, n=10)
RandomSearchPlot(resultRS = Res, result = resultrect, Polygon1 = polygon, best=2)

Res = RandomSearch(resultrect, polygon, n=10, best=4)
RandomSearchPlot(resultRS = Res, result = resultrect, Polygon1 = polygon, best=2)