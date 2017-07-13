pkgname <- "windfarmGA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "windfarmGA-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('windfarmGA')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BaroHoehe")
### * BaroHoehe

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BaroHoehe
### Title: Calculates Air Density, Air Pressure and Temperature according
###   to the Barometric Height Formula
### Aliases: BaroHoehe

### ** Examples

data <- matrix(seq(0,5000,500));
BaroHoehe(data)
plot.ts(BaroHoehe(data))





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BaroHoehe", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GoogleChromePlot")
### * GoogleChromePlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GoogleChromePlot
### Title: Plot the Best Results in Google Chrome
### Aliases: GoogleChromePlot

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GoogleChromePlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GooglePlot")
### * GooglePlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GooglePlot
### Title: Plot the 'best' Results with Google background map
### Aliases: GooglePlot

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GooglePlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GridFilter")
### * GridFilter

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GridFilter
### Title: Make a grid from a Polygon
### Aliases: GridFilter

### ** Examples

library(sp)

## Exemplary input Polygon with 2km x 2km:
Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000),
c(2000, 2000), c(2000, 0)))
Polygon1 <- Polygons(list(Polygon1),1);
Polygon1 <- SpatialPolygons(list(Polygon1))
Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4string(Polygon1) <- CRS(Projection)
plot(Polygon1,axes=TRUE)

## Create a Grid
GridFilter(Polygon1,200,1,TRUE)
GridFilter(Polygon1,200,0.5,TRUE)
GridFilter(Polygon1,400,1,TRUE)
GridFilter(Polygon1,400,0.5,TRUE)


## Examplary irregular input Polygon
Polygon1 <- Polygon(rbind(c(0, 20), c(0, 200),
                          c(2000, 2000), c(3000, 0)))
Polygon1 <- Polygons(list(Polygon1),1);
Polygon1 <- SpatialPolygons(list(Polygon1))
Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4string(Polygon1) <- CRS(Projection)
plot(Polygon1,axes=TRUE)

## Create a Grid
GridFilter(Polygon1,200,1,TRUE)
GridFilter(Polygon1,200,0.5,TRUE)
GridFilter(Polygon1,200,0.1,TRUE)
GridFilter(Polygon1,400,1,TRUE)
GridFilter(Polygon1,400,0.5,TRUE)
GridFilter(Polygon1,400,0.1,TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GridFilter", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("HexaTex")
### * HexaTex

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: HexaTex
### Title: Polygon to Hexagonal Grid Tessellation
### Aliases: HexaTex

### ** Examples

library(spatstat)
library(maptools)
library(sp)
Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
                          c(4499991, 2669343), c(4499991, 2668272)))
Polygon1 <- Polygons(list(Polygon1),1);
Polygon1 <- SpatialPolygons(list(Polygon1))
Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4string(Polygon1) <- CRS(Projection)
plot(Polygon1,axes=TRUE)
HexGrid <- HexaTex(Polygon1, 100, TRUE)
plot(HexGrid[[2]])
str(HexGrid[[1]])




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("HexaTex", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("InfluPoints")
### * InfluPoints

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: InfluPoints
### Title: Find potentially influencing turbines
### Aliases: InfluPoints

### ** Examples

library(sp);library(raster)
## Exemplary input Polygon with 2km x 2km:
polYgon <- Polygon(rbind(c(0, 0), c(0, 2000),
c(2000, 2000), c(2000, 0)))
polYgon <- Polygons(list(polYgon),1);
polYgon <- SpatialPolygons(list(polYgon))
Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4string(polYgon) <- CRS(Projection); plot(polYgon,axes=TRUE)

t <- as.matrix(cbind(x=runif(10,0,raster::extent(polYgon)[2]),
     y=runif(10,0,raster::extent(polYgon)[4])))
wnkl=20
dist=100000
dirct=0

resInfluPoi <- InfluPoints(t,wnkl,dist,polYgon,dirct,plotAngles=TRUE)
str(resInfluPoi)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("InfluPoints", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PlotWindfarmGA")
### * PlotWindfarmGA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PlotWindfarmGA
### Title: Plot the results of an optimization run
### Aliases: PlotWindfarmGA

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PlotWindfarmGA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PointToLine2")
### * PointToLine2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PointToLine2
### Title: Distances between right triangle points
### Aliases: PointToLine2

### ** Examples

## For further calculations only the distances between B-C and A-C
## and the angle at A will be needed. B represents the current turbine
## and A represents a turbine, that could potentially influence turbine B.
x <- c(100,100); y <- c(500,500);
plot(rbind(x,y),col=c("red","blue"),cex=2,pch=20);
PointToLine2(x,y,TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PointToLine2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("StartGA")
### * StartGA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: StartGA
### Title: Create a random initial Population
### Aliases: StartGA

### ** Examples

library(sp)
## Exemplary input Polygon with 2km x 2km:
Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000),
c(2000, 2000), c(2000, 0)))
Polygon1 <- Polygons(list(Polygon1),1);
Polygon1 <- SpatialPolygons(list(Polygon1))
Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4string(Polygon1) <- CRS(Projection)
plot(Polygon1,axes=TRUE)

Grid <- GridFilter(Polygon1,200,1,"TRUE")

## Create 5 individuals with 10 wind turbines each.
firstPop <- StartGA(Grid = Grid[[1]], n = 10, nStart = 5)
str(firstPop)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("StartGA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("VekWinkelCalc")
### * VekWinkelCalc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: VekWinkelCalc
### Title: Calculate distances and angles of possibly influencing turbines
### Aliases: VekWinkelCalc

### ** Examples

library(sp);library(raster)

## Exemplary input Polygon with 2km x 2km:
polYgon <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
polYgon <- Polygons(list(polYgon),1);
polYgon <- SpatialPolygons(list(polYgon))
Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
               +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4string(polYgon) <- CRS(Projection); plot(polYgon,axes=TRUE)

## Create a random windfarm with 10 turbines
t <- as.matrix(cbind(x=runif(10,0,raster::extent(polYgon)[2]),
     y=runif(10,0,raster::extent(polYgon)[4])))
wnkl <- 20
distanz <- 100000

## Evaluate and plot for every turbine all other potentially influencing turbines
potInfTur <- list()
for (i in 1:(length(t[,1]))) {
  potInfTur[[i]] <- VekWinkelCalc(t = t, o = i, wkl = wnkl,
                   distanz = distanz, polYgon = polYgon, plotAngles=TRUE);
}
potInfTur




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("VekWinkelCalc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("WinkelCalc")
### * WinkelCalc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: WinkelCalc
### Title: Calculates Angles between 3 Points
### Aliases: WinkelCalc

### ** Examples

  Aa= as.numeric(cbind(1,1))
  Bb= as.numeric(cbind(10,3))
  Cc= as.numeric(cbind(10,1))
  plot(rbind(Aa,Bb,Cc,Aa), type="b", xlab="x",
       ylab="y", ylim=c(0,4), xlim=c(0,11));
  points(x=Aa[1],y=Aa[2],col="green",pch=20);
  points(x=Bb[1],y=Bb[2],col="red",pch=20);
  points(x=Cc[1],y=Cc[2],col="blue",pch=20)
  Angles <- WinkelCalc(Aa,Bb,Cc); Angles
  text(rbind(Aa,Bb,Cc),labels=round(Angles,2),pos=1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("WinkelCalc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calculateEn")
### * calculateEn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calculateEn
### Title: Calculate Energy Outputs of Individuals
### Aliases: calculateEn

### ** Examples

## Not run: 
##D ## Create a random shapefile
##D library(sp)
##D Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
##D                     c(4499991, 2669343), c(4499991, 2668272)))
##D Polygon1 <- Polygons(list(Polygon1),1);
##D Polygon1 <- SpatialPolygons(list(Polygon1))
##D Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
##D +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
##D proj4string(Polygon1) <- CRS(Projection)
##D plot(Polygon1,axes=TRUE)
##D 
##D ## Initialize a dummy wind speed raster with value 1
##D windraster <-raster::rasterize(Polygon1, raster::raster(
##D                                raster::extent(Polygon1),
##D                                ncol=180, nrow=180),field=1)
##D 
##D ## Create a uniform and unidirectional wind data.frame and plot the
##D ## resulting wind rose
##D data.in <- as.data.frame(cbind(ws=12,wd=0))
##D windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
##D                 dir = data.in$wd, dirres=10, spdmax=20)
##D 
##D ## Assign the rotor radius and a factor of the radius for grid spacing.
##D Rotor= 50; fcrR= 3
##D resGrid <- GridFilter(shape = Polygon1,resol = Rotor*fcrR, prop=1,
##D                       plotGrid =TRUE)
##D ## Assign the indexed data frame to new variable. Element 2 of the list
##D ## is the grid, saved as SpatialPolygon.
##D resGrid1 <- resGrid[[1]]
##D 
##D ## Create an initial population with the indexed Grid, 15 turbines and
##D ## 100 individuals.
##D resStartGA <- StartGA(Grid = resGrid1,n = 15,nStart = 100)
##D 
##D ## Calculate the expected energy output of the first individual of the
##D ## population.
##D par(mfrow=c(1,2))
##D plot(Polygon1); points(resStartGA[[1]]$X,resStartGA[[1]]$Y, pch=20,cex=2)
##D plot(resGrid[[2]],add=TRUE)
##D resCalcEn <-calculateEn(sel=resStartGA[[1]],referenceHeight= 50,
##D                    RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
##D                    distanz = 100000, resol = 200,dirSpeed = data.in,
##D                    RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
##D                    windraster = windraster)
##D length(resCalcEn)
##D str(resCalcEn)
##D resCalcEn <- as.data.frame(resCalcEn)
##D plot(Polygon1, main = resCalcEn$Energy_Output_Red[[1]])
##D points(x = resCalcEn$Bx, y = resCalcEn$By, pch = 20)
##D 
##D 
##D ## Create a variable and multidirectional wind data.frame and plot the
##D ## resulting wind rose
##D data.in10 <- as.data.frame(cbind(ws=runif(10,1,25),wd=runif(10,0,360)))
##D windrosePlot <- plotWindrose(data = data.in10, spd = data.in10$ws,
##D                 dir = data.in10$wd, dirres=10, spdmax=20)
##D 
##D ## Calculate the energy outputs for the first individual with more than one
##D ## wind direction.
##D resCalcEn <-calculateEn(sel=resStartGA[[1]],referenceHeight= 50,
##D                    RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
##D                    distanz = 100000, resol = 200,dirSpeed = data.in10,
##D                    RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
##D                    windraster = windraster)
##D length(resCalcEn)
##D str(resCalcEn)
##D 
##D ## Take Weibull Paramter Raster from the package. (Only for Austria)
##D plot(Polygon1); points(resStartGA[[1]]$X,resStartGA[[1]]$Y, pch=20,cex=2)
##D plot(resGrid[[2]],add=TRUE)
##D resCalcEn <-calculateEn(sel=resStartGA[[1]], referenceHeight=50,
##D                         RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
##D                         distanz = 100000, resol = 200,dirSpeed = data.in,
##D                         RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
##D                         windraster = windraster, weibull = TRUE)
##D length(resCalcEn)
##D str(resCalcEn)
##D resCalcEn <- as.data.frame(resCalcEn)
##D plot(Polygon1, main = resCalcEn$Energy_Output_Red[[1]])
##D points(x = resCalcEn$Bx, y = resCalcEn$By, pch = 20)
##D 
##D ## Use your own rasters for the Weibull parameters.
##D araster <- "/..pathto../a_param_raster.tif"
##D kraster <- "/..pathto../k_param_raster.tif"
##D weibullrasters <- list(raster(kraster), raster(araster))
##D plot(Polygon1); points(resStartGA[[1]]$X,resStartGA[[1]]$Y, pch=20,cex=2)
##D plot(resGrid[[2]],add=TRUE)
##D resCalcEn1 <-calculateEn(sel=resStartGA[[1]], referenceHeight= 50,
##D                          RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
##D                          distanz = 100000, resol = 200,dirSpeed = data.in,
##D                          RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
##D                          windraster = windraster, weibull = TRUE,
##D                          weibullsrc = weibullrasters)
##D length(resCalcEn1)
##D str(resCalcEn1)
##D resCalcEn1 <- as.data.frame(resCalcEn1)
##D plot(Polygon1, main = resCalcEn1$Energy_Output_Red[[1]])
##D points(x = resCalcEn1$Bx, y = resCalcEn1$By, pch = 20)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calculateEn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("crossover1")
### * crossover1

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: crossover1
### Title: Crossover Method
### Aliases: crossover1

### ** Examples

 ## Create two random parents with an index and random binary values
 Parents <- data.frame(cbind(ID=1:20,bin=sample(c(0,1),20,replace=TRUE,
                         prob = c(70,30)),bin.1=sample(c(0,1),20,
                         replace=TRUE,prob = c(30,70))))
 Parents

 ## Create random Fitness values for both individuals
 FitParents <- data.frame(cbind(ID=1,Fitness=1000,Fitness.1=20))
 FitParents

 ## Assign both values to a list
 CrossSampl <- list(Parents,FitParents);
 str(CrossSampl)

 ## Cross their data at equal locations with 2 crossover parts
 crossover1(CrossSampl, u=1.1, uplimit=300, crossPart = "EQU")

 ## with 3 crossover parts and equal locations
 crossover1(CrossSampl, u=2.5, uplimit=300, crossPart = "EQU")

 ## or with random locations and 5 crossover parts
 crossover1(CrossSampl, u=4.9, uplimit=300, crossPart = "RAN")





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("crossover1", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("euc.dist")
### * euc.dist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: euc.dist
### Title: Euclidian Distance between two Points
### Aliases: euc.dist

### ** Examples

x=c(200,100)
y=c(1000,2000)
euc.dist(x,y)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("euc.dist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fitness")
### * fitness

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fitness
### Title: Evaluate the Individual Fitness values
### Aliases: fitness

### ** Examples





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fitness", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("genAlgo")
### * genAlgo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: genAlgo
### Title: Start The Genetic Algorithm for a wind Farm Layout
### Aliases: genAlgo

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("genAlgo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getRects")
### * getRects

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getRects
### Title: Get the Grid-IDs from binary matrix
### Aliases: getRects

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getRects", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("heatmapGA")
### * heatmapGA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: heatmapGA
### Title: Plot heatmap of fit grid cells
### Aliases: heatmapGA

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("heatmapGA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("leafPlot")
### * leafPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: leafPlot
### Title: Leaflet Plot of a Wind Park
### Aliases: leafPlot

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("leafPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mutation")
### * mutation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mutation
### Title: Mutation Method
### Aliases: mutation

### ** Examples

## Create 4 random individuals with binary values
a <- cbind(bin=sample(c(0,1),20,replace=TRUE,prob = c(70,30)),
        bin.1=sample(c(0,1),20,replace=TRUE,prob = c(30,70)),
        bin.2=sample(c(0,1),20,replace=TRUE,prob = c(30,70)),
        bin.3=sample(c(0,1),20,replace=TRUE,prob = c(30,70)))
a

## Mutate the individuals with a low percentage
aMut <- mutation(a,0.1)
## Check which values are not like the originals
a==aMut

## Mutate the individuals with a high percentage
aMut <- mutation(a,0.4)
## Check which values are not like the originals
a==aMut




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mutation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotCloud")
### * plotCloud

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotCloud
### Title: Plot outputs of all generations with standard deviations
### Aliases: plotCloud

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotCloud", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotEvolution")
### * plotEvolution

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotEvolution
### Title: Plot the evolution of fitness values
### Aliases: plotEvolution

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotEvolution", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotResult")
### * plotResult

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotResult
### Title: Plot the best Results
### Aliases: plotResult

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotResult", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotWindrose")
### * plotWindrose

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotWindrose
### Title: Plot a Windrose
### Aliases: plotWindrose

### ** Examples

## Exemplary Input Wind speed and direction data frame
# Uniform wind speed and single wind direction
data.in <- as.data.frame(cbind(ws=12,wd=0))
windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
   dir = data.in$wd)

# Random wind speeds and random wind directions
data.in <- as.data.frame(cbind(ws=sample(1:25,10),wd=sample(1:260,10)))
windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
   dir = data.in$wd)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotWindrose", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotbeorwor")
### * plotbeorwor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotbeorwor
### Title: Plot if previous population was better or worse
### Aliases: plotbeorwor

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotbeorwor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotfitnessevolution")
### * plotfitnessevolution

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotfitnessevolution
### Title: Plot the changes of min/mean/max fitness values
### Aliases: plotfitnessevolution

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotfitnessevolution", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotparkfitness")
### * plotparkfitness

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotparkfitness
### Title: Plot the genetic algorithm results
### Aliases: plotparkfitness

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotparkfitness", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readinteger")
### * readinteger

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readinteger
### Title: Check Input Crossover Method
### Aliases: readinteger

### ** Examples





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readinteger", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readintegerSel")
### * readintegerSel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readintegerSel
### Title: Check Input Selection Method
### Aliases: readintegerSel

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readintegerSel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("selection1")
### * selection1

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: selection1
### Title: Selection Method
### Aliases: selection1

### ** Examples

## Not run: 
##D ## Create a random rectangular shapefile
##D library(sp)
##D Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
##D Polygon1 <- Polygons(list(Polygon1),1);
##D Polygon1 <- SpatialPolygons(list(Polygon1))
##D Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
##D +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
##D proj4string(Polygon1) <- CRS(Projection)
##D plot(Polygon1,axes=TRUE)
##D 
##D ## Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
##D Grid1 <- GridFilter(shape = Polygon1,resol = 200,prop = 1);
##D Grid <- Grid1[[1]]
##D AmountGrids <- nrow(Grid)
##D 
##D startsel <- StartGA(Grid,10,20);
##D wind <- as.data.frame(cbind(ws=12,wd=0))
##D fit <- fitness(selection = startsel,referenceHeight = 100, RotorHeight=100,
##D                SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,rot=20, dirspeed = wind,
##D                srtm_crop="",topograp=FALSE,cclRaster="")
##D allparks <- do.call("rbind",fit);
##D 
##D ## SELECTION
##D ## print the amount of Individuals selected. Check if the amount of Turbines is as requested.
##D selec6best <- selection1(fit, Grid,2, T, 6, "VAR");
##D selec6best <- selection1(fit, Grid,2, T, 6, "FIX");
##D selec6best <- selection1(fit, Grid,4, F, 6, "FIX");
##D str(selec6best)
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("selection1", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("splitAt")
### * splitAt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: splitAt
### Title: Divide matrices or integer at certain locations
### Aliases: splitAt

### ** Examples

splitAt(1:100,20)
splitAt(as.matrix(1:100),20)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("splitAt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tess2SPdf")
### * tess2SPdf

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tess2SPdf
### Title: Create a Tesselation from a Polygon
### Aliases: tess2SPdf

### ** Examples

library(spatstat)
library(maptools)
library(sp)
Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
                          c(4499991, 2669343), c(4499991, 2668272)))
Polygon1 <- Polygons(list(Polygon1),1);
Polygon1 <- SpatialPolygons(list(Polygon1))
Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4string(Polygon1) <- CRS(Projection)
plot(Polygon1,axes=TRUE)
## Create a hexagonal Tesselation
HexaGrid <- spatstat::hextess(maptools::as.owin.SpatialPolygons(Polygon1),s = 100)
plot(HexaGrid)
HexaGrid
## Convert the Tesselation to SpatialPolygons
Hex2spdf <- tess2SPdf(HexaGrid)
plot(Hex2spdf)
Hex2spdf




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tess2SPdf", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("trimton")
### * trimton

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: trimton
### Title: Adjust the amount of turbines per windfarm
### Aliases: trimton

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("trimton", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("windfarmGA")
### * windfarmGA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: windfarmGA
### Title: Controls the given inputs and initiates an Optimization run
### Aliases: windfarmGA

### ** Examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("windfarmGA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
