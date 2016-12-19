#' @title Initiating Optimization File
#' @name InitFile
#' @description  The initiating file, where all needed and obligatory
#' input variables are assigned and an optimization run is started.
#' To use this file, first uncomment the lines from 56 untill the end.
#' If all sections in this file are collapsed, it is split in 4 parts,
#' the first 2 are for the input area and the input wind speeds.
#'     Section 3 starts an optimization run with the given inputs.
#'     Section 4 plots the results of the optimization run.
#'
#' @export
#'
#' @importFrom rgdal readOGR
#' @importFrom sp Polygon SpatialPolygons CRS proj4string
#'
#' @param Polygon1 The considered area as shapefile. (SpatialPolygons)
#' @param sourceCCL The source to the Corine Land Cover raster (character)
#' @param sourceCCLRoughness The source to the adapted
#' Corine Land Cover legend as csv file. (character)
#' @param data.in A data.frame containing the incoming wind speeds,
#' wind directions and probabilities. (data.frame)
#' @param n A numeric value indicating the required amount of turbines.
#' (numeric)
#' @param Rotor A numeric value that gives the rotor radius in meter.
#' (numeric)
#' @param fcrR A numeric value, that is used for grid spacing. (numeric)
#' @param iteration A numeric value indicating the desired amount of
#' iterations of the algorithm. (numeric)
#' @param vdirspe A data.frame for the incoming wind directions. (data.frame)
#'
#' @details Assigns the needed input values and starts an optimization run.
#' The result of this run is a matrix of all relevant output parameters.
#' This output is used for several plotting functions.
#'
#' @examples \donttest{
#' library(sp);
#'
#' ## Exemplary input Polygon with 2km x 2km:
#' Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000),
#' c(2000, 2000), c(2000, 0)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' plot(Polygon1,axes=T)
#'
#' library(rgdal);
#' ## Or Polygon is given by source:
#' Polygon1 <- rgdal::readOGR(dsn="C:/Users/.../Shapefiles",
#'            layer="NameOfPolygon")
#' plot(Polygon1)
#'
#' ## If terrain model is activated, the source of the Corine Land Cover
#' ## raster and of the adapted CLC-legend csv file must be given
#' sourceCCL <- "C:/Users/.../g100_06.tif"
#' sourceCCLRoughness <- "C:/Users/.../clc_legend.csv""
#'
#' library(GenAlgo);
#' ## Exemplary Input Wind speed and direction data frame
#' # Uniform wind speed and single wind direction
#' data.in <- structure(list(ws =  c(12,12), wd = c(0,0),
#'          probab = c(25,25)), .Names = c( "ws", "wd","probab"),
#'          row.names = c(NA, 2L), class = "data.frame")
#' # windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
#' #                dir = data.in$wd, dirres=10, spdmax=20)
#'
#' # Random wind speeds and random wind directions
#' data.in <- as.data.frame(cbind(ws=sample(1:25,10),wd=sample(1:260,10)))
#' # windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
#' #                dir = data.in$wd, dirres=10, spdmax=20)
#'
#' # Starting an optimization run
#' #result <- genAlgo(Polygon1=Polygon1, n=12, Rotor=30, fcrR=3, iteration=10,
#' #          vdirspe=data.in)
#' }
#' @author Sebastian Gatscha

# ############### REQUIRED INPUT POLYGON AND CCL SOURCE ################
# library(rgdal);library(sp); library(GenAlgo)
# # Input the Source of the desired Polygon in the Variable dns, as in this example: dns = "C:/User/Documents/Shapefiles"
# Polygon1 <- rgdal::readOGR(dsn="Source of the Polygon File (SHP)", layer="Name of Polygon"); plot(Polygon1)


# # The following two sources are required, if terrain effects should be considered
# sourceCCL <- "Source of the CCL raster (TIF)"
# sourceCCLRoughness <- "Source of the Adaped CCL legend (CSV)"
#
#
# ############### REQUIRED INPUT WIND SPEED DATA FRAME  ################
# # Exemplary Input Wind speed and direction data frame
#
# # Uniform wind speed and single wind direction
# data.in <- structure(list(ws =  c(12,12), wd = c(0,0), probab = c(25,25)), .Names = c( "ws", "wd","probab"),row.names = c(NA, 2L), class = "data.frame")
# windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,dir = data.in$wd, dirres=10, spdmax=20)
#
# # Random wind speeds and random wind directions
# data.in <- as.data.frame(cbind(ws=sample(1:25,10),wd=sample(1:260,10)))
# windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,dir = data.in$wd, dirres=10, spdmax=20)
#
#

# ############### RUNNING GENETIC ALGORITHM  ###################
# # First check if the grid size is convenient
# Rotor= 30
# fcrR= 3
# GridFilter(shape = Polygon1,resol = (Rotor*fcrR), prop=1, plotGrid = TRUE)
# result <- genAlgo(Polygon1 = Polygon1, n=12, Rotor=30,fcrR=3,iteration=100,vdirspe = data.in)


# ############### PLOTTING OUTPUTS ###################
# plotEvolution(result,T,0.3)
# plotResult(resultMa = result, Polygon1 = Polygon1, best = 1 ,plotEn = 2,topographie = FALSE);
# plotparkfitness(result,0.1)
# plotfitnessevolution(result)
# plotcloud(result,"TRUE")
# plotbeorwor(result)
# GooglePlot(result,Polygon1,Projection)
# GoogleChromePlot(result,Polygon1,1,1)
# heatmapGA(result,si=5,Polygon1 = Polygon1)



