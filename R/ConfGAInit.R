#' @title Add the required inputs and initiate an optimization run
#' @name ConfGAInit
#' @description  The initiating file, where all needed and obligatory
#' input variables are assigned and an optimization run is started.
#' To use this file, first uncomment the lines from 56 untill the end.
#' If all sections in this file are collapsed, it is split in 4 parts,
#' the first 2 are for the input area and the input wind speeds.
#' Section 3 starts an optimization run with the given inputs.
#' Section 4 plots the results of the optimization run.
#'
#' @export
#'
#' @importFrom rgdal readOGR
#' @importFrom sp Polygon SpatialPolygons CRS proj4string
#' @importFrom raster plot
#' @importFrom utils globalVariables
#'
#' @param dns The source to the desired Shapefile. Only required, if the
#' shapefile is not already loaded. (character)
#' @param layer The name of the desired Shapefile. Only required, if the
#' shapefile is not already loaded. (SpatialPolygons)
#' @param Polygon1 The considered area as shapefile. Only required, if the
#' shapefile is already loaded.(SpatialPolygons)
#' @param sourceCCL The source to the Corine Land Cover raster. Only
#' required, when terrain effect model is activated. (character)
#' @param sourceCCLRoughness The source to the adapted
#' Corine Land Cover legend as csv file. Only required, when terrain
#' effect model is activated.(character)
#' @param data.in A data.frame containing the incoming wind speeds,
#' wind directions and probabilities. See also: \code{\link{plotWindrose}}
#' (data.frame)
#' @param n A numeric value indicating the required amount of turbines.
#' (numeric)
#' @param Rotor A numeric value that gives the rotor radius in meter.
#' (numeric)
#' @param fcrR A numeric value, that is used for grid spacing.
#' See also: \code{\link{GridFilter}} (numeric)
#' @param iteration A numeric value indicating the desired amount of
#' iterations of the algorithm. (numeric)
#' @param topograp Boolean value that indicates whether the terrain effect
#' model is activated ("TRUE") or deactivated ("FALSE"). (character)
#' @param referenceHeight The height at which the incoming
#' wind speeds were measured. Default is 50m (numeric)
#' @param RotorHeight The desired height of the turbine.
#' Default is 100m (numeric)
#' @param SurfaceRoughness A surface roughness length of the
#' considered area in m. (numeric)
#' @param Proportionality A numeric factor used for grid calculation.
#' Determines the percentage a grid has to overlay.
#' See also: \code{\link{GridFilter}} (numeric)
#' @param mutr A numeric mutation rate, with low default value of 0.008
#' (numeric)
#' @param elitism Boolean Value, which indicates whether elitism should
#' be included or not. (character)
#' @param nelit If \code{elitism} is "TRUE", then this input variable
#' determines the amount of individuals in the elite group. (numeric)
#' @param selstate Determines which selection method is used,
#' "FIX" selects a constant percentage and "VAR" selects a variable percentage,
#' depending on the development of the fitness values. (character)
#' @param crossPart1 Determines which crossover method is used,
#' "EQU" divides the genetic code at equal intervals and
#' "RAN" divides the genetic code at random locations. (character)
#' @param trimForce If activated (\code{trimForce}=="TRUE"),
#' the algorithm will take a probabilistic approach to trim the windfarms
#' to the desired amount of turbines. If \code{trimForce}=="FALSE" the
#' adjustment will be random. Default is "TRUE". (character)
#' @param plotRes Logical value indicating whether the results should be
#' plotted with a series of plots or not. Default is TRUE. (logical)
#'
#' @details Assigns the needed input values and starts an optimization run.
#' The result of this run is a matrix of all relevant output parameters.
#' This output is used for several plotting functions.
#'
#' @examples \donttest{
#' ############### REQUIRED INPUT POLYGON AND CCL SOURCE
#'library(rgdal);library(sp);
#'Polygon1 <- sp::Polygon(rbind(c(0, 0), c(0, 2000),
#'                              c(2000, 2000), c(2000, 0)))
#'Polygon1 <- sp::Polygons(list(Polygon1),1);
#'Polygon1 <- sp::SpatialPolygons(list(Polygon1))
#'Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#'+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#'proj4string(Polygon1) <- CRS(Projection)
#'plot(Polygon1,axes=T)
#
#'## Or Polygon is given by source:
#'Polygon1 <- rgdal::readOGR(dsn=dsn,layer=layer)
#'plot(Polygon1)
#
#' dsn="Source of the Polygon File (SHP)"
#' layer="Name of Polygon"
#' ## The following two sources are required, if terrain effects should
#' ## be considered
#' sourceCCL <- "Source of the CCL raster (TIF)"
#' sourceCCLRoughness <- "Source of the Adaped CCL legend (CSV)"
#
#' ############### REQUIRED INPUT WIND SPEED DATA FRAME
#' ## Exemplary Input Wind speed and direction data frame
#' ## Uniform wind speed and single wind direction
#' data.in <- structure(list(ws =  c(12,12), wd =c(0,0), probab =c(25,25)),
#' .Names = c( "ws", "wd","probab"),row.names = c(NA, 2L),class ="data.frame")
#' windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
#' dir = data.in$wd, dirres=10, spdmax=20)
#
#' ##First check if the grid size is convenient
#' Rotor= 50; fcrR= 9
#' GridFilter(shape = Polygon1,resol = (Rotor*fcrR), prop=1, plotGrid ="TRUE")
#'
#' ## Starting an optimization run
#' result <- genAlgo(Polygon1=Polygon1, n=12, Rotor=30, fcrR=3, iteration=10,
#'           vdirspe=data.in)
#'
#'##########
#' }
#' @author Sebastian Gatscha

utils::globalVariables(c("X","Y","X1","X2","var1.pred","x",
                         "y","EfficAllDir","sourceCCLRoughness","sourceCCL","RotorR",
                         "Punkt_id","A_ov","WakeR","Windmean","Laenge_A","Laenge_B",
                         "Ay","Ax","V_i","V_red","Rotorflaeche","AbschatInProz",
                         "Windrichtung","alpha","Laenge_A","Laenge_B","By","Bx",
                         "dry.grid.filtered","spd.binned","Run","EnergyOverall",
                         "ID", "bin", "Fitness","Rect_ID", "Parkfitness", "AbschGesamt"));


ConfGAInit <- function(dns,layer,Polygon1,sourceCCL,sourceCCLRoughness,
                       data.in,Rotor=30,fcrR=3,n=10,topograp=FALSE,
                       iteration=100,plotRes=TRUE,referenceHeight=50,
                       RotorHeight=50,SurfaceRoughness=0.14, Proportionality=1,
                       mutr=0.001, elitism="TRUE", nelit=6,
                       selstate="FIX", crossPart1="EQU",trimForce="TRUE"){



  if (!missing(dns) & !missing(layer)){
    # Input the Source of the desired Polygon
    Polygon1 <- rgdal::readOGR(dsn=dns, layer=layer);
    plot(Polygon1)
  }
  # dev.new()

  plot.new()
  Grid <- GridFilter(shape = Polygon1,resol = (Rotor*fcrR), prop = Proportionality,plotGrid="TRUE")
  print("Is the grid spacing appropriate?")
  InputDaccor <- readline(prompt =
      "Type 'y' if the the grid is corrent and 'n' if you like to change some intputs.")
  if  (InputDaccor== "n") {
    stop()
  }


  print("1.Komm ich hierher?")
  ############### RUNNING GENETIC ALGORITHM
  result <- genAlgo(Polygon1 = Polygon1, Rotor = Rotor, n=n, fcrR=fcrR, iteration=iteration, vdirspe = data.in,
                    topograp = topograp,referenceHeight = referenceHeight,RotorHeight = RotorHeight,
                    SurfaceRoughness = SurfaceRoughness,Proportionality = Proportionality,mutr = mutr,
                    elitism = elitism,nelit = nelit,selstate = selstate,crossPart1 = crossPart1,trimForce = trimForce)


  if (plotRes == TRUE){
    ############### PLOTTING OUTPUTS
    plotEvolution(result,T,0.3)
    plotResult(result = result, Polygon1 = Polygon1, best = 1 ,plotEn = 2,topographie = "FALSE",Grid= Grid[[2]]);
    plotparkfitness(result,0.1)
    plotfitnessevolution(result)
    plotCloud(result,"TRUE")
    GooglePlot(result,Polygon1)
    GoogleChromePLot(result,Polygon1,1,1)
    heatMap(result,si=5,Polygon1 = Polygon1)
  }
}


# ResOut <- ConfGAInit(Polygon1 = Polygon1,n = 10,data.in = data.in,
#                       Rotor = 50,fcrR = 6, iteration = 10)





