#' @title Controlls the given inputs and starts an Optimization run
#' @name windfarmGA
#' @description  The initiating function of an optimization process, which
#' will interactively check user-inputs first. If all inputs are correct,
#' an optimization run will be started. This process can take a long time,
#' depending on the size of the problem and the number of desired iterations.
#'
#' @details A terrain effect model can be included in the optimization process.
#' Therefore an SRTM elevation model will be downloaded automaticelly via the
#' \code{raster::getData} function. Another raster (.tif) is
#' \strong{required}, which has to be downloaded previously at the following
#' page: \url{http://www.eea.europa.eu/data-and-maps/data/clc-2006-raster-1}.
#' Download the .zip package with \strong{100 meter} resolution.
#' Unzip the downloaded package and assign the source of the Raster Image
#' \file{"g100_06.tif"} in the package to the input variable \code{sourceCCL}.
#' The algorithm will use an adapted version of the Raster legend
#' ("clc_legend.csv"), which is stored in the package subdirectory
#' \file{~/extdata}. To use own values for the land cover roughness
#' lengths, insert a column named \strong{"Rauhigkeit_z"} to the .csv file
#' in the Corine Land Cover package, assign a surface roughness lenght
#' to all land cover types. Be sure that all rows are filled with numeric
#' values and save the csv file then with \strong{";"} separation. Assign the
#' source of the resulting .csv file to the input variable
#' \code{sourceCCLRoughness} of this function. For further information, see
#' the examples.
#'
#'
#'
#' @export
#' @importFrom rgdal readOGR
#' @importFrom sp Polygon SpatialPolygons CRS proj4string spTransform
#' @importFrom raster plot crs
#' @importFrom utils globalVariables
#'
#' @param dns The source to the desired Shapefile. Only required, if the
#' shapefile is not already loaded. (character)
#' @param layer The name of the desired Shapefile. Only required, if the
#' shapefile is not already loaded. (character)
#' @param Polygon1 The considered area as shapefile. Only required, if the
#' shapefile is already loaded.(SpatialPolygons)
#' @param sourceCCL The source to the Corine Land Cover raster (.tif). Only
#' required, when the terrain effect model is activated. (character)
#' @param sourceCCLRoughness The source to the adapted
#' Corine Land Cover legend as .csv file. Only required, when terrain
#' effect model is activated. As default a .csv file within this
#' package (\file{~/extdata}) is taken that was already adapted
#' manually. To use your own .csv legend this variable has to be assigned.
#' See Details. (character)
#' @param data.in A data.frame containing the incoming wind speeds,
#' wind directions and probabilities. To plot a wind rose from this
#' data frame, see: \code{\link{plotWindrose}}. (data.frame)
#' @param n A numeric value indicating the required amount of turbines.
#' (numeric)
#' @param Rotor A numeric value that gives the rotor radius in meter.
#' (numeric)
#' @param fcrR A numeric value, that is used for grid spacing. The resolution
#' of a grid cell will be the rotor radius \code{Rotor} multiplied with this
#' value. (numeric)
#' @param iteration A numeric value indicating the desired amount of
#' iterations of the algorithm. (numeric)
#' @param topograp Logical value that indicates whether the terrain effect
#' model is activated (TRUE) or deactivated (FALSE). (logical)
#' @param referenceHeight The height at which the incoming
#' wind speeds were measured. Default is 50m (numeric)
#' @param RotorHeight The desired height of the turbine.
#' Default is 100m (numeric)
#' @param SurfaceRoughness A surface roughness length of the
#' considered area in m. If the terrain effect model is activated, a
#' surface roughness will be calculated for every grid cell with the
#' elevation and land cover information. (numeric)
#' @param Proportionality A numeric factor used for grid calculation.
#' Determines the percentage a grid has to overlay.
#' See also: \code{\link{GridFilter}} (numeric)
#' @param mutr A numeric mutation rate, with low default value of 0.008
#' (numeric)
#' @param elitism Boolean Value, which indicates whether elitism should
#' be included or not. (logical)
#' @param nelit If \code{elitism} is TRUE, then this input variable
#' determines the amount of individuals in the elite group. (numeric)
#' @param selstate Determines which selection method is used,
#' "FIX" selects a constant percentage and "VAR" selects a variable percentage,
#' depending on the development of the fitness values. (character)
#' @param crossPart1 Determines which crossover method is used,
#' "EQU" divides the genetic code at equal intervals and
#' "RAN" divides the genetic code at random locations. (character)
#' @param trimForce If activated (\code{trimForce}==TRUE),
#' the algorithm will take a probabilistic approach to trim the wind farms
#' to the desired amount of turbines. If \code{trimForce}==FALSE the
#' adjustment will be random. Default is TRUE. (logical)
#' @param Projection A desired Projection can be used instead
#' of the default Lambert Azimuthal Equal Area Projection. (character)
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
#' GridFilter(shape = Polygon1,resol = (Rotor*fcrR), prop=1, plotGrid =TRUE)
#'
#' ############### STARTING AN OPTIMIZATION RUN
#' result <- genAlgo(Polygon1=Polygon1, n=12, Rotor=30, fcrR=3, iteration=10,
#'           vdirspe=data.in)
#'
#' ## Use the resulting list with the different plotting methods of this package,
#' ## to explore the behaviour of the genetic algorithm
#'
#'##########
#' }
#' @author Sebastian Gatscha

windfarmGA <- function(dns,layer,Polygon1,Projection,sourceCCL,sourceCCLRoughness,
                       data.in,Rotor=30,fcrR=3,n=10,topograp=FALSE,
                       iteration=100,referenceHeight=50,
                       RotorHeight=50,SurfaceRoughness=0.14, Proportionality=1,
                       mutr=0.001, elitism=TRUE, nelit=6,
                       selstate="FIX", crossPart1="EQU",trimForce=TRUE){

  utils::globalVariables(c("X","Y","X1","X2","var1.pred","x",
                           "y","EfficAllDir","sourceCCLRoughness","sourceCCL","RotorR",
                           "Punkt_id","A_ov","WakeR","Windmean","Laenge_A","Laenge_B",
                           "Ay","Ax","V_i","V_red","Rotorflaeche","AbschatInProz",
                           "Windrichtung","alpha","Laenge_A","Laenge_B","By","Bx",
                           "dry.grid.filtered","spd.binned","Run","EnergyOverall",
                           "ID", "bin", "Fitness","Rect_ID", "Parkfitness", "AbschGesamt"));

  ##########################################################
  ######## CHECK INPUT POLYGON
  ## Check if Polygon given is correctly
  if (!missing(Polygon1)){
    plot(Polygon1, col="red",main ="Original Input Shapefile");
    title(sub=Polygon1@proj4string,line = 1)
    readline(prompt = "\nPress <ENTER> if this is your Polygon")
  }
  ## Load Polygon from a Source File. Just if dns and layer are not missing.
  if (!missing(dns) & !missing(layer)){
    # Input the Source of the desired Polygon
    Polygon1 <- rgdal::readOGR(dsn=dns, layer=layer);
    plot(Polygon1,col="red",main ="Original Input Shapefile");
    title(sub=Polygon1@proj4string,line = 1)
    readline(prompt = "\nPress <ENTER> if this is your Polygon")
  }
  ##  Project the Polygon to LAEA if it is not already.
  if (missing(Projection)) {
    cat("Projection missing. Take Lambert Azimuthal Equal Area Projection.\n")
    Projection = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  } else {
    Projection <- Projection;
  }
  if (as.character(raster::crs(Polygon1)) != Projection) {
    Polygon1 <- sp::spTransform(Polygon1, CRSobj = Projection);
    plot(Polygon1, col="red",main ="Projected Input Shapefile");
    title(sub=Polygon1@proj4string,line = 1)
    readline(prompt = "\nPress <ENTER> if this is your Polygon")
  }

  ######## CHECK INPUT GENETIC ALGORITHM
  ## Check if Crossover Method is chosen correctly.
  if (missing(crossPart1)) {crossPart1 <- readinteger()}
  if  (crossPart1!= "EQU" & crossPart1 !="RAN") {
    crossPart1 <- readinteger()
  }
  ## Check if Selection Method is chosen correctly.
  if (missing(selstate)) {selstate <- readinteger()}
  if  (selstate!= "FIX" & selstate !="VAR") {
    selstate <- readintegerSel()
  }

  ## Check if Input Wind data is given correctly.
  if (missing(data.in)) {
    stop("\n##### No wind data.frame is given. \nThis input is required for an optimization run")
  }
  plot.new();   plotWindrose(data = data.in, spd = data.in$ws,dir = data.in$wd)
  readline(prompt = "\nPress <ENTER> if the windrose looks correct?")
  ## Check if Rotor,fcrR,n,iteration,RotorHeight,SurfaceRoughness,Proportionality,mutr,nelit are numeric
  ChekNumer <- is.numeric(c(Rotor,n,fcrR,iteration,referenceHeight,RotorHeight,SurfaceRoughness,Proportionality,mutr,nelit))
  if (ChekNumer==F) {
    cat("################### GA WARNING MESSAGE ###################")
    stop("\n##### A required numeric input is not numeric.\n
         See documentation.")
  }
  ## Check if topograp,elitism,trimForce are logical
  ChekLogic <- is.logical(c(topograp,elitism,trimForce))
  if (ChekLogic==F) {
    cat("################### GA WARNING MESSAGE ###################")
    stop("\n##### A required logical input is not logical.\n
         See documentation.")
  }
  ## Check if Grid with given inputs is correctly
  plot.new(); Grid <- GridFilter(shape = Polygon1,resol = (Rotor*fcrR), prop = Proportionality,plotGrid=TRUE)
  cat("\nIs the grid spacing appropriate?")
  InputDaccor <- readline(prompt = "Type 'ENTER' if the the grid is corrent and 'n' if you like to change some inputs.")
  InputDaccor <- tolower(InputDaccor)
  if  (InputDaccor== "n") {
    cat("################### GA WARNING MESSAGE ###################")
    stop("\n##### The grid spacing is not as required. \n
             Adjust radius (Rotor) or fraction of the radius (fcrR) or the reference system.\n
             You can also use the function GridFilter first, to see the resulting grid with given inputs.\n
             Be carefull, with GridFilter you have to give a resolution as input, in this case this will be fcrR*Rotor.
             ")
  }



  ##########################################################
  ############### RUNNING GENETIC ALGORITHM
  result <- genAlgo(Polygon1 = Polygon1, Rotor = Rotor, n=n, fcrR=fcrR, iteration=iteration, vdirspe = data.in,
                    topograp = topograp,referenceHeight = referenceHeight,RotorHeight = RotorHeight,
                    SurfaceRoughness = SurfaceRoughness,Proportionality = Proportionality,mutr = mutr,
                    elitism = elitism,nelit = nelit,selstate = selstate,crossPart1 = crossPart1,trimForce = trimForce,
                    Projection=Projection)
invisible(result)
}



