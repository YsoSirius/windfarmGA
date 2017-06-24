#' @title Plot the best Results in Google Chrome
#' @name GoogleChromePlot
#' @description  Plot the best energy or efficiency solutions in Google Chrome
#' with a satellite background image of Google maps. Input Polygon is
#' not plotted
#'
#' @export
#'
#' @importFrom googleVis gvisMap
#'
#' @param result The output matrix of \code{\link{windfarmGA}} or
#' \code{\link{genAlgo}}, which has stored all relevant information.
#' (matrix)
#' @param Polygon1 The considered area as shapefile (SpatialPolygons)
#' @param best A numeric value indicating the best individual
#' to be plotted. 1 will indicate the solution with highest value. (numeric)
#' @param plotEn A numeric value that indicates if the best energy or
#' efficiency output should be plotted. (plotEn==1) will plot the best
#' energy solution and (plotEn==2) will plot the best efficiency solution.
#' (numeric)
#' @param Projection A desired Projection can be used instead
#' of the default Lambert Azimuthal Equal Area Projection. (character)
#'
#' @return NULL
#' @examples \donttest{
#' ## Create a random rectangular shapefile
#' library(sp)
#' Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
#'                           c(4499991, 2669343), c(4499991, 2668272)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' plot(Polygon1,axes=TRUE)
#'
#' ## Create a uniform and unidirectional wind data.frame and plots the
#' ## resulting wind rose
#' ## Uniform wind speed and single wind direction
#' data.in <- as.data.frame(cbind(ws=12,wd=0))
#' # windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
#' #                dir = data.in$wd, dirres=10, spdmax=20)
#'
#' ## Runs an optimization run for 10 iterations (iteration) with the
#' ## given shapefile (Polygon1), the wind data.frame (data.in),
#' ## 12 turbines (n) with rotor radii of 30m (Rotor) and a grid spacing
#' ## factor of 3 (fcrR) and other required inputs.
#' result <- genAlgo(Polygon1 = Polygon1, n=12, Rotor=20,fcrR=3,iteration=10,
#'              vdirspe = data.in,crossPart1 = "EQU",selstate="FIX",mutr=0.8,
#'             Proportionality = 1, SurfaceRoughness = 0.3, topograp = FALSE,
#'             elitism=TRUE, nelit = 7, trimForce = TRUE,
#'             referenceHeight = 50,RotorHeight = 100)
#'
#' GoogleChromePlot(result, Polygon1, 1,1)
#'}
#' @author Sebastian Gatscha
GoogleChromePlot <- function(result,Polygon1,best=1,plotEn=1,Projection) {

  a <- GooglePlot(result,Polygon1,best,plotEn,Projection,plT=F)
  a$latlon <- paste(a$lat, a$lon, sep=":")
  map.gb <- googleVis::gvisMap(a, locationvar="latlon",chartid = "Map",
                               options = list(showTip=T, showLine=T, enableScrollWheel=TRUE,mapType="hybrid",
                                   useMapTypeControl=T, width=1400,
                                   height=800,icons=paste0("{","'default': {'normal': 'http://icons.iconarchive.com/",
                                                               "icons/icons-land/vista-map-markers/48/",
                                                               "Map-Marker-Ball-Azure-icon.png',\n",
                                                               "'selected': 'http://icons.iconarchive.com/",
                                                               "icons/icons-land/vista-map-markers/48/",
                                                               "Map-Marker-Ball-Right-Azure-icon.png'","}}")))
  plot(map.gb)
}
