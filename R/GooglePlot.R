#' @title Plot the \code{best} Results with Google background map
#' @name GooglePlot
#' @description  Plot the best energy or efficiency solutions with a
#' background image of Google maps.
#'
#' @export
#'
#' @importFrom sp spTransform proj4string SpatialPoints SpatialPolygons
#' @importFrom dplyr select
#' @importFrom RgoogleMaps GetMap PlotOnStaticMap PlotPolysOnStaticMap
#' @importFrom raster extent crs
#' @importFrom rgeos gCentroid
#'
#' @param result The output matrix of \code{\link{windfarmGA}} or
#' \code{\link{genAlgo}}, which has stored all relevant information.
#' (matrix)
#' @param Polygon1 The considered area as shapefile (SpatialPolygons)
#' @param best A numeric value indicating which best individuals
#' to be plotted. 1 will indicate the solution with highest value. (numeric)
#' @param plotEn A numeric value that indicates if the best energy or
#' efficiency output should be plotted. (plotEn==1) will plot the best
#' energy solution and (plotEn==2) will plot the best efficiency solution.
#' (numeric)
#' @param Projection A desired Projection can be used instead
#' of the default Lambert Azimuthal Equal Area Projection. (character)
#' @param ... If the plot is not required, it can be disabled with
#' the option \code{plT = FALSE}
#'
#' @return Returns a data.frame with the coordinates in LON/LAT and plots
#' the desired best result with a google background map. (data.frame)
#'
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
#' GooglePlot(result, Polygon1, 1,1)
#'}
#' @author Sebastian Gatscha
GooglePlot <- function(result,Polygon1,best=1,plotEn=1, Projection,...){

  ## Set the graphical parameters
  op <- par(ask=F)
  on.exit(par(op))
  par(mfrow=c(1,1))

  ## Set the splitting parameters
  if (plotEn == 1) {
    en = "EnergyOverall"
  }
  if (plotEn == 2) {
    en = "Parkfitness"
  }

  ## Split resulting individuals
  result <- result[,2][order(as.character(sapply(result[,2], "[", en)),decreasing = T)]
  result <- result[best]
  Solution <- do.call("rbind", result)

  ## Input reference systems
  if (missing(Projection)) {
    ProjLAEA = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  } else {
    ProjLAEA <- Projection;
  }

  ProjLonLat <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"
  ## If Polygon is not already in Lon/Lan it will be projected
  if (sp::proj4string(Polygon1)!=(ProjLonLat)){
    # cat("Polygon is not projected in LatLon.")
    Polygon1 <- sp::spTransform(Polygon1, CRSobj = raster::crs(ProjLonLat))
  }

  ## Take the resulting individual coordinates and project them to Lat/Lon. Used for Google Mapss
  PointSol <- data.frame(dplyr::select(Solution,X,Y)); names(PointSol) <- c("lon","lat")
  PointSol1 <- sp::SpatialPoints(sp::coordinates(PointSol), proj4string = raster::crs(ProjLAEA))
  PointSol1 <- sp::spTransform(PointSol1, CRSobj = raster::crs(ProjLonLat))
  PointSol1 <- as.data.frame(PointSol1)

  ## Create a Google static map and plot it
  arguments <- list(...);
  plT <- (paste(arguments))
  if (length(plT)==0){plT <- TRUE}
  if (plT == TRUE){
    map <- RgoogleMaps::GetMap(center = c(raster::extent(rgeos::gCentroid(Polygon1))[4],
                                          raster::extent(rgeos::gCentroid(Polygon1))[1]), zoom = 13,
                               size= c(640,640), maptype="satellite",frame = T)
    RgoogleMaps::PlotOnStaticMap(MyMap = map, lat = PointSol1$lat,
                                 lon = PointSol1$lon, zoom = 13, size= c(640,640),
                                 cex = 1.3, pch = 19, col = "red", FUN = points, add = F)
    RgoogleMaps::PlotPolysOnStaticMap(MyMap = map,
                                      polys = sp::SpatialPolygons(Polygon1@polygons,proj4string=Polygon1@proj4string),
                                      border = NULL, lwd = 1, add=T)
  }
  invisible(PointSol1)
}

