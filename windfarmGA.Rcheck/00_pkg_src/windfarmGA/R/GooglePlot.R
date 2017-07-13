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
#' @param ... Some arguments can be passed to
#' \link[RgoogleMaps]{GetMap} and \link[RgoogleMaps]{PlotOnStaticMap},
#' of the 'RgoogleMaps' package including \code{maptype}, \code{col},
#' \code{cex}, \code{pch}, \code{size} and \code{zoom}. If the plot is not
#' required, it can be disabled with the option \code{plT = FALSE}.
#' Check the examples for some action.
#'
#' @return Returns a data.frame with the coordinates in LON/LAT and plots
#' the desired best result with a Google background map. (data.frame)
#'
#' @examples \donttest{
#' ## Add some data examples from the package
#' load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
#'
#' ## Plot the results of a wind farm optimization
#' result <- resultrect
#' Polygon1 <- polygon
#'
#' GooglePlot(result, Polygon1, 1, 1)
#' GooglePlot(result, Polygon1, 2, 1, zoom=14, maptype="hybrid",
#'            col="darkblue", pch=18)
#' GooglePlot(result, Polygon1, 3, 1, zoom=14, maptype="terrain",
#'            col="black", pch=20)
#' GooglePlot(result, Polygon1, 3, 2, zoom=15, maptype="satellite",
#'            col="red", pch=10)
#' GooglePlot(result, Polygon1, 1, 1, zoom=14, maptype="mobile",
#'            col="darkblue", pch=17)
#' GooglePlot(result, Polygon1, 1, 1, zoom=14, maptype="hybrid",
#'            col="darkblue", pch=20)
#' GooglePlot(result, Polygon1, 1, 1, zoom=14, maptype="hybrid",
#'            col="blue", pch=18, cex= 2)
#' GooglePlot(result, Polygon1, 1, 1, zoom=14, maptype="hybrid",
#'            col="blue", pch=18, cex= 2, size=c(320,320))
#'
#'}
#' @author Sebastian Gatscha
GooglePlot <- function(result,Polygon1,best=1,plotEn=1, Projection,...){

  ## Set the graphical parameters
  op <- par(ask=F)
  on.exit(par(op))
  par(mfrow=c(1,1))

  ## Set the splitting parameters
  if (plotEn == 1) {
    en <- "EnergyOverall"
  }
  if (plotEn == 2) {
    en <- "Parkfitness"
  }

  ## Split resulting individuals
  result <- result[,2][order(as.character(sapply(result[,2], "[", en)),decreasing = T)]
  result <- result[best]
  Solution <- do.call("rbind", result)

  ## Input reference systems
  if (missing(Projection)) {
    if (is.na(proj4string(Polygon1))){
      ProjLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
      +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
      raster::crs(Polygon1) <- raster::crs(ProjLAEA)
    } else {
      ProjLAEA <- sp::proj4string(Polygon1)
    }
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

  zoom= 15
  maptype = "hybrid"
  col = "red"
  cex = 1.3
  pch = 19
  size= c(640,640)
  plT = TRUE

  if (!is.null(arguments$maptype)) {
    maptype <- arguments$maptype
  }
  if (!is.null(arguments$zoom)) {
    zoom <- arguments$zoom
  }
  if (!is.null(arguments$col)) {
    col <- arguments$col
  }
  if (!is.null(arguments$zoom)) {
    zoom <- arguments$zoom
  }
  if (!is.null(arguments$cex)) {
    cex <- arguments$cex
  }
  if (!is.null(arguments$pch)) {
    pch <- arguments$pch
  }
  if (!is.null(arguments$size)) {
    size <- arguments$size
  }
  if (!is.null(arguments$plT)) {
    plT <- arguments$plT
  }

  if (plT == TRUE){
    map <- RgoogleMaps::GetMap(center = c(raster::extent(rgeos::gCentroid(Polygon1))[4],
                                          raster::extent(rgeos::gCentroid(Polygon1))[1]), zoom = zoom,
                               size= size, maptype=maptype,frame = T)
    RgoogleMaps::PlotOnStaticMap(MyMap = map, lat = PointSol1$lat,
                                 lon = PointSol1$lon, zoom = zoom, size= size,
                                 cex = cex, pch = pch, col = col, FUN = points, add = F)
    RgoogleMaps::PlotPolysOnStaticMap(MyMap = map,
                                      polys = sp::SpatialPolygons(Polygon1@polygons,proj4string=Polygon1@proj4string),
                                      border = NULL, lwd = 1, add=T)
  }

  invisible(PointSol1)
}

