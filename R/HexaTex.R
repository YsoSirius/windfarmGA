#' @title Polygon to Hexagonal Grid Tessellation
#'
#' @name HexaTex
#'
#' @description  The function takes a Polygon and a sizing argument and
#' creates a list with an indexed data frame with coordinates
#' and a SpatialPolygons object, that consists of hexagonal grid cells.
#'
#' @export
#'
#' @importFrom spatstat hextess hexgrid
#' @importFrom maptools as.owin.SpatialPolygons
#' @importFrom methods as
#' @importFrom graphics points
#' @importFrom raster plot area
#'
#' @param Polygon1 The SpatialPolygons object (SpatialPolygons)
#' @param size The side length of an hexagon (numeric)
#' @param plotTrue Should the object be plotted (logical)
#'
#' @return Returns a list with an indexed data frame of the point coordinates
#' and a SpatialPolygons object of the hexagons (list)
#'
#' @examples
#' library(spatstat)
#' library(maptools)
#' library(sp)
#' library(raster)
#' Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
#'                           c(4499991, 2669343), c(4499991, 2668272)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' plot(Polygon1,axes=TRUE)
#' HexGrid <- HexaTex(Polygon1, 100, TRUE)
#' plot(HexGrid[[2]])
#' str(HexGrid[[1]])
#'
#' @author Sebastian Gatscha
HexaTex <- function(Polygon1, size, plotTrue = FALSE){
  # Polygon1=Polygon1;size=200; plotTrue=T
  opar = par(no.readonly = T)
  par(mfrow=c(1,1))

  ## Make a Tesselation Object from the Polygon with the size parameter.
  HexaGrid <- spatstat::hextess(maptools::as.owin.SpatialPolygons(Polygon1), s = size)
  ## Convert it to a SpatialPolygons object.
  HexaGrid <- tess2SPdf(HexaGrid)
  ## Make Points inside every hexagonal grid cell
  points <- spatstat::hexgrid(maptools::as.owin.SpatialPolygons(Polygon1),s = size)
  ## Convert the planar point pattern to a data frame
  PointsHexa <- as.data.frame(cbind(X=points$x, Y=points$y));
  ## Add an ID Column and reorder and rename the data frame
  IDs <- 1: nrow(PointsHexa)
  PointsHexa <- as.data.frame(cbind(IDs,PointsHexa$X, PointsHexa$Y))
  colnames(PointsHexa) <- c("ID","X","Y")
  ## Plot the Tesselation with the points if desired
  if (plotTrue == TRUE){
    plot.new()
    raster::plot(HexaGrid, col="lightgreen",main=paste("Resolution: ", size, "m",
                                               "\n Total Area: ", round(sum(raster::area(Polygon1)/1000000),2), "km^2",
                                               "\nAmount of Hexagons: ", length(HexaGrid),
                                               "\nAmount of Points: ", nrow(PointsHexa)))
    graphics::points(cbind(PointsHexa$X,PointsHexa$Y), col="blue", pch=20)
  }
  par(opar)
  invisible(list(PointsHexa,HexaGrid))
}


