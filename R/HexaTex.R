#' @title Create a Tesselation from a Polygon
#'
#' @name tess2SPdf
#'
#' @description  Returns a Spatial Polygons object from
#' a Tesselation object.
#'
#' @export
#'
#' @importFrom spatstat is.tess
#' @importFrom methods as
#'
#' @param x The Tesselation object (Tessellation)
#'
#' @return Returns a SpatialPolygons. (SpatialPolygons)
#'
#' @examples
#' library(spatstat)
#' library(maptools)
#' library(sp)
#' Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
#'                           c(4499991, 2669343), c(4499991, 2668272)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' plot(Polygon1,axes=TRUE)
#' ## Create a hexagonal Tesselation
#' HexaGrid <- spatstat::hextess(maptools::as.owin.SpatialPolygons(Polygon1),s = 100)
#' plot(HexaGrid)
#' HexaGrid
#' ## Convert the Tesselation to SpatialPolygons
#' Hex2spdf <- tess2SPdf(HexaGrid)
#' plot(Hex2spdf)
#' Hex2spdf
#'
#' @author Sebastian Gatscha
tess2SPdf <- function(x) {
  stopifnot(spatstat::is.tess(x))
  sp.obj <- methods::as(x, "SpatialPolygons")
  return(sp.obj)
}



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
#' @importFrom sp proj4string
#' @importFrom graphics points
#' @importFrom raster plot area
#'
#' @param Polygon1 The SpatialPolygons object 
#' @param size The side length of an hexagon 
#' @param plotTrue Should the object be plotted
#'
#' @return Returns a list with an indexed data frame of the point coordinates
#' and a SpatialPolygons object of the hexagons 
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

  if (plotTrue) {
    parHex = par(no.readonly = TRUE)
    on.exit(par(parHex))
    par(mfrow = c(1,1))
  }

  ## Make a Tesselation Object from the Polygon with the size parameter.
  owinSP = maptools::as.owin.SpatialPolygons(Polygon1)
  HexaGrid <- spatstat::hextess(owinSP, s = size)
  ## Convert it to a SpatialPolygons object.
  HexaGrid <- tess2SPdf(HexaGrid)
  ## Make Points inside every hexagonal grid cell
  points <- spatstat::hexgrid(owinSP, s = size)
  ## Convert the planar point pattern to a data frame
  PointsHexa <- cbind(X = points$x, Y = points$y)
  ## Add an ID Column and reorder and rename the data frame
  PointsHexa <- cbind("ID" =  1:nrow(PointsHexa), PointsHexa)
  
  ## Plot the Tesselation with the points if desired
  if (plotTrue){
    plot.new()
    raster::plot(HexaGrid, col="lightgreen", 
                 main = paste("Resolution: ", size, "m",
                              "\n Total Area: ", round(sum(raster::area(Polygon1)/1000000),2), "km^2",
                              "\nAmount of Hexagons: ", length(HexaGrid),
                              "\nAmount of Points: ", nrow(PointsHexa)))
    graphics::points(cbind(PointsHexa[,'X'], PointsHexa[,'Y']), col = "blue", pch = 20)
  }
  
  ## Assign same Projection as Polygon
  sp::proj4string(HexaGrid) <- sp::proj4string(Polygon1)
  
  invisible(list(PointsHexa,HexaGrid))
}



