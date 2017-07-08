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
#' @examples {
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
#' }
#' @author Sebastian Gatscha
tess2SPdf <- function(x) {
  stopifnot(spatstat::is.tess(x))
  sp.obj <- methods::as(x, "SpatialPolygons")
  return(sp.obj)
}

