#' @title Create a Tesselation from a Polygon
#'
#' @name tess2SPdf
#'
#' @description  Returns a Spatial Polygons object from a Tesselation object.
#'
#' @export
#'
#' @param x The Tesselation object (Tessellation)
#'
#' @family Helper Functions
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
#' 
#' ## Create a hexagonal Tesselation
#' HexaGrid <- spatstat::hextess(maptools::as.owin.SpatialPolygons(Polygon1),s = 100)
#' plot(HexaGrid)
#' 
#' ## Convert the Tesselation to SpatialPolygons
#' Hex2spdf <- tess2SPdf(HexaGrid)
#' plot(Hex2spdf)
#'
tess2SPdf <- function(x) {
  stopifnot(spatstat::is.tess(x))
  sp.obj <- methods::as(x, "SpatialPolygons")
  return(sp.obj)
}



#' @title Polygon to Hexagonal Grid Tessellation
#'
#' @name hexa_area
#'
#' @description  The function takes a Polygon and a sizing argument and creates
#'   a list with an indexed matrix with coordinates and a SpatialPolygons
#'   object, that consists of hexagonal grids
#'
#' @export
#'
#' @param Polygon1 The SpatialPolygons object
#' @param size The side length of an hexagon
#' @param plotTrue Should the object be plotted
#'
#' @family Helper Functions
#' @return Returns a list with an indexed matrix of the point coordinates and a
#'   SpatialPolygons object of the hexagons
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
#' HexGrid <- hexa_area(Polygon1, 100, TRUE)
#' plot(HexGrid[[2]])
#'
hexa_area <- function(Polygon1, size, plotTrue = FALSE){

  if (plotTrue) {
    par_hex <- par(no.readonly = TRUE)
    on.exit(par(par_hex))
    par(mfrow = c(1, 1))
  }

  ## Make a Tesselation Object from the Polygon with the size parameter.
  owin_sp <- maptools::as.owin.SpatialPolygons(Polygon1)
  hexa_grid <- spatstat::hextess(owin_sp, s = size)
  ## Convert it to a SpatialPolygons object.
  hexa_grid <- tess2SPdf(hexa_grid)
  ## Make Points inside every hexagonal grid cell
  points <- spatstat::hexgrid(owin_sp, s = size)
  ## Convert the planar point pattern to a data frame
  points_hexa <- cbind(X = points$x, Y = points$y)
  ## Add an ID Column and reorder and rename the data frame
  points_hexa <- cbind("ID" = 1:nrow(points_hexa), points_hexa)

  ## Plot the Tesselation with the points if desired
  if (plotTrue){
    plot.new()
    raster::plot(hexa_grid, col = "lightgreen",
                 main = paste("Resolution: ", size, "m",
                              "\n Total Area: ", round(sum(
                                raster::area(Polygon1) / 1000000), 2), "km^2",
                              "\nAmount of Hexagons: ", length(hexa_grid),
                              "\nAmount of Points: ", nrow(points_hexa)))
    graphics::points(cbind(points_hexa[, "X"], points_hexa[, "Y"]),
                     col = "blue", pch = 20)
  }

  ## Assign same Projection as Polygon
  sp::proj4string(hexa_grid) <- sp::proj4string(Polygon1)
  invisible(list(points_hexa, hexa_grid))
}