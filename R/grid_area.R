#' @title Make a grid from a Simple Feature Polygon
#' @name grid_area
#' @description Create a grid from a given polygon with a certain resolution
#'   and proportionality. The grid cell centroids represent possible
#'   wind turbine locations.
#'
#' @note The grid of the genetic algorithm will have a resolution of \code{Rotor
#'   * fcrR}. See the arguments of \code{\link{windfarmGA}}
#'
#' @export
#'
#' @param shape Simple Feature Polygon of the considered area
#' @param size The cellsize of the grid in meters. Default is 500
#' @param prop A factor used for grid calculation. It determines the minimum 
#'   percentage that a grid cell must cover the area. Default is 1
#' @param plotGrid Logical value indicating whether the results should be
#'   plotted. Default is \code{FALSE}
#'
#' @family Helper Functions
#' @return Returns a list with 2 elements. List element 1 will have the grid
#'   cell IDS, and the X and Y coordinates of the centers of each grid cell.
#'   List element 2 is the grid as Simple Feature Polygons, which is used for 
#'   plotting purposes.
#'   
#' @examples \donttest{
#' ## Exemplary input Polygon with 2km x 2km:
#' library(sf)
#' Polygon1 <- sf::st_as_sf(sf::st_sfc(
#'  sf::st_polygon(list(cbind(
#'    c(0, 0, 2000, 2000, 0),
#'    c(0, 2000, 2000, 0, 0)))),
#' crs = 3035
#' ))
#'
#' ## Create a Grid
#' grid_area(Polygon1, 200, 1, TRUE)
#' grid_area(Polygon1, 400, 1, TRUE)
#'
#' ## Examplary irregular input Polygon
#' Polygon1 <- sf::st_as_sf(sf::st_sfc(
#'  sf::st_polygon(list(cbind(
#'    c(0, 0, 2000, 3000, 0),
#'    c(20, 200, 2000, 0, 20)))),
#'  crs = 3035
#' ))
#'
#' ## Create a Grid
#' grid_area(Polygon1, 200, 1,   TRUE)
#' grid_area(Polygon1, 200, 0.1, TRUE)
#' grid_area(Polygon1, 400, 1,   TRUE)
#' grid_area(Polygon1, 400, 0.1, TRUE)
#' }
grid_area <- function(shape, size = 500, prop = 1, plotGrid = FALSE) {
  if (prop < 0.01) {
    prop <- 0.01
  }
  if (prop > 1) {
    prop <- 1
  }

  grid_polys <- sf::st_make_grid(shape, cellsize = size, what = "polygons")
  grid_intersect <- sf::st_intersects(grid_polys, shape, sparse = FALSE)
  grid_polys <- grid_polys[grid_intersect]
  grid_intersect <- sf::st_intersection(st_geometry(shape), grid_polys)
  areadrygrid <- sf::st_area(grid_intersect)
  indx <- as.numeric((areadrygrid / size^2)) >= prop
  
  if (!any(indx)) {
    cat("\n################### GA ERROR MESSAGE ###################\n")
    stop("A grid cannot be drawn. Reduce the `size` argument ",
         "or define a projection in meters.")
  }
  
  grid_filtered <- grid_intersect[indx]
  grid_centr <- sf::st_centroid(grid_filtered)
  centpo <- st_coordinates(grid_centr)
  centpo <- cbind(ID = 1:nrow(centpo), "X" = centpo[, 1], "Y" = centpo[, 2])
  
  if (plotGrid) {
    par_grid <- par(ask = FALSE, no.readonly = TRUE)
    on.exit(par_grid)
    plot.new()
    par(mar = c(5, 5, 5, 4), mfrow = c(1, 1))
    
    plot(st_geometry(shape), col="orange", 
         main = paste("Cellsize:", size, "m and prop: ", prop,
                      "\nTotal area:", round(sum(as.numeric(sf::st_area(shape)))*0.000001, 3), "km^2",
                      "\nNumber of Grids:", length(grid_filtered),
                      "\nGrid area:", round(sum(as.numeric(sf::st_area(grid_filtered)))*0.000001, 3), "km^2"))
    plot(grid_filtered, col = "lightgreen", add=TRUE)
    graphics::points(centpo[, "X"], centpo[, "Y"], col = "blue", pch = 20)
    graphics::text(centpo[, "X"], centpo[, "Y"],
                   labels = centpo[, "ID"], pos = 2, offset=0.2, cex=0.7)
  }

  ## Return Grid Cell Matrix and Grid as Simple Feature Polygons
  invisible(list(centpo, grid_filtered))
}



#' @title Polygon to Hexagonal Grids
#' @name hexa_area
#' @description The function takes a Simple Feature Polygon and a size argument 
#'   and creates a list with an indexed matrix with coordinates and a Simple Feature
#'   object, that consists of hexagonal grids.
#'
#' @export
#' @inheritParams grid_area
#' @family Helper Functions
#' @inherit grid_area return
#'   
#' @examples
#' library(sf)
#' ## Exemplary input Polygon with 2km x 2km:
#' Poly <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)))), 
#'   crs = 3035
#' ))
#' HexGrid <- hexa_area(Poly, 100, TRUE)
#'
hexa_area <- function(shape, size = 500, plotGrid = FALSE) {
  grid_polys <- sf::st_make_grid(shape, cellsize = size,
                                 what = "polygons", square = FALSE)
  grid_intersect <- sf::st_intersects(grid_polys, shape, sparse = FALSE)
  grid_polys <- grid_polys[grid_intersect]
  grid_intersect <- sf::st_intersection(st_geometry(shape), grid_polys)
  
  areadrygrid <- sf::st_area(grid_intersect)
  indx <- as.numeric(areadrygrid) >= (2* sqrt(3) * (size/2)^2)*0.99
    
  if ((!any(indx)) || (length(grid_polys)==1)) {
    cat("\n################### GA ERROR MESSAGE ###################\n")
    stop("A grid cannot be drawn. Reduce the `size` argument ",
         "or define a projection in meters.")
  }
  
  grid_filtered <- grid_polys[indx]
  grid_centr <- sf::st_centroid(grid_filtered)
  
  centpo <- st_coordinates(grid_centr)
  centpo <- cbind(ID = 1:nrow(centpo), "X" = centpo[, 1], "Y" = centpo[, 2])
  
  if (plotGrid) {
    par_grid <- par(ask = FALSE, no.readonly = TRUE)
    on.exit(par_grid)
    plot.new()
    par(mar = c(5, 5, 5, 4), mfrow = c(1, 1))
    
    plot(st_geometry(shape), col="orange", 
         main = paste("Cellsize:", size, "m",
                      "\nTotal area:", round(sum(as.numeric(st_area(st_geometry(shape))))*0.000001, 3), "km^2",
                      "\nNumber of Grids:", length(grid_filtered),
                      "\nGrid area:", round(sum(as.numeric(st_area(grid_filtered)))*0.000001, 3), "km^2"))
    plot(grid_filtered, col = "lightgreen", add=TRUE)
    graphics::points(centpo[, "X"], centpo[, "Y"], col = "blue", pch = 20)
    graphics::text(centpo[, "X"], centpo[, "Y"],
                   labels = centpo[, "ID"], pos = 2, offset=0.2, cex=0.7)
  }
  
  ## Return Grid Cell Matrix and Grid as Simple Feature Polygons
  invisible(list(centpo, grid_filtered))
}
