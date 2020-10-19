#' @title Make a grid from a Polygon
#' @name grid_area
#' @description Create a grid from a given polygon and with a certain resolution
#'   and proportionality. The center points of each grid cell represent possible
#'   locations for wind turbines.
#'
#' @note The grid of the genetic algorithm will have a resolution of \code{Rotor
#'   * fcrR}. See the arguments of \code{\link{windfarmGA}}
#'
#' @export
#'
#' @param shape Shape file of the considered area
#' @param resol The resolution of the grid in meters. Default is 500
#' @param prop A factor used for grid calculation. Determines the percentage a
#'   grid has to overlay the considered area to be represented as grid cell.
#'   Default is 1.
#' @param plotGrid Logical value indicating whether resulting grid should be
#'   plotted or not. Default is FALSE.
#'
#' @family Helper Functions
#' @return Returns a list with 2 elements. List element 1 will have the grid
#'   cell IDS, and the X and Y coordinates of the centers of each grid cell.
#'   List element 2 is the grid as SpatialPolygons, which is used for plotting
#'   purposes.
#'
#' @references
#'   \url{http://rfunctions.blogspot.co.at/2014/12/gridfilter-intersect-grid-with-shape.html}
#'
#'   
#' @examples
#' library(sp)
#' library(raster)
#' library(rgeos)
#' 
#' ## Exemplary input Polygon with 2km x 2km:
#' Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000),
#' c(2000, 2000), c(2000, 0)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+init=epsg:3035"
#' proj4string(Polygon1) <- Projection
#'
#' ## Create a Grid
#' grid_area(Polygon1,200,1,TRUE)
#' grid_area(Polygon1,400,1,TRUE)
#'
#'
#' ## Examplary irregular input Polygon
#' Polygon1 <- Polygon(rbind(c(0, 20), c(0, 200),
#'                           c(2000, 2000), c(3000, 0)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+init=epsg:3035"
#' proj4string(Polygon1) <- Projection
#'
#' ## Create a Grid
#' grid_area(Polygon1,200,1,TRUE)
#' grid_area(Polygon1,200,0.5,TRUE)
#' grid_area(Polygon1,200,0.1,TRUE)
#' grid_area(Polygon1,400,1,TRUE)
#' grid_area(Polygon1,400,0.5,TRUE)
#' grid_area(Polygon1,400,0.1,TRUE)
#'
#' @author Jose Hidasi (original) / Sebastian Gatscha (adapted)
grid_area <- function(shape, resol = 500, prop = 1, plotGrid = FALSE) {
  if (prop < 0.01) {
    prop <- 0.01
  }
  if (prop > 1) {
    prop <- 1
  }

  ## Polygon to Raster, assign Resolution and Project
  grid <- raster::raster(raster::extent(shape))
  raster::res(grid) <- c(resol, resol)
  if (utils::compareVersion(sf::sf_extSoftVersion()[[3]], "6") > 0) {
    slot(grid, "crs") <- slot(shape, "proj4string")
  } else {
    sp::proj4string(grid) <- sp::proj4string(shape)
  }

  ## Raster to Grid (SpatialPolygonsDataFrame) and get areas
  gridpolygon <- rasterToPolygons(grid)
  gridpolygon$layer <- c(1:length(gridpolygon$layer))
  areagrid <- raster::area(gridpolygon)

  ## Intersect Polygon with Grid and get new areas
  requireNamespace("rgeos")
  grid_intersect <- raster::intersect(shape, gridpolygon)
  areadrygrid <- raster::area(grid_intersect)

  ## Bind Information together and calulcate coverages
  info <- cbind(grid_intersect$layer,
                areagrid[grid_intersect$layer],
                areadrygrid)

  grid_intersect$layer <- info[, 3] / info[, 2]
  if (!any(grid_intersect$layer >= prop)) {
    cat("\n################### GA ERROR MESSAGE ###################\n")
    stop("A grid cannot be drawn. Reduce the resolution ",
         "or define a projection in meters.")
  }

  ## Subtract Grid cells with too small coverage
  grid_filtered <- grid_intersect[grid_intersect$layer >= prop - 0.0001, ]

  if (plotGrid) {
    ## Calculate total area
    areaquares <- round(sum(sapply(grid_filtered@polygons, function(x)
      sapply(x@Polygons, function(y) y@area))) / 1000000, 3)

    par_grid <- par(ask = FALSE, no.readonly = TRUE)
    on.exit(par_grid)
    plot.new()
    par(mar = c(5, 5, 5, 4))
    par(mfrow = c(1, 1))
    raster::plot(shape, col = "orange",
           main = paste("Resolution:", resol, "m and prop: ", prop,
                        "\n Total Area:", round(sum(areadrygrid) / 1000000, 3),
                        "km^2 \n Number Grids:", length(grid_filtered),
                        "\n Sum Grid size:", areaquares, "km^2"))
    raster::plot(grid_filtered, col = "lightgreen", add = TRUE)
  }

  ## Get Grid Centers and add ID Field
  centpo <- sp::coordinates(grid_filtered)
  centpo <- cbind(ID = 1:nrow(centpo), "X" = centpo[, 1], "Y" = centpo[, 2])

  if (plotGrid) {
    graphics::points(centpo[, "X"], centpo[, "Y"], col = "blue", pch = 20)
    graphics::text(centpo[, "X"], centpo[, "Y"],
                   labels = centpo[, "ID"], pos = 2)
  }

  ## Return Grid Cell Matrix and Grid as SpatialObject
  centpo <- list(centpo, grid_filtered)
  invisible(centpo)
}