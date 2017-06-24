#' @title Make a grid from a Polygon
#' @name GridFilter
#' @description Create a grid from a given polygon and with a certain
#' resolution and proportionality. The center points of each grid cell
#' represent possible locations for wind turbines.
#'
#' @note The grid of the genetic algorithm will have a resolution of
#' \code{Rotor * fcrR}. See the arguments of \code{\link{windfarmGA}}
#'
#' @export
#'
#' @importFrom raster extent res rasterToPolygons plot
#' @importFrom sp proj4string spTransform CRS coordinates
#' @importFrom rgeos gArea intersect gCentroid
#' @importFrom dplyr select
#' @importFrom graphics text plot
#'
#' @param shape Shape file of the considered area (SpatialPolygons)
#' @param resol The resolution of the grid in meter. Default is 500.
#' (numeric)
#' @param prop A factor used for grid calculation.
#' Determines the percentage a grid has to overlay the considered area
#' to be represented as grid cell. Default is 1. (numeric)
#' @param plotGrid Logical value indicating whether resulting grid
#' should be plotted or not. Default is FALSE. (logical)
#'
#' @return Returns a list with 2 elements. List element 1 will have
#' the grid cell IDS, and the X and Y coordinates of the centers
#' of each grid cell. List element 2 is the grid as SpatialPolygons,
#' which is used for plotting purposes. (list)
#'
#' @author Jose Hidasi (original) / Sebastian Gatscha (adapted)
#' @references \url{http://rfunctions.blogspot.co.at/2014/12/
#' gridfilter-intersect-grid-with-shape.html}
#'
#' @examples
#' library(sp)
#' ## Exemplary input Polygon with 2km x 2km:
#' Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000),
#' c(2000, 2000), c(2000, 0)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' plot(Polygon1,axes=TRUE)
#'
#' GridFilter(Polygon1,200,1,TRUE)
#'
GridFilter <- function(shape, resol = 500, prop = 1, plotGrid=FALSE){
  # library(rgeos);  library(sp);  library(raster);
  # require(raster)
  # shape = Polygon1;resol = 90; prop=1; plotGrid = TRUE

  if (prop < 0.01){prop = 0.01}
  if (prop > 1){prop = 1}
  grid <- raster::raster(raster::extent(shape))
  raster::res(grid) <- c(resol,resol)
  sp::proj4string(grid)<-sp::proj4string(shape)
  gridpolygon <- raster::rasterToPolygons(grid)

  drylandproj<- sp::spTransform(shape, sp::CRS("+proj=laea"))
  gridpolproj<-sp::spTransform(gridpolygon, sp::CRS("+proj=laea"))
  gridpolproj$layer <- c(1:length(gridpolproj$layer))
  areagrid <- rgeos::gArea(gridpolproj, byid=T)
  dry.grid <- rgeos::intersect(drylandproj, gridpolproj)
  areadrygrid <- rgeos::gArea(dry.grid, byid=T)
  info <- cbind(dry.grid$layer, areagrid[dry.grid$layer], areadrygrid)
  dry.grid$layer<-info[,3]/info[,2]
  dry.grid <- sp::spTransform(dry.grid, sp::CRS(sp::proj4string(shape)))

  if(!any(dry.grid$layer >= prop)) {
    print("\n################### GA ERROR MESSAGE ###################")
    stop("A grid cannot be drawn. Reduce the resolution or define a projection in meters.")
  }

  dry.grid.filtered <- dry.grid[dry.grid$layer >= prop,];
  areaquares <- round(sum(sapply(dry.grid.filtered@polygons, function(x)
              sapply(x@Polygons, function(y) y@area)))/1000000,3)

  par(mar=c(5,5,5,4))
  if (plotGrid == TRUE){
    raster::plot(shape, col="orange",main = paste("Resolution:", resol, "m and prop: ",prop,
                                          "\n Total Area:", round(sum(areadrygrid)/1000000,3),
                                          "km^2 \n Number Grids:",length(dry.grid.filtered),
                                          "\n Sum Grid size:", areaquares, "km^2"))
    raster::plot(dry.grid.filtered, col="lightgreen",add=TRUE)
  }


  x <- lapply(dry.grid.filtered@polygons, function(x) sapply(x@Polygons, function(y) y@coords[,1]))
  y <- lapply(dry.grid.filtered@polygons, function(x) sapply(x@Polygons, function(y) y@coords[,2]))


  rect_Nu <- rgeos::gCentroid(dry.grid.filtered,byid = T);  raster::plot(rect_Nu,add=T)
  centpo <- sp::coordinates(rect_Nu);     centpo <- as.data.frame(centpo)
  centpo$ID <- 1:nrow(centpo);     names(centpo) <- c("X","Y","ID")
  centpo <- dplyr::select(centpo, ID,X,Y)

  graphics::points(centpo$X,centpo$Y, col="blue", pch=20)
  graphics::text(centpo$X,centpo$Y,labels=centpo$ID, pos=2)

  centpo <- list(centpo,dry.grid.filtered)
  invisible(centpo)
}


