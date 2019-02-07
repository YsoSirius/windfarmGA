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
#' @importFrom raster extent res rasterToPolygons plot area intersect
#' @importFrom sp proj4string spTransform CRS coordinates
#' @importFrom graphics text plot par
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
#' @references \url{http://rfunctions.blogspot.co.at/2014/12/
#' gridfilter-intersect-grid-with-shape.html}
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
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' plot(Polygon1,axes=TRUE)
#'
#' ## Create a Grid
#' GridFilter(Polygon1,200,1,TRUE)
#' GridFilter(Polygon1,400,1,TRUE)
#'
#'
#' ## Examplary irregular input Polygon
#' Polygon1 <- Polygon(rbind(c(0, 20), c(0, 200),
#'                           c(2000, 2000), c(3000, 0)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' plot(Polygon1,axes=TRUE)
#'
#' ## Create a Grid
#' GridFilter(Polygon1,200,1,TRUE)
#' GridFilter(Polygon1,200,0.5,TRUE)
#' GridFilter(Polygon1,200,0.1,TRUE)
#' GridFilter(Polygon1,400,1,TRUE)
#' GridFilter(Polygon1,400,0.5,TRUE)
#' GridFilter(Polygon1,400,0.1,TRUE)
#'
#' @author Jose Hidasi (original) / Sebastian Gatscha (adapted)
GridFilter <- function(shape, resol = 500, prop = 1, plotGrid = FALSE) {
  if (prop < 0.01){prop <- 0.01}
  if (prop > 1){prop <- 1}
  grid <- raster::raster(raster::extent(shape))
  raster::res(grid) <- c(resol,resol)
  sp::proj4string(grid) <- sp::proj4string(shape)
  
  gridpolygon <- rasterToPolygons(grid)
  gridpolygon$layer <- c(1:length(gridpolygon$layer))
  # areagrid <- gArea(gridpolygon, byid = TRUE)
  areagrid <- raster::area(gridpolygon)
  
  ## raster::intersect statt rgeos.. dann kann rgeos weg. Sind ca gleich schnell (rgeos Spur schneller)
  # dry.grid <- rgeos::intersect(shape, gridpolygon)
  dry.grid <- raster::intersect(shape, gridpolygon)
  
  # areadrygrid <- gArea(dry.grid, byid=T)
  areadrygrid <- raster::area(dry.grid)
  
  info <- cbind(dry.grid$layer, areagrid[dry.grid$layer], areadrygrid)
  dry.grid$layer <- info[,3]/info[,2]
  
  if(!any(dry.grid$layer >= prop)) {
    print("\n################### GA ERROR MESSAGE ###################")
    stop("A grid cannot be drawn. Reduce the resolution or define a projection in meters.")
  }
  
  dry.grid.filtered <- dry.grid[dry.grid$layer >= prop,]
  
  areaquares <- round(sum(sapply(dry.grid.filtered@polygons, function(x)
    sapply(x@Polygons, function(y) y@area))) / 1000000, 3)
  
  if (plotGrid){
    parGrid = par(ask = FALSE, no.readonly = TRUE)
    par(mar = c(5,5,5,4))
    par(mfrow = c(1,1))
    plot.new()
    raster::plot(shape, col = "orange", main = paste("Resolution:", resol, "m and prop: ", prop,
                                                   "\n Total Area:", round(sum(areadrygrid) /1000000,3),
                                                   "km^2 \n Number Grids:", length(dry.grid.filtered),
                                                   "\n Sum Grid size:", areaquares, "km^2"))
    raster::plot(dry.grid.filtered, col = "lightgreen", add = TRUE)
  }
  
  ## 3 mal schneller als untere Methode
  x <- lapply(dry.grid.filtered@polygons, function(x) matrix(unlist(lapply(x@Polygons, function(y) y@coords[,1])), ncol = 1))
  y <- lapply(dry.grid.filtered@polygons, function(x) matrix(unlist(lapply(x@Polygons, function(y) y@coords[,2])), ncol = 1))
  
  # x <- lapply(dry.grid.filtered@polygons, function(x) sapply(x@Polygons, function(y) y@coords[,1]))
  # y <- lapply(dry.grid.filtered@polygons, function(x) sapply(x@Polygons, function(y) y@coords[,2]))
  
  
  ## 17 mal scheller, kein dplyr-select und kein gCentroid mehr, dafür geosphere::centroid
  # centpo = geosphere::centroid(dry.grid.filtered)
  # centpo = cbind(ID = 1:nrow(centpo), "X" = centpo[,1], "Y" = centpo[,2])
  
  # rect_Nu <- gCentroid(dry.grid.filtered, byid = T);
  # centpo <- data.frame(coordinates(rect_Nu))
  # centpo$ID <- 1:nrow(centpo);
  # names(centpo) <- c("X","Y","ID")
  # centpo <- dplyr::select(centpo, ID,X,Y)
  
  ## Nochmal um Faktor 1.5 schneller und braucht nur sp::coordinates
  centpo <- sp::coordinates(dry.grid.filtered)
  centpo <- cbind(ID = 1:nrow(centpo), "X" = centpo[,1], "Y" = centpo[,2])
  
  ## TODO
  ## Teste mit unregelmäßigen Polygonen
  
  if (plotGrid){
    graphics::points(centpo[,'X'], centpo[,'Y'], col = "blue", pch = 20)
    graphics::text(centpo[,'X'], centpo[,'Y'], labels = centpo[,'ID'], pos = 2)
    par(parGrid)
  }
  
  centpo <- list(centpo, dry.grid.filtered)
  invisible(centpo)
}
# 
# GridFilter <- function(shape, resol = 500, prop = 1, plotGrid=FALSE){
#   if (prop < 0.01){prop <- 0.01}
#   if (prop > 1){prop <- 1}
#   grid <- raster::raster(raster::extent(shape))
#   raster::res(grid) <- c(resol,resol)
#   sp::proj4string(grid)<- sp::proj4string(shape)
#   gridpolygon <- rasterToPolygons(grid)
#   
#   drylandproj <- spTransform(shape, CRS("+proj=laea"))
#   gridpolproj <- spTransform(gridpolygon, CRS("+proj=laea"))
#   gridpolproj$layer <- c(1:length(gridpolproj$layer))
#   areagrid <- gArea(gridpolproj, byid=T)
#   dry.grid <- intersect(drylandproj, gridpolproj)
#   areadrygrid <- gArea(dry.grid, byid=T)
#   info <- cbind(dry.grid$layer, areagrid[dry.grid$layer], areadrygrid)
#   dry.grid$layer<- info[,3]/info[,2]
#   dry.grid <- spTransform(dry.grid, CRS(proj4string(shape)))
#   
#   if(!any(dry.grid$layer >= prop)) {
#     print("\n################### GA ERROR MESSAGE ###################")
#     stop("A grid cannot be drawn. Reduce the resolution or define a projection in meters.")
#   }
#   
#   dry.grid.filtered <- dry.grid[dry.grid$layer >= prop,];
#   areaquares <- round(sum(sapply(dry.grid.filtered@polygons, function(x)
#     sapply(x@Polygons, function(y) y@area)))/1000000,3)
#   
#   if (plotGrid == TRUE){
#     parGrid = par(ask=F, no.readonly=T)
#     par(mar=c(5,5,5,4))
#     par(mfrow=c(1,1))
#     plot.new()
#     raster::plot(shape, col="orange",main = paste("Resolution:", resol, "m and prop: ",prop,
#                                                   "\n Total Area:", round(sum(areadrygrid)/1000000,3),
#                                                   "km^2 \n Number Grids:",length(dry.grid.filtered),
#                                                   "\n Sum Grid size:", areaquares, "km^2"))
#     raster::plot(dry.grid.filtered, col="lightgreen",add=TRUE)
#   }
#   
#   x <- lapply(dry.grid.filtered@polygons, function(x) sapply(x@Polygons, function(y) y@coords[,1]))
#   y <- lapply(dry.grid.filtered@polygons, function(x) sapply(x@Polygons, function(y) y@coords[,2]))
#   
#   rect_Nu <- gCentroid(dry.grid.filtered, byid = T);
#   centpo <- data.frame(coordinates(rect_Nu))
#   centpo$ID <- 1:nrow(centpo);
#   names(centpo) <- c("X","Y","ID")
#   centpo <- dplyr::select(centpo, ID,X,Y)
#   
#   if (plotGrid == TRUE){
#     raster::plot(rect_Nu,add=T)
#     graphics::points(centpo$X,centpo$Y, col="blue", pch=20)
#     graphics::text(centpo$X,centpo$Y,labels=centpo$ID, pos=2)
#     par(parGrid)
#   }
#   
#   centpo <- list(centpo,dry.grid.filtered)
#   invisible(centpo)
# }


