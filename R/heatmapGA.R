#' @title Plot heatmap of fit grid cells
#' @name heatmapGA
#' @description  Plot a heatmap of the selected grid cells. Green grid
#' cells have been selected more often than red grid cells.
#'
#' @export
#'
#' @importFrom geoR dup.coords
#' @importFrom sp SpatialPoints coordinates gridded
#' @importFrom gstat idw
#' @importFrom rgl open3d rgl.surface rgl.snapshot
#' @importFrom ggplot2 ggplot geom_tile geom_point aes scale_fill_gradient
#' coord_equal
#'
#'
#' @param result The output matrix of \code{\link{windfarmGA}} or
#' \code{\link{genAlgo}}, which has stored all relevant information (matrix)
#' @param si A numeric value that is used for the sizing of the
#' resolution of the heatmap. Default is 2 (numeric)
#'
#' @return NULL
#' @examples \donttest{
#' ## Create a random rectangular shapefile
#' library(sp)
#' Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
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
#' heatmapGA(result)
#' }
#' @author Sebastian Gatscha
heatmapGA <- function(result,si=2){
  bpe <- do.call("rbind",result[,'allCoords']);
  bpe <- bpe[c(1,2)]

  sizing <- as.integer(result[,'inputData'][[1]][,1]['Resolution'])/si

  dupco <- geoR::dup.coords(bpe,simplify = TRUE);   bpe$Ids <- as.integer(rownames(bpe));

  dupco <- lapply(dupco, function(x) as.integer(x));  dupcosum <- lapply(dupco, function(x) length(x));
  bpenew <- vector("list",length(dupco))
  for (i in 1:length(dupco)){
    bpenew[[i]] <- bpe[bpe$Ids==dupco[[i]][1],];     bpenew[[i]]$Sum <- dupcosum[[i]][1]
  }
  bpenew <- do.call("rbind",bpenew);
  bpenew <- bpenew[-3]

  polo <- sp::SpatialPoints(sp::coordinates(cbind(bpenew$X,bpenew$Y)))

  exMar <- 50
  x.range <-   range(bpenew$X); y.range <-   range(bpenew$Y)
  grd <- expand.grid(x=seq(from=x.range[1]-exMar, to=x.range[2]+exMar, by=sizing), y=seq(from=y.range[1]-exMar, to=y.range[2]+exMar, by=sizing))
  ## convert grid to SpatialPixel class
  sp::coordinates(grd) <- ~ x+y;   sp::gridded(grd) <- TRUE


  idwout <- as.data.frame(gstat::idw(formula = bpenew$Sum~1,locations = polo,newdata=grd))


  plot1 <- ggplot2::ggplot(data=idwout,mapping=ggplot2::aes(x=x,y=y))+
    c(ggplot2::geom_tile(data=idwout,ggplot2::aes(fill=var1.pred)))+
    ggplot2::geom_point(data=bpenew,mapping=ggplot2::aes(x=X,y=Y),
                        show.legend = TRUE,size=sqrt(sqrt(bpenew$Sum)),alpha=0.6)
  plot1+ggplot2::scale_fill_gradient(low="red", high="green")+ggplot2::coord_equal()

}
