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
#' @importFrom ggplot2 ggplot geom_tile geom_point aes scale_fill_gradient coord_equal
#'
#'
#' @param result The output matrix of \code{\link{genAlgo}}, which
#' has stored all relevant informations (matrix)
#' @param si A numeric value that is used for the sizing of the
#' resolution of the heatmap. Default is 2 (numeric)
#' @param Polygon1 The considered area as shapefile (SpatialPolygons)
#'
#' @return NULL
#'
#' @author Sebastian Gatscha
heatmapGA <- function(result,si=2, Polygon1){
  # result=resultConfGa1; Polygon=Polygon1
  # library(geoR);   library(gstat);   library(ggplot2)
  bpe <- do.call("rbind",result[,'allCoords']);
  bpe <- bpe[c(1,2)]
  #plot(bpe,col="red",pch=20); summary(bpe)

  sizing = as.integer(result[,'inputData'][[1]][,1]['Resolution'])/si

  dupco <- geoR::dup.coords(bpe,simplify = TRUE);   bpe$Ids <- as.integer(rownames(bpe));

  dupco <- lapply(dupco, function(x) as.integer(x));  dupcosum <- lapply(dupco, function(x) length(x));
  bpenew <- vector("list",length(dupco))
  for (i in 1:length(dupco)){
    bpenew[[i]] <- bpe[bpe$Ids==dupco[[i]][1],];     bpenew[[i]]$Sum <- dupcosum[[i]][1]
  }
  bpenew <- do.call("rbind",bpenew);
  bpenew <- bpenew[-3]

  #points(bpenew$X,bpenew$Y,pch=20, cex=bpenew$Sum/100)

  polo <- sp::SpatialPoints(sp::coordinates(cbind(bpenew$X,bpenew$Y)))

  exMar <- 50
  x.range <-   range(bpenew$X); y.range <-   range(bpenew$Y)
  grd <- expand.grid(x=seq(from=x.range[1]-exMar, to=x.range[2]+exMar, by=sizing), y=seq(from=y.range[1]-exMar, to=y.range[2]+exMar, by=sizing))
  ## convert grid to SpatialPixel class
  sp::coordinates(grd) <- ~ x+y;   sp::gridded(grd) <- TRUE


  idwout <- as.data.frame(gstat::idw(formula = bpenew$Sum~1,locations = polo,newdata=grd))


  plot1<-ggplot2::ggplot(data=idwout,mapping=ggplot2::aes(x=x,y=y))+
    c(ggplot2::geom_tile(data=idwout,ggplot2::aes(fill=var1.pred)))+
    ggplot2::geom_point(data=bpenew,mapping=ggplot2::aes(x=X,y=Y),
                        show.legend = TRUE,size=sqrt(sqrt(bpenew$Sum)),alpha=0.6)
  #geom_contour(bins=2,ggplot2::aes(colour = ..level..))
  plot1+ggplot2::scale_fill_gradient(low="red", high="green")+ggplot2::coord_equal()


}




