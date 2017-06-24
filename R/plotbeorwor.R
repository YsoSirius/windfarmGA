#' @title Plot if previous population was better or worse
#' @name plotbeorwor
#' @description  Plot the changes in mean and max fitness values to previous
#' generation.
#'
#' @export
#'
#' @importFrom graphics plot par abline title
#'
#' @param result The output matrix of \code{\link{windfarmGA}} or
#' \code{\link{genAlgo}}, which has stored all relevant information. (matrix)
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
#' plotbeorwor(result)
#' }
#' @author Sebastian Gatscha
plotbeorwor <- function(result){
  #result = result5
  opar <- par(no.readonly = T)
  beorworse = do.call("rbind",result[,9]);

  par(mfrow=c(2,1))
  maxdif <- as.data.frame(beorworse[,1]);
  maxdif$farbe <- 0
  maxdif$farbe[maxdif$`beorworse[, 1]` < 0]  <- "red"
  maxdif$farbe[maxdif$`beorworse[, 1]` > 0] <- "green"
  maxdif$farbe[maxdif$`beorworse[, 1]` == 0] <- "orange"
  plot(maxdif$`beorworse[, 1]`, type="b", col=maxdif$farbe, pch=20,cex=2); abline(0,0)
  title("Max Difference to previous generation")


  meandif <- as.data.frame(beorworse[,2]);
  meandif$farbe <- 0
  meandif$farbe[meandif$`beorworse[, 2]` < 0]  <- "red"
  meandif$farbe[meandif$`beorworse[, 2]` > 0] <- "green"
  meandif$farbe[meandif$`beorworse[, 2]` == 0] <- "orange"
  plot(meandif$`beorworse[, 2]`, type="b", col=meandif$farbe, pch=20,cex=2); abline(0,0)
  title("Mean Difference to previous generation")

  par(opar)
  return()
}

