#' @title Plot the evolution of fitness values
#' @name plotEvolution
#' @description  Plot the evolution of energy outputs and efficiency rates
#' over the whole generations. Plots min, mean and max values.
#' @export
#'
#' @importFrom graphics plot lines grid points par
#'
#' @param result The output matrix of \code{\link{windfarmGA}} or
#' \code{\link{genAlgo}}, which has stored all relevant information. (matrix)
#' @param ask Should R wait for interaction for subsequent plotting.
#' Default is "T" (character)
#' @param spar A numeric value determining how exact a spline should
#' be drawn. Default is 0.5 (numeric)
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
#' plotEvolution(result, TRUE, 0.1)
#'}
#' @author Sebastian Gatscha
plotEvolution <- function(result,ask=T, spar=0.5){
  #result=result;ask=T; spar=0.5
  # library(stats);
  par(mfrow=c(1,1))
  result1 <- as.data.frame(do.call("rbind", result[,1]))

  plot(result1$minParkwirkungsg, xaxt='n', main="Park Efficiency per Generation", xlab="Generation",
       ylab="Park Efficiency in %", cex=1.2,col="red", pch=20,
       ylim= c(min(result1$minParkwirkungsg),max(result1$maxParkwirkungsg)))
  axis(1,at = 1:nrow(result1),tick=T)
  #mtext(result[,4],side=1,col=Col, at=1:length(dir1))
  grid(col = "black")
  points(result1$meanParkwirkungsg,ylab="MeanxParkwirkungsg", cex=1.2,col="blue", pch=20)
  points(result1$maxParkwirkungsg,ylab="maxParkwirkungsg", cex=1.2,col="green", pch=20)
  x <- 1:length(result1$MaxEnergyRedu)

  if (nrow(result)>=4){
    lmin <- smooth.spline(x,result1$minParkwirkungsg, spar=spar); lines(lmin, col='red', lwd=1.2)
    lmea <- smooth.spline(x,result1$meanParkwirkungsg, spar=spar); lines(lmea, col='blue', lwd=1.2)
    lmax <- smooth.spline(x,result1$maxParkwirkungsg, spar=spar); lines(lmax, col='green', lwd=1.2)
  }

  op <- par(ask=ask)
  on.exit(par(op))

  plot(result1$MeanEnergyRedu,xaxt='n',main="Energy Yield per Generation",xlab="Generation",ylab="Energy in kW", cex=1.2,
       col="blue", pch=20, ylim= c(min(result1$MinEnergyRedu),max(result1$MaxEnergyRedu)))
  axis(1,at = 1:nrow(result1),tick=T)
  #mtext(result[,4],side=1,col=Col, at=1:length(dir1))
  grid(col = "black")
  points(result1$MaxEnergyRedu,ylab="maxParkwirkungsg", cex=1.2,col="green", pch=20)

  if (nrow(result)>=4){
    emean <- smooth.spline(x,result1$MeanEnergyRedu, spar=spar); lines(emean, col='blue', lwd=1.2)
    emax <- smooth.spline(x,result1$MaxEnergyRedu, spar=spar); lines(emax, col='green', lwd=1.2)
  }


  return()
}

