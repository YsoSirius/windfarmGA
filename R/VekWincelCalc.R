#' @title Calculate distances and angles of possibly influencing turbines
#' @name VekWinkelCalc
#' @description  Calculate the relevant distances \code{\link{PointToLine2}}
#' and angles \code{\link{WinkelCalc}} for a given turbine location and
#' all potentially influencing turbines.
#' @export
#'
#' @importFrom raster extent plot
#' @importFrom graphics title plot points
#'
#' @param t A matrix of the current individual with x and y coordinates
#' (matrix)
#' @param o A numeric value indicating the index of the current turbine
#' (numeric)
#' @param wkl A numeric value indicating the angle, at which no wake
#' influences are considered. Default is 20 degrees. (numeric)
#' @param distanz A numeric value indicating the distance, after which
#' the wake effects are considered to be eliminated. Default is 100km.
#' (numeric)
#' @param polYgon A shapefile representing the considered area
#' (SpatialPolygons)
#' @param plotAngles A logical variable, which is used to plot the
#' distances and angles. Default is FALSE (logical)
#' @return Returns a matrix with the distances and angles of
#' potentially influencing turbines (matrix)
#' @examples
#' library(sp)
#' library(raster)
#' 
#' ## Exemplary input Polygon with 2km x 2km:
#' polYgon <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
#' polYgon <- Polygons(list(polYgon),1);
#' polYgon <- SpatialPolygons(list(polYgon))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#'                +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(polYgon) <- CRS(Projection); plot(polYgon,axes=TRUE)
#' 
#' ## Create a random windfarm with 10 turbines
#' t <- as.matrix(cbind(x=runif(10,0,raster::extent(polYgon)[2]),
#'      y=runif(10,0,raster::extent(polYgon)[4])))
#' wnkl <- 20
#' distanz <- 100000
#' 
#' ## Evaluate and plot for every turbine all other potentially influencing turbines
#' potInfTur <- list()
#' for (i in 1:(length(t[,1]))) {
#'   potInfTur[[i]] <- VekWinkelCalc(t = t, o = i, wkl = wnkl,
#'                    distanz = distanz, polYgon = polYgon, plotAngles=TRUE);
#' }
#' potInfTur
#'
#' @author Sebastian Gatscha
VekWinkelCalc     <- function(t,o, wkl, distanz, polYgon, plotAngles) {
  # t = t; o = 1; wkl = wnkl;
  # distanz = distanz; polYgon = polYgon; plotAngles=TRUE
  
  ## Get coordinates of actual turbine location
  WKA_akt <- c(x = t[o,1L], y = t[o,2L])
  ## Find all turbines that are in front of actual turbine
  xynew1 <- subset.matrix(x = t, subset = (t[o,2L] < t[,2L]))
  
  if (plotAngles){
    graphics::plot(t, xlim = raster::extent(polYgon)[1:2], ylim = raster::extent(polYgon)[3:4],
                   col.axis = "darkblue", xlab = "X-Coordinates", ylab = "Y-Coordinates");
    title(main = "Potentially Influential Points",
          sub = paste("PointNr: ", o,";","Distance: ", distanz,"Meter",";", "Angle: ", wkl,"Degrees"),
          outer = FALSE, cex.main = 1, cex.sub = 1)
    raster::plot(polYgon, add = TRUE)
    ## Plot the actual turbine in green, the ones in front in red
    points(x = WKA_akt[1], y = WKA_akt[2], col = "green", pch = 20)
    points(xynew1[,1], y = xynew1[,2], col = "red", pch = 20)
  }
  
  ## Are there turbines in front or not? If yes, calculate distances and angles and check if they might have an influence or not
  len2 <- length(xynew1[,1L])
  if (len2 != 0L) {
    ## If turbines are in front of the current turbine, create a list and save only the ones that could possibly influence others
    datalist = vector("list", len2)
    for (indx in 1:len2) {
      ## Calcuate the distances and angles of the imaginary right triangle
      P2LFu <- PointToLine2(WKA_akt, xynew1[indx,], plotAngles)
      # P2LFu <- point_2_line_CPP(WKA_akt, xynew1[indx,])
      # winkel <- t(WinkelCalc(xynew1[indx,], WKA_akt, P2LFu[5:6]))
      winkel <- angles_CPP(xynew1[indx,], WKA_akt, P2LFu[5:6])
      data <- c(P2LFu, winkel) 
      datalist[[indx]] <- data
    }
    res <- do.call(rbind, datalist)
    colnames(res) <- c("Ax", "Ay", "Bx", "By", "Cx", "Cy", "Laenge_C", "Laenge_B", "Laenge_A", "alpha", "betha", "gamma")
    
    ## Dismiss the ones with too high distances or big angles.
    dl <- subset.matrix(res, subset = res[,'alpha'] < wkl & res[,'Laenge_B'] < distanz) 
    
    if (plotAngles){
      points(dl[,'Ax'], dl[,'Ay'], col = "orange", pch = 20, cex = 2)
    }
    
    ## If no turbines exist, the variable "dl" will be deleted.
    if (nrow(dl) == 0){rm(dl)}
    
    if (exists("dl")) {
      ## If possible influencing turbines exist, save them in the variable "DataLun3"
      DataLun3 <- dl
    } else {
      ## If no possible influencing turbines remain and the variable "dl" therefoe doesnt exist,
      ## the variable "DataLun3" is fulled with default Values of 0 for distances and angles
      DataLun3 <- matrix(data = c(0, 0, t[o,1], t[o,2], (rep(0, 8))), nrow = 1, ncol = 12);
      colnames(DataLun3) <- c("Ax","Ay","Bx","By","Cx","Cy","Laenge_C","Laenge_B","Laenge_A",
                              "alpha","betha","gamma")
    }
  } else  {
    # If no turbines are in front, the variabel "DataLun3" is again filled with default 
    ## values of 0 for angles and distances.
    DataLun3 <- matrix(data = c(0,0, t[o,1], t[o,2], (rep(0,8))), nrow = 1, ncol = 12);
    colnames(DataLun3) <- c("Ax","Ay","Bx","By","Cx","Cy","Laenge_C","Laenge_B","Laenge_A",
                            "alpha","betha","gamma")
  }
  
  return(DataLun3);
}
