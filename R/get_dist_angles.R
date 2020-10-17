#' @title Calculate distances and angles of possibly influencing turbines
#' @name get_dist_angles
#' @description Calculate distances and angles for a turbine and all it's
#'   potentially influencing turbines.
#' @export
#'
#' @param t A matrix of the current individual with x and y coordinates
#' @param o A numeric value indicating the index of the current turbine
#' @param wkl A numeric value indicating the angle, at which no wake influences
#'   are considered. Default is 20 degrees.
#' @param distanz A numeric value indicating the distance, after which the wake
#'   effects are considered to be eliminated. Default is 100km.
#' @param polYgon A shapefile representing the considered area
#' @param plotAngles A logical variable, which is used to plot the distances and
#'   angles. Default is FALSE
#'
#' @family Wind Energy Calculation Functions
#' @return Returns a matrix with the distances and angles of potentially
#'   influencing turbines
#' @examples
#' library(sp)
#' library(raster)
#' 
#' ## Exemplary input Polygon with 2km x 2km:
#' polYgon <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
#' polYgon <- Polygons(list(polYgon),1)
#' polYgon <- SpatialPolygons(list(polYgon))
#' Projection <- "+init=epsg:3035"
#' proj4string(polYgon) <- CRS(Projection); plot(polYgon, axes = TRUE)
#' 
#' ## Create a random windfarm with 10 turbines
#' t <- as.matrix(cbind(x = runif(10, 0, raster::extent(polYgon)[2]),
#'      y = runif(10, 0, raster::extent(polYgon)[4])))
#' wnkl <- 20
#' distanz <- 100000
#' 
#' ## Evaluate and plot for every turbine all other potentially influencing turbines
#' potInfTur <- list()
#' for (i in 1:(length(t[,1]))) {
#'   potInfTur[[i]] <- get_dist_angles(t = t, o = i, wkl = wnkl,
#'                    distanz = distanz, polYgon = polYgon, plotAngles = TRUE)
#' }
#' potInfTur
#'
get_dist_angles     <- function(t, o, wkl, distanz, polYgon, plotAngles) {
  col_names <- c("Ax", "Ay", "Bx", "By", "Cx", "Cy",
               "Laenge_C", "Laenge_B", "Laenge_A",
               "alpha", "betha", "gamma")
  ## Get coordinates of actual turbine location
  turbine_loc <- c(x = t[o, 1L], y = t[o, 2L])
  ## Find all turbines that are in front of actual turbine
  turbines_ahead <- subset.matrix(x = t, subset = (t[o, 2L] < t[, 2L]))

  if (plotAngles) {
    graphics::plot(t, xlim = raster::extent(polYgon)[1:2],
                   ylim = raster::extent(polYgon)[3:4],
                   col.axis = "darkblue",
                   xlab = "X-Coordinates", ylab = "Y-Coordinates")
    title(main = "Potentially Influential Points",
          sub = paste("PointNr: ", o,";","Distance: ",
                      distanz,"Meter",";", "Angle: ", wkl,"Degrees"),
          outer = FALSE, cex.main = 1, cex.sub = 1)
    raster::plot(polYgon, add = TRUE)
    ## Plot the actual turbine in green, the ones in front in red
    points(x = turbine_loc[1], y = turbine_loc[2], col = "green", pch = 20)
    points(turbines_ahead[,1], y = turbines_ahead[,2], col = "red", pch = 20)
  }

  ## Are there turbines in front or not? If yes, calculate distances and
  ## angles and check if they might have an influence or not
  len2 <- length(turbines_ahead[,1L])
  if (len2 != 0L) {
    ## If turbines are in front of the current turbine, create a list and
    ## save only the ones that could possibly influence others
    datalist <- lapply(1:len2, function(i) {
      P2LFu <- point_2_line_CPP(turbine_loc, turbines_ahead[i, ])
      winkel <- angles_CPP(turbines_ahead[i,], turbine_loc, P2LFu[5:6])
      c(P2LFu, winkel) 
    })
    res <- matrix(unlist(datalist), ncol = 12, byrow = TRUE)
    colnames(res) <- col_names

    ## Dismiss the ones with too high distances or big angles.
    dl <- subset.matrix(res, 
                        subset = 
                          res[, 'alpha'] < wkl & res[, 'Laenge_B'] < distanz)

    if (plotAngles) {
      points(dl[,'Ax'], dl[,'Ay'], col = "orange", pch = 20, cex = 2)
    }
    ## TODO can i take length here?
    ## Are turbines left after subsetting?
    if (nrow(dl) != 0) {
      ## If possible influencing turbines exist, save them in the variable "DataLun3"
      DataLun3 <- dl
    } else {
      ## TODO WHY??
      ## If no possible influencing turbines remain and the variable "dl" therefoe is empty,
      ## the variable "DataLun3" is filled with default Values of 0 for distances and angles
      DataLun3 <- matrix(data = c(0, 0, t[o, 1], t[o, 2], rep(0, 8)), nrow = 1, ncol = 12)
      colnames(DataLun3) <- col_names
    }
  } else {
    ## TODO WHY??
    # If no turbines are in front, the variabel "DataLun3" is again filled with default 
    ## values of 0 for angles and distances.
    DataLun3 <- matrix(data = c(0, 0, t[o, 1], t[o, 2], rep(0, 8)), nrow = 1, ncol = 12)
    colnames(DataLun3) <- col_names
  }

  return(DataLun3)
}
