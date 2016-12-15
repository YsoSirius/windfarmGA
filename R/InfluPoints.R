#' @title Find potentially influencing turbines
#' @name InfluPoints
#' @description  Find all turbines that could potentially influence
#' another turbine and save them to a list.
#'
#' @export
#'
#' @importFrom raster extent
#'
#' @param t A data.frame of the current individual with X and Y
#' coordinates (data.frame)
#' @param wnkl A numeric value indicating the angle, at which no wake
#' influences are considered. Default is 20 degrees. (numeric)
#' @param dist A numeric value indicating the distance, after which
#' the wake effects are considered to be eliminated.
#' Default is 100km. (numeric)
#' @param polYgon A shapefile representing the considered area
#' (SpatialPolygons)
#' @param dirct A numeric value indicating the current wind direction
#' (numeric)
#' @param plotAngles A logical variable, which is used to plot the
#' distances and angles. Default is FALSE. (logical)
#'
#' @return Returns a list of all individuals of the current generation
#' which could potentially influence other turbines. List includes the
#' relevant coordinates, the distances and angles in between and assigns
#' the Point ID. (list)
#'
#' @examples \donttest{
#' library(sp);library(raster)
#' ## Exemplary input Polygon with 2km x 2km:
#' polYgon <- Polygon(rbind(c(0, 0), c(0, 2000),
#' c(2000, 2000), c(2000, 0)))
#' polYgon <- Polygons(list(polYgon),1);
#' polYgon <- SpatialPolygons(list(polYgon))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(polYgon) <- CRS(Projection); plot(polYgon,axes=T)
#'
#' t <- as.matrix(cbind(x=runif(10,0,raster::extent(polYgon)[2]),
#'      y=runif(10,0,raster::extent(polYgon)[4])))
#' wnkl=20
#' dist=100000
#' dirct=0
#'
#' resInfluPoi <- InfluPoints(t,wnkl,dist,polYgon,dirct,plotAngles=TRUE)
#' }
#' @author Sebastian Gatscha
InfluPoints       <- function(t, wnkl, dist,polYgon,dirct,plotAngles=FALSE) {
  # t = xyBgldMa; dist = distanz; polYgon = Polygon; dirct = angle

  # For every turbine in the wind farm, find all other turbines, that stand in front, next
  # and inside a certain angle of the incoming wind direction and assing to the list
  pointList <- list();
  for (i in 1:(length(t[,1]))) {
    ## Calculate the angles and distances of pontentially influencing turbines (data.frame)
    ee11 <- VekWinkelCalc(t = t, o = i, wkl = wnkl, distanz = dist, polYgon = polYgon, plotAngles);
    ## Add the wind direction to the data.frame
    ee11[,'Windrichtung'] <- dirct
    ## Assign the iteration as point ID of the current turbine to the data.frame. Necessary for multiple wake effects
    ee11[,'Punkt_id'] <- i;
    ## Assign the data.frame as list element
    pointList[[i]] <- ee11;
  }
  return(pointList)
}


##importFrom GenAlgo VekWinkelCalc


