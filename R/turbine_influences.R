#' @title Find potentially influencing turbines
#' @name turbine_influences
#' @description  Find all turbines that could potentially influence another
#'   turbine and save them to a list.
#'
#' @export
#'
#' @param t A data.frame of the current individual with X and Y coordinates
#' @param wnkl A numeric value indicating the angle, at which no wake influences
#'   are considered. Default is 20 degrees.
#' @param dist A numeric value indicating the distance, after which the wake
#'   effects are considered to be eliminated. Default is 100km.
#' @param polYgon A shapefile representing the considered area
#' @param dirct A numeric value indicating the current wind direction
#' @param plotAngles A logical variable, which is used to plot the distances and
#'   angles. Default is FALSE.
#'
#' @family Wind Energy Calculation Functions
#' @return Returns a list of all individuals of the current generation which
#'   could potentially influence other turbines. List includes the relevant
#'   coordinates, the distances and angles in between and assigns the Point ID.
#'
#' @examples
#' library(sp)
#' library(raster)
#' ## Exemplary input Polygon with 2km x 2km:
#' polYgon <- Polygon(rbind(c(0, 0), c(0, 2000),
#' c(2000, 2000), c(2000, 0)))
#' polYgon <- Polygons(list(polYgon), 1)
#' polYgon <- SpatialPolygons(list(polYgon))
#' Projection <- "+init=epsg:3035"
#' proj4string(polYgon) <- CRS(Projection)
#'
#' t <- as.matrix(cbind(x = runif(10, 0, extent(polYgon)[2]),
#'      y = runif(10, 0, extent(polYgon)[4])))
#' wnkl <- 20
#' dist <- 100000
#' dirct <- 0
#'
#' res <- turbine_influences(t, wnkl, dist, polYgon, dirct, plotAngles = TRUE)
#'
turbine_influences       <- function(t, wnkl, dist, polYgon, dirct,
                              plotAngles = FALSE) {
  ## For every turbine in the wind farm, find all other turbines,
  ## that stand in front, next and inside a certain angle of the
  ## incoming wind direction and assing to the list
  lapply(seq_along(t[, 1]), function(i) {
    ## Calculate the angles and distances of potentially influencing turbines
    ee11 <- get_dist_angles(t = t, o = i, wkl = wnkl, distanz = dist,
                          polYgon = polYgon, plotAngles = plotAngles)
    ## Add the wind direction to the data.frame
    ## Assign the iteration as point ID of the current turbine
    ## Necessary for multiple wake effects
    cbind(ee11, "Windrichtung" = dirct, "Punkt_id" = i)
  })
}
