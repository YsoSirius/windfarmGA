#' @title Map layouts to grid coordinates
#' @name get_grids
#' @description Map a population of layouts to grid coordinates. Accepts either
#'   an integer matrix of unique cell IDs (`n` turbines × individuals) or a
#'   legacy binary matrix (`n_gridcells` × individuals).
#'
#' @export
#'
#' @param trimtonOut Binary matrix (legacy) or integer matrix of grid IDs
#'   (`n` turbines × individuals)
#' @param Grid Grid of the considered area
#'
#' @family Helper Functions
#' @return Returns a list of all individuals with X and Y coordinates and the
#'   grid cell ID.
#'
#' @examples \donttest{
#' ## Create a random rectangular shapefile
#' library(sf)
#' Polygon1 <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(0, 0, 2000, 2000, 0),
#'     c(0, 2000, 2000, 0, 0)
#'   ))),
#'   crs = 3035
#' ))
#'
#' ## Calculate a Grid and an indexed data.frame with coordinates and
#' ## grid cell Ids.
#' Grid1 <- grid_area(shape = Polygon1, size = 200, prop = 1)
#' Grid <- Grid1[[1]]
#'
#' startsel <- init_population(Grid, 10, 20)
#' wind <- data.frame(ws = 12, wd = 0)
#' wind <- list(wind, probab = 100)
#' fit <- fitness(
#'   selection = startsel, referenceHeight = 100, RotorHeight = 100,
#'   SurfaceRoughness = 0.3, Polygon = Polygon1, resol1 = 200, rot = 20,
#'   dirspeed = wind, srtm_crop = "", topograp = FALSE, cclRaster = ""
#' )
#' allparks <- do.call("rbind", fit)
#'
#' ## SELECTION (n unique cell IDs per individual)
#' selec6best <- selection(fit, Grid, 2, TRUE, 6, "VAR")
#'
#' ## Set-crossover and swap-mutation keep exactly n turbines.
#' cross_ids <- set_crossover(selec6best[[1]], Grid[, "ID"], uplimit = 20)
#' mut_ids <- swap_mutation(cross_ids, Grid[, "ID"], p = 0.2)
#'
#' ## Look up XY coordinates for the next fitness evaluation.
#' getRectV <- get_grids(mut_ids, Grid)
#' fit <- fitness(
#'   selection = getRectV, referenceHeight = 100, RotorHeight = 100,
#'   SurfaceRoughness = 0.3, Polygon = Polygon1, resol1 = 200, rot = 20,
#'   dirspeed = wind, srtm_crop = "", topograp = FALSE, cclRaster = ""
#' )
#' head(fit)
#' }
get_grids <- function(trimtonOut, Grid) {
  if (!is.matrix(trimtonOut)) {
    trimtonOut <- matrix(trimtonOut, ncol = 1)
  }
  binary <- isTRUE(all(trimtonOut %in% c(0, 1))) &&
    nrow(trimtonOut) == nrow(Grid)
  lapply(seq_len(ncol(trimtonOut)), function(i) {
    if (binary) {
      Grid[trimtonOut[, i] == 1, , drop = FALSE]
    } else {
      Grid[match(trimtonOut[, i], Grid[, "ID"]), , drop = FALSE]
    }
  })
}
