#' @title Map layouts to grid coordinates
#' @name get_grids
#' @description Map a population of layouts to grid coordinates. Accepts either
#'   an integer matrix of unique cell IDs (`n` turbines x individuals) or a
#'   legacy binary matrix (`n_gridcells` x individuals).
#'
#' @export
#'
#' @param layouts Binary matrix (legacy) or integer matrix of grid IDs
#'   (`n` turbines x individuals)
#' @param grid Indexed grid from [grid_area()]
#'
#' @family Helper Functions
#' @return Returns a list of all individuals with X and Y coordinates and the
#'   grid cell ID.
#'
#' @examples \donttest{
#' ## Create a random rectangular shapefile
#' library(sf)
#' area <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(0, 0, 2000, 2000, 0),
#'     c(0, 2000, 2000, 0, 0)
#'   ))),
#'   crs = 3035
#' ))
#'
#' ## Calculate a Grid and an indexed data.frame with coordinates and
#' ## grid cell Ids.
#' Grid1 <- grid_area(area = area, size = 200, prop = 1)
#' Grid <- Grid1[[1]]
#'
#' startsel <- init_population(Grid, 10, 20)
#' wind <- data.frame(ws = 12, wd = 0)
#' wind <- list(wind, probab = 100)
#' fit <- fitness(
#'   population = startsel, reference_height = 100, rotor_height = 100,
#'   surface_roughness = 0.3, area = area, rotor = 20,
#'   wind = wind, terrain = FALSE
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
#'   population = getRectV, reference_height = 100, rotor_height = 100,
#'   surface_roughness = 0.3, area = area, rotor = 20,
#'   wind = wind, terrain = FALSE
#' )
#' head(fit)
#' }
get_grids <- function(layouts, grid) {
  Grid <- grid
  if (!is.matrix(layouts)) {
    layouts <- matrix(layouts, ncol = 1)
  }
  binary <- isTRUE(all(layouts %in% c(0, 1))) &&
    nrow(layouts) == nrow(Grid)
  lapply(seq_len(ncol(layouts)), function(i) {
    if (binary) {
      Grid[layouts[, i] == 1, , drop = FALSE]
    } else {
      Grid[match(layouts[, i], Grid[, "ID"]), , drop = FALSE]
    }
  })
}
