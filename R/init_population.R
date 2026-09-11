#' @title Create a random initial Population
#' @name init_population
#' @description  Create \code{n_start} random sub-selections from the indexed
#'   grid and assign binary variable 1 to selected grids. This function
#'   initiates the genetic algorithm with a first random population and will
#'   only be needed in the first iteration.
#'
#' @export
#'
#' @param grid Indexed grid from [grid_area()] (X, Y, cell IDs).
#' @param n A numeric value indicating the amount of required turbines.
#' @param n_start A numeric indicating the amount of randomly generated initial
#'   individuals. Default is 100.
#'
#' @family Genetic Algorithm Functions
#' @return Returns a list of \code{n_start} initial individuals, each consisting
#'   of \code{n} turbines. Resulting list has the x and y coordinates, the grid
#'   cell ID and a binary variable of 1, indicating a turbine in the grid cell.
#'
#' @examples
#' library(sf)
#' ## Exemplary input Polygon with 2km x 2km:
#' area <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)
#'   ))),
#'   crs = 3035
#' ))
#'
#' Grid <- grid_area(area, 200, 1, TRUE)
#'
#' ## Create 5 individuals with 10 wind turbines each.
#' firstPop <- init_population(grid = Grid[[1]], n = 10, n_start = 5)
#'
init_population <- function(grid, n, n_start = 100) {
  Grid <- grid
  if (length(Grid[, "ID"]) <= n) {
    message("\n################### GA ERROR MESSAGE ###################\n")
    message(paste(
      "##### Amount Grid-cells: ", length(Grid[, "ID"]),
      "\n##### Amount of turbines: ", n, "\n"
    ))
    stop(
      "The amount of Grid-cells is smaller or equal the number of turbines.\n",
      "Decrease Resolution (fcr), number of turbines (n) or Rotorradius (Rotor)."
    )
  }
  if (length(Grid[, "ID"]) < (2 * n)) {
    message("\n################### GA ERROR MESSAGE ###################\n")
    message(paste(
      "##### Amount Grid-cells: ", length(Grid[, "ID"]),
      "\n##### Amount of turbines: ", n, "\n"
    ))
    stop(
      "The amount of Grid-cells should at least be double the amount of turbines.\n",
      "Decrease Resolution (fcr), number of turbines (n), or Rotorradius (Rotor)."
    )
  }
  ## Assign Binary Variable 0 to all Grid cells
  Grid <- cbind(Grid, "bin" = 0)
  ## Randomly sample n grid cells and assign 1 to bin column
  lapply(seq_len(n_start), function(i) {
    res <- Grid[Grid[, "ID"] %in% sample(
      x = Grid[, "ID"],
      size = n, replace = FALSE
    ), ,
    drop = FALSE
    ]
    res[, "bin"] <- 1L
    res
  })
}
