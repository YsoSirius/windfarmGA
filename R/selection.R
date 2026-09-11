#' @title Selection Method
#' @name selection
#' @description  Select a certain amount of individuals and recombine them to
#'   parental teams. Add the mean fitness value of both parents to the parental
#'   team. Depending on the selected \code{selstate}, the algorithm will either
#'   take always 50 percent or a variable percentage of the current population.
#'   The variable percentage depends on the evolution of the populations fitness
#'   values. With \code{elitism = TRUE} the best individuals are always included
#'   in the mating pool.
#' @export
#'
#' @inheritParams genetic_algorithm
#' @param fit A list of all fitness-evaluated individuals
#' @param Grid Is the indexed grid output from \code{\link{grid_area}}
#' @param teil A numeric value that determines the selection percentage
#' @param verbose If TRUE, will print out further information.
#'
#' @family Genetic Algorithm Functions
#' @return Returns a list with 2 elements. Element 1 is an integer matrix of
#'   selected layouts (`n` turbines × selected individuals), each column a
#'   set of unique grid cell IDs. Element 2 is the fitness of each selected
#'   individual.
#' @examples \donttest{
#' ## Exemplary input Polygon with 2km x 2km:
#' library(sf)
#' Polygon1 <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)
#'   ))),
#'   crs = 3035
#' ))
#'
#' ## Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
#' Grid1 <- grid_area(shape = Polygon1, size = 200, prop = 1)
#' Grid <- Grid1[[1]]
#' AmountGrids <- nrow(Grid)
#'
#' startsel <- init_population(Grid, 10, 20)
#' wind <- as.data.frame(cbind(ws = 12, wd = 0))
#' wind <- list(wind, probab = 100)
#' fit <- fitness(
#'   selection = startsel, referenceHeight = 100, RotorHeight = 100,
#'   SurfaceRoughness = 0.3, Polygon = Polygon1, resol1 = 200,
#'   rot = 20, dirspeed = wind,
#'   srtm_crop = "", topograp = FALSE, cclRaster = ""
#' )
#' allparks <- do.call("rbind", fit)
#' ## SELECTION
#' ## print the amount of Individuals selected. Check if the amount
#' ## of Turbines is as requested.
#' selec6best <- selection(fit, Grid, 2, TRUE, 6, "VAR")
#' selec6best <- selection(fit, Grid, 2, TRUE, 6, "FIX")
#' selec6best <- selection(fit, Grid, 4, FALSE, 6, "FIX")
#' }
selection <- function(fit, Grid, teil, elitism, nelit, selstate, verbose) {
  if (missing(verbose)) {
    verbose <- FALSE
  }

  ## Make a DataFrame of the Fitness Function Output. Representing all x Parks with their fitness value.
  new <- do.call("rbind", fit)

  ## Get the unique Fitness value according to the RunID
  new1 <- subset.matrix(new,
    subset = !duplicated(new[, "Run"]),
    select = c("Run", "Parkfitness", "EnergyOverall")
  )

  ## arrange descending, to dismiss last 2
  new1 <- new1[order(new1[, "Parkfitness"], decreasing = TRUE), ]
  row.names(new1) <- NULL

  ## Elitism: keep the best individuals in the mating pool (no fitness scaling)
  elite_runs <- integer(0)
  if (elitism) {
    if (nrow(new1) < nelit) {
      nelit <- nrow(new1)
    }
    if (verbose) {
      message(paste("Elitism activated. Best", nelit, "individuals are kept"))
    }
    elite_runs <- new1[seq_len(nelit), "Run"]
  }

  ## Delete some of the worst individuals, if there are more than 10
  if (nrow(new1) > 10) {
    new1 <- new1[-seq(length(new1[, 1]), nrow(new1) - 3, -1), ]
  }

  ## The next two methods determine how a selection percentage is calculated
  # Either a fixed percentage of 50% is used
  if (selstate == "FIX") {
    # Select a fixed amount of indivs. # Teil=2 takes always 50% of population
    if (teil != 1) {
      teil <- 2
    }
    nPar <- ceiling(nrow(new1) / teil)
    if (verbose) {
      message(paste("Selection Percentage:", round(100 / teil, 3)))
      message(paste(
        "FIX: How many parental individuals are selected:", nPar, "from",
        nrow(new1), "with", ((1 / teil) * 100), "%"
      ))
    }
  }
  ## Or the selection percentage is variable, depending on the development of the fitness values.
  if (selstate == "VAR") {
    # Select a variable amount of indivs. Teil comes from the "fuzzy logic" modell
    nPar <- ceiling(nrow(new1) / teil)
    if (verbose) {
      message(paste(
        "VAR: How many parental individuals are selected:", nPar, "from",
        nrow(new1), "with", ((1 / teil) * 100), "%\n"
      ))
    }
  }

  ## Upper Limit of selected individuals is 100.
  max_selec <- getOption("windfarmGA.max_selection", 300)
  if (nPar > max_selec) {
    nPar <- max_selec
  }
  if (nPar > 1 && (nPar %% 2 == 1)) {
    nPar <- nPar - 1
  }

  ## Randomly sample some individuals, based on their fitness value.
  ## Elites are always included in the mating pool.
  if (length(elite_runs)) {
    n_rest <- max(0, nPar - length(elite_runs))
    pool <- new1[!new1[, "Run"] %in% elite_runs, , drop = FALSE]
    if (n_rest > 0 && nrow(pool) > 0) {
      n_rest <- min(n_rest, nrow(pool))
      extra <- sample(pool[, "Run"], n_rest,
                      prob = pool[, "Parkfitness"], replace = FALSE)
      childsRunID <- c(elite_runs, extra)
    } else {
      childsRunID <- elite_runs[seq_len(min(nPar, length(elite_runs)))]
    }
  } else {
    childsRunID <- sample(new1[, 1], nPar,
      prob = new1[, "Parkfitness"],
      replace = FALSE
    )
  }

  ## Pick layouts as unique grid-cell IDs (combinatorial genome)
  chile <- seq_len(length(childsRunID))
  n_turb <- nrow(fit[[childsRunID[1]]])
  ids <- vapply(chile, function(z) {
    sort(as.integer(fit[[childsRunID[z]]][, "Rect_ID"]))
  }, integer(n_turb))
  if (!is.matrix(ids)) {
    ids <- matrix(ids, nrow = n_turb)
  }
  fitness_sel <- vapply(chile, function(z) {
    as.numeric(fit[[childsRunID[z]]][1, "Parkfitness"])
  }, numeric(1))

  if (!all(ids %in% Grid[, "ID"])) {
    stop("Selected layouts contain grid IDs that are not in Grid.")
  }

  return(list(ids, fitness_sel))
}
