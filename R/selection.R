#' @title Selection Method
#' @name selection
#' @description  Select a certain amount of individuals and recombine them to
#'   parental teams. Add the mean fitness value of both parents to the parental
#'   team. Depending on the selected \code{selstate}, the algorithm will either
#'   take always 50 percent or a variable percentage of the current population.
#'   The variable percentage depends on the evolution of the populations fitness
#'   values.
#' @export
#'
#' @param fit A list of all fitness-evaluated individuals
#' @param Grid Is the indexed grid output from \code{\link{grid_area}}
#' @param teil A numeric value that determines the selection percentage
#' @param elitism Boolean value which indicates whether elitism should be
#'   included or not.
#' @param nelit If \code{elitism} is TRUE, then this input variable determines
#'   the amount of individuals in the elite group.
#' @param selstate Determines which selection method is used, "FIX" selects a
#'   constant percentage and "VAR" selects a variable percentage, depending on
#'   the development of the fitness values.
#' @param verbose If TRUE, will print out further information.
#'
#' @family Genetic Algorithm Functions
#' @return Returns list with 2 elements. Element 1 is the binary encoded matrix
#'   which shows all selected individuals. Element 2 represent the mean fitness
#'   values of each parental team.
#' @examples \dontrun{
#' ## Create a random rectangular shapefile
#' library(sp)
#' Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+init=epsg:3035"
#' proj4string(Polygon1) <- CRS(Projection)
#'
#' ## Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
#' Grid1 <- grid_area(shape = Polygon1,resol = 200,prop = 1);
#' Grid <- Grid1[[1]]
#' AmountGrids <- nrow(Grid)
#'
#' startsel <- init_population(Grid,10,20);
#' wind <- as.data.frame(cbind(ws=12,wd=0))
#' wind <- list(wind, probab = 100)
#' fit <- fitness(selection = startsel, referenceHeight = 100, RotorHeight=100,
#'                SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,
#'                rot = 20, dirspeed = wind, 
#'                srtm_crop = "", topograp = FALSE, cclRaster = "")
#' allparks <- do.call("rbind",fit);
#'
#' ## SELECTION
#' ## print the amount of Individuals selected. Check if the amount 
#' ## of Turbines is as requested.
#' selec6best <- selection(fit, Grid, 2, TRUE, 6, "VAR")
#' selec6best <- selection(fit, Grid, 2, TRUE, 6, "FIX")
#' selec6best <- selection(fit, Grid, 4, FALSE, 6, "FIX")
#' }
selection         <- function(fit, Grid, teil, elitism, nelit, 
                               selstate, verbose) {
  if (missing(verbose)) {
    verbose <- FALSE
  }

  ## Make a DataFrame of the Fitness Function Output. Representing all x Parks with their fitness value.
  new <- do.call("rbind", fit)

  ## Get the unique Fitness value according to the RunID
  new1 <- subset.matrix(new, subset = !duplicated(new[,'Run']), 
                        select = c("Run", "Parkfitness", "EnergyOverall"))

  ## arrange descending, to dismiss last 2
  new1 <- new1[order(new1[,'Parkfitness'], decreasing = TRUE),]
  row.names(new1) <- NULL

  ## Elitarism - A certain amount of individuals will get their fitness values increased
  if (elitism) {
    if (nrow(new1) < nelit) {
      nelit <- nrow(new1)
    }
    if (verbose) {
      cat(paste("Elitarism activated. Best", nelit, "individuals are increased\n"))
    }
    ## Increase best 'nelit' individuals by factor 10
    new1[1:nelit,'Parkfitness'] <- new1[1:nelit,'Parkfitness'] * 10
  }

  ## Delete some of the worst individuals, if there are more than 10
  if (nrow(new1) > 10) {
    new1 <- new1[-seq(length(new1[,1]), nrow(new1) - 3 , -1), ]
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
      cat(paste("Selection Percentage:", round(100 / teil, 3), "\n"))
      cat(paste("FIX: How many parental individuals are selected:", nPar, "from",  
                nrow(new1), "with", ((1 / teil) * 100), "%\n"))
    }
  }
  ## Or the selection percentage is variable, depending on the development of the fitness values.
  if (selstate == "VAR") {
    # Select a variable amount of indivs. Teil comes from the "fuzzy logic" modell
    nPar <- ceiling(nrow(new1) / teil)
    if (verbose) {
      cat(paste("VAR: How many parental individuals are selected:", nPar, "from",
                nrow(new1), "with", ((1 / teil) * 100), "%\n"))
    }
  }

  ## Upper Limit of selected individuals is 100.
  if (nPar > 100) {nPar <- 100}

  ## Randomly sample some individuals, based on their fitness value
  childsRunID <- sample(new1[, 1], nPar, prob = new1[, 'Parkfitness'], 
                        replace = FALSE)

  ## Pick the parks with those list indeces. 
  ## (park with all config) and return Run and Rect_ID
  chile <- seq_len(length(childsRunID)) 
  child <- lapply(chile, function(z) {
    subset.matrix(fit[[childsRunID[z]]], 
                  select = c("Run", "Rect_ID", "Parkfitness"))
  })

  ## Create binary code for every parkconfiguration (individual) 
  ## (Turbine yes = 1, Turbine no = 0)
  childbin <- lapply(chile, function(i) {
    ## For every Child element, assign the total Grid to a binaryvariable[i], 
    ## and set all binary =0. Assign Run Value as well
    tmp <- cbind(Grid, 
                 "Run" = child[[i]][1, 'Run'],
                 "bin" = 0,
                 "Fitness" = child[[i]][1, 'Parkfitness'])

    for (e in 1:length(child[[i]][,1])) {
      ## For every element in a child (turbines) get his Rect_ID and 
      ## set binary to 1, where GridId = Rect_ID
      rectid <- child[[i]][e, 2]
      tmp[tmp[,'ID'] == rectid, 'bin'] <- 1
    }
    tmp
  })

  ## Create the parents
  parents <- vector("list", length(childsRunID) / 2)
  for (i in 1:(length(childsRunID) / 2) ) {
    parents[[i]] <- sample(x = childsRunID, 2, replace = FALSE)
    childsRunID <-  childsRunID[!(childsRunID %in% parents[[i]])]
  }
  parall <- unlist(parents)

  ## Create the children
  childbindf <- do.call("rbind", childbin)
  paralli <- lapply(1:length(parall), function(i) {
    subset.matrix( childbindf[which(childbindf[,'Run'] %in% parall[i]),],
                  select = c("ID", "Run", "bin", "Fitness"))
  })

  ## Squeeze list to data.frame and remove unnecessary columns 
  parentsall <- data.frame(paralli)
  
  lePar <- length(parentsall)

  ## Select the binary matrix and the fitness values of the parents and return as list
  parents_Fitness <- parentsall[1, c(1, seq(4, lePar, 4))]
  parentsall      <- parentsall[, c(1, seq(3, lePar, 4))]
  return(list(parentsall, parents_Fitness))
}

