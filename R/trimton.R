#' @title Adjust the amount of turbines per windfarm
#' @name trimton
#' @description  Adjust the mutated individuals to the required amount of
#'   turbines.
#'
#' @export
#'
#' @param mut A binary matrix with the mutated individuals
#' @param nturb A numeric value indicating the amount of required turbines
#' @param allparks A data.frame consisting of all individuals of the current
#'   generation
#' @param nGrids A numeric value indicating the total amount of grid cells
#' @param trimForce A boolean value which determines which adjustment method
#'   should be used. TRUE uses a probabilistic approach and FALSE uses a random
#'   approach
#' @param seed Set a seed for comparability. Default is NULL
#'
#' @family Genetic Algorithm Functions
#' @return Returns a binary matrix with the correct amount of turbines per
#'   individual
#' @examples \donttest{
#' ## Create a random rectangular shapefile
#' library(sp)
#' Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+init=epsg:3035"
#' proj4string(Polygon1) <- CRS(Projection)
#'
#' ## Create a uniform and unidirectional wind data.frame and plots the
#' ## resulting wind rose
#' ## Uniform wind speed and single wind direction
#' data.in <- as.data.frame(cbind(ws=12,wd=0))
#'
#' ## Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
#' Grid1 <- grid_area(shape = Polygon1,resol = 200,prop = 1);
#' Grid <- Grid1[[1]]
#' AmountGrids <- nrow(Grid)
#'
#' startsel <- init_population(Grid,10,20);
#' wind <- as.data.frame(cbind(ws=12,wd=0))
#' wind <- list(wind, probab = 100)
#' fit <- fitness(selection = startsel,referenceHeight = 100, RotorHeight=100,
#'               SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,rot=20, dirspeed = wind,
#'               srtm_crop="",topograp=FALSE,cclRaster="")
#' allparks <- do.call("rbind",fit);
#'
#' ## SELECTION
#' ## print the amount of Individuals selected.
#' ## Check if the amount of Turbines is as requested.
#' selec6best <- selection(fit, Grid,2, TRUE, 6, "VAR");
#' selec6best <- selection(fit, Grid,2, TRUE, 6, "FIX");
#' selec6best <- selection(fit, Grid,4, FALSE, 6, "FIX");
#'
#' ## CROSSOVER
#' ## u determines the amount of crossover points,
#' ## crossPart determines the method used (Equal/Random),
#' ## uplimit is the maximum allowed permutations
#' crossOut <- crossover(selec6best, 2, uplimit = 300, crossPart="RAN");
#' crossOut <- crossover(selec6best, 7, uplimit = 500, crossPart="RAN");
#' crossOut <- crossover(selec6best, 3, uplimit = 300, crossPart="EQU");
#'
#' ## MUTATION
#' ## Variable Mutation Rate is activated if more than 2 individuals represent
#' ## the current best solution.
#' mut <- mutation(a = crossOut, p = 0.3, NULL);
#'
#' ## TRIMTON
#' ## After Crossover and Mutation, the amount of turbines in a windpark change and have to be
#' ## corrected to the required amount of turbines.
#' mut1 <- trimton(mut = mut, nturb = 10, allparks = allparks, nGrids = AmountGrids,
#'                 trimForce=FALSE)
#' colSums(mut)
#' colSums(mut1)
#'
#'}
trimton           <- function(mut, nturb, allparks, nGrids, trimForce, seed){
  if (missing(seed)) {seed <- NULL}
  k <- 0.5
  nGrids1 <- 1:nGrids

  ## TODO Does it have to be in for-loop????
  # Calculate probability, that Turbine is selected to be eliminated.
  indivprop <- subset.matrix(allparks, select = c("Rect_ID", "Parkfitness", "AbschGesamt"))
  # Group mean wake effect and fitness value of a grid cell.
  indivprop <- aggregate(indivprop[,2:3], by = list(indivprop[,1]), FUN = mean)
  colnames(indivprop) <- c("Rect_ID","Parkfitness","AbschGesamt")

  lepa <- length(mut[1,])
  mut1 <- vector("list", lepa)
  for (i in 1:lepa) {
    # i=75
    tmp <- mut[,i]
    e <- tmp == 1
    ## How much turbines are there too many?
    zviel <- sum(e) - nturb
    ## Which grid cell IDs have a turbine
    welche <- which(e)

    propwelche <- cbind(
      RectID = welche,
      Prop = rep(mean(indivprop[,'AbschGesamt']), length(welche)))
    if (trimForce) {
      propexi <- indivprop[indivprop[,'Rect_ID'] %in% welche,]
      npt  <- (1 + ((max(propexi[,'AbschGesamt']) - propexi[,'AbschGesamt']) / (1 + max(propexi[,'AbschGesamt']))))
      npt0 <- (1 + ((max(propexi[,'Parkfitness']) - propexi[,'Parkfitness']) / (1 + max(propexi[,'Parkfitness'])))) ^ k
      NewProb <- 1 / (npt / npt0)
      propwelche[welche %in%  indivprop[,'Rect_ID'], 'Prop'] <- NewProb
    }

    propwelcheN <-  cbind(
      Rect_ID = nGrids1,
      Prop = rep(min(indivprop[,'AbschGesamt']), nGrids))
    if (trimForce) {
      propexiN <- indivprop[indivprop[,'Rect_ID'] %in% nGrids1,]
      npt1 <- (1 + ((max(propexiN[,'AbschGesamt']) - propexiN[,'AbschGesamt']) / (1 + max(propexiN[,'AbschGesamt']))))
      npt2 <- (1 + ((max(propexiN[,'Parkfitness']) - propexiN[,'Parkfitness']) / (1 + max(propexiN[,'Parkfitness'])))) ^ k
      NewProb1 <- npt1 / npt2
      propwelcheN[propwelcheN[,'Rect_ID'] %in%  indivprop[,'Rect_ID'], 'Prop'] <- NewProb1
      if (!all(propwelcheN[,'Rect_ID'] %in%  indivprop[,'Rect_ID'] == TRUE)) {
        propwelcheN[!propwelcheN[,'Rect_ID'] %in%  indivprop[,'Rect_ID'], 'Prop'] <- min(NewProb1)
      }
    }

    propwelcheN <- propwelcheN[!propwelcheN[,'Rect_ID'] %in% welche,]
    ## P1 - Deleting Turbines
    prob1 <- propwelche[,'Prop']
    ## P2 - Adding Turbines
    prob2 <- propwelcheN[,'Prop']

    if (zviel != 0) {
      if (zviel > 0) {
        if (trimForce) {
          # Delete turbines with Probability
          if (!is.null(seed)) {set.seed(as.integer(seed))}
          smpra <- base::sample(welche, zviel, replace = FALSE, prob = prob1)
          # smpra <- sample(welche, zviel, replace = FALSE, prob = prob1)
        } else {
          # Delete them randomly
          if (!is.null(seed)) {set.seed(as.integer(seed))}
          smpra <- base::sample(welche, zviel, replace = FALSE)
        }
        # Delete the 1 entry and make no turbine.
        tmp[smpra] <- 0
        mut1[[i]] <- tmp
      } else {
        if (trimForce) {
          # Add turbines with Probability
          if (!is.null(seed)) {set.seed(as.integer(seed))}
          smpra <- sample(propwelcheN[,'Rect_ID'], -zviel, replace = FALSE, prob = prob2)
        } else {
          # Add turbines randomly
          if (!is.null(seed)) {set.seed(as.integer(seed))}
          smpra <- sample(propwelcheN[,'Rect_ID'], -zviel, replace = FALSE)
        }
        # Assign 1 to binary code. So Turbine is created here.
        tmp[smpra] <- 1
        mut1[[i]] <- tmp
      }
    } else {
      mut1[[i]] <- tmp
    }
  }
  mut1 <- do.call("cbind", mut1)
  return(mut1)
}
