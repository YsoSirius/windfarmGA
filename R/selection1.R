#' @title Selection Method
#' @name selection1
#' @description  Select a certain amount of individuals and recombine them
#' to parental teams. Add the mean fitness value of both parents to
#' the parental team. Depending on the selected \code{selstate}, the
#' algorithm will either take always 50 percent or a variable percentage
#' of the current population. The variable percentage depends on the
#' evolution of the populations fitness values.
#' @export
#'
#' @param fit A list of all fitness-evaluated individuals (list)
#' @param Grid Is the indexed grid output from \code{\link{GridFilter}}
#' (data.frame)
#' @param teil A numeric value that determines the selection percentage
#' (numeric)
#' @param elitism Boolean value which indicates whether elitism should be
#' included or not. (logical)
#' @param nelit If \code{elitism} is TRUE, then this input variable
#' determines the amount of individuals in the elite group. (numeric)
#' @param selstate Determines which selection method is used, "FIX" selects
#' a constant percentage and "VAR" selects a variable percentage, depending
#' on the development of the fitness values. (character)
#' @param verbose If TRUE, will print out further information. 
#'
#' @return Returns list with 2 elements. Element 1 is the binary encoded
#' matrix which shows all selected individuals. Element 2 represent the mean
#' fitness values of each parental team. (list)
#' @examples \dontrun{
#' ## Create a random rectangular shapefile
#' library(sp)
#' Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' plot(Polygon1,axes=TRUE)
#'
#' ## Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
#' Grid1 <- GridFilter(shape = Polygon1,resol = 200,prop = 1);
#' Grid <- Grid1[[1]]
#' AmountGrids <- nrow(Grid)
#'
#' startsel <- StartGA(Grid,10,20);
#' wind <- as.data.frame(cbind(ws=12,wd=0))
#' fit <- fitness(selection = startsel,referenceHeight = 100, RotorHeight=100,
#'                SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,rot=20, dirspeed = wind,
#'                srtm_crop="",topograp=FALSE,cclRaster="")
#' allparks <- do.call("rbind",fit);
#'
#' ## SELECTION
#' ## print the amount of Individuals selected. Check if the amount of Turbines is as requested.
#' selec6best <- selection1(fit, Grid, 2, T, 6, "VAR", NULL)
#' selec6best <- selection1(fit, Grid, 2, T, 6, "FIX", NULL)
#' selec6best <- selection1(fit, Grid, 4, F, 6, "FIX", NULL)
#' str(selec6best)
#' }
#' @author Sebastian Gatscha
selection1         <- function(fit, Grid,teil,elitism,nelit,selstate, verbose) {
  # fit = fit; Grid = Grid; teil = teil; elitism = elitism; nelit = nelit; 
  # selstate = selstate; verbose = verbose
  
  
  if (missing(verbose)){verbose = FALSE}
  
  # Make a DataFrame of the Fitness Function Output. Representing all x Parks with their fitness value.
  new <- do.call("rbind", fit)
  
  # Get the unique Fitness value according to the RunID
  new1 <- subset.matrix(new, subset = !duplicated(new[,'Run']), 
                        select = c("Run", "Parkfitness", "EnergyOverall"))
  
  ## arrange descending, to dismiss last 2
  new1 <- new1[order(new1[,'Parkfitness'], decreasing = TRUE),]
  row.names(new1) <- NULL
  # Elitarism - A certain amount of individuals will get their fitness values increased
  if (elitism){
    if (verbose){
      cat(paste("Elitarism activated. Best", nelit, "individuals are increased\n"))
    }
    if (nrow(new1) < nelit) {
      nelit = nrow(new1)
    }
    ## TODO - This or create vector of same length with nelit times 10 and otherwise 1.
    new1[1:nelit,'Parkfitness'] <- new1[1:nelit,'Parkfitness'] * 10
  }
  
  if (nrow(new1) > 10) {
    # Delete the 4 worst fitness entries.
    new1 <- new1[-seq(length(new1[,1]), nrow(new1) -3 , -1),]
  }
  
  selstate <- toupper(selstate)
  ## The next two methods determine how a selection percentage is calculated
  # Either a fixed percentage of 50% is used
  if (selstate == "FIX") {
    # Select a fixed amount of indivs. # Teil=2 takes always 50% of population
    if (teil == 1){
      teil = 1
    } else {
      teil = 2
    }
    nPar <- ceiling(nrow(new1) / teil)
    if (verbose){
      cat(paste("Selection Percentage:", round(100/teil,3), "\n"))
      cat(paste("FIX: How many parental individuals are selected:",nPar, "from",  
                nrow(new1),"with", ((1/teil)*100), "%\n"))
    }
    
  }
  # Or the selection percentage is variable, depending on the development of the fitness values.
  if (selstate == "VAR") {
    # Select a variable amount of indivs. Teil comes from the "fuzzy logic" modell
    nPar <- ceiling(nrow(new1) / teil);
    if(verbose){
      cat(paste("VAR: How many parental individuals are selected:",nPar, "from",
                nrow(new1),"with", ((1/teil)*100), "%\n"))
    }
    
  }
  # Upper Limit of selected individuals is 100.
  if (nPar > 100) {nPar <- 100}
  if (any(is.na(new1[,'Parkfitness']))) {
    cat("some Values are NA....")
    # browser()
  }
  childsRunID <- sort(sample(new1[,1], nPar, prob = new1[,'Parkfitness'], replace = FALSE));
  
  # Pick the parks with those list indeces. (park with all config) and return Run and Rect_ID
  chile <- seq_len(length(childsRunID)) 
  child <- lapply(chile, function(z) {
    subset.matrix(fit[[childsRunID[z]]], select = c("Run", "Rect_ID", "Parkfitness"))
  })
  
  ## Create binary code for every parkconfiguration (individual) respective to the whole Grid. (Turbine yes = 1, Turbine no = 0)
  childbin <- lapply(chile, function(i) {
    ## For every Child element, assign the total Grid to a binaryvariable[i], and set all binary =0. Assign Run Value as well
    tmp <- Grid
    tmp <- cbind(tmp, 
                 "Run" = child[[i]][1, 'Run'],
                 "bin" = 0,
                 "Fitness" = child[[i]][1, 'Parkfitness'])
    # tmp[,'Run'] <- child[[i]][1, 'Run']
    # tmp$bin <- 0
    # tmp$Fitness <- child[[i]]$Parkfitness[[1]]
    for (e in 1:length(child[[i]][,1])) {
      ## For every element in a child (turbines) get his Rect_ID and set binaryvariable[i] to 1, where GridId = Rect_ID
      rectid <- child[[i]][e, 2]
      tmp[tmp[,'ID'] == rectid, 'bin'] <- 1
    }
    tmp
  })
  
  # Create the parents
  # parall <- unlist(lapply(1:(length(childsRunID)/2), function(i) {
  #   new <- new1[new1$Run %in% childsRunID,]
  #   set.seed(104)
  #   pare <- sample(x = sort(childsRunID), 2, replace = FALSE);
  #   childsRunID <<- childsRunID[!(childsRunID %in% pare)];
  #   pare
  # }))
  parents <- vector("list", length(childsRunID) / 2)
  for (i in 1:(length(childsRunID) / 2) ) {
    ## TODO - do I need next line in for loop??
    new <- new1[new1[,'Run'] %in% childsRunID,]
    parents[[i]] <- sample(x = sort(childsRunID), 2, replace = FALSE)
    childsRunID <-  childsRunID[!(childsRunID %in% parents[[i]])]
  }
  parall <- unlist(parents)
  
  # Create the children
  childbindf <- do.call("rbind", childbin)
  paralli <- lapply(1:length(parall), function(i) {
    subset.matrix(childbindf[which(childbindf[,'Run'] %in% parall[i]),],
                  select = c("ID","Run","bin","Fitness"))
  })
  
  parentsall <- data.frame(paralli)
  lePar = length(parentsall)
  parents_Fitness <- parentsall[1, c(1, seq(4, lePar, 4))]
  parentsall <- parentsall[, c(1, seq(3, lePar, 4))];
  return(list(parentsall, parents_Fitness))
}

