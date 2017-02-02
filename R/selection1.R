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
#' @importFrom dplyr select arrange
#'
#' @param fit A list of all fitness-evaluated individuals (list)
#' @param Grid Is the indexed grid output from \code{\link{GridFilter}}:
#' (data.frame)
#' @param teil A numeric value, that determines the selection percentage
#' (numeric)
#' @param elitism Boolean Value, which indicates whether elitism should be
#' included or not. (logical)
#' @param nelit If \code{elitism}: is TRUE, then this input variable
#' determines the amount of individuals in the elite group. (numeric)
#' @param selstate Determines which selection method is used, "FIX" selects
#' a constant percentage and "VAR" selects a variable percentage, depending
#' on the development of the fitness values. (character)
#'
#' @return Returns list with 2 elements. Element 1 is the binary encoded
#' matrix, which shows all selected individuals. Element 2 represent the mean
#' fitness values of each parental team. (list)
#'
#' @author Sebastian Gatscha

selection1         <- function(fit, Grid,teil,elitism,nelit,selstate){
  # library(dplyr);
  # Make a DataFrame of the Fitness Function Output. Representing all x Parks with their fitness value.
  new <- do.call("rbind", fit)
  # Get the unique Fitness value according to the RunID
  new1 <- dplyr::select(split(new, duplicated(new$Run))$'FALSE', Run, Parkfitness,EnergyOverall)
  ## arrange descending, to dismiss last 2
  new1 <- dplyr::arrange(new1,dplyr::desc(Parkfitness))
  # Elitarism - A certain amount of individuals will get their fitness values increased
  if (elitism == TRUE){
    cat(paste("Elitarism activated. Best", nelit, "individuals are increased\n"))
    new1[1:nelit,]$Parkfitness <- new1[1:nelit,]$Parkfitness*10
  }
  # Delete the 4 worst fitness entries.
  selv <- nrow(new1)-3;   new1 <- new1[-seq(length(new1[[1]]),selv,-1),];
  selstate <- toupper(selstate)
  ## The next two methods determine how a selection percentage is calculated
  # Either a fixed percentage of 50% is used
  if (selstate == "FIX") {
    # Select a fixed amount of indivs. # Teil=2 takes always 50% of population
    if (teil==1){teil=1}else{teil=2}
    cat(paste("Selection Percentage:", round(100/teil,3), "\n"))
    nPar <- ceiling(nrow(new1)/teil);
    cat(paste("FIX: How many parental individuals are selected:",nPar, "from",  nrow(new1),"with", ((1/teil)*100), "%\n"))
  }
  # Or the selection percentage is variable, depending on the deveopment of the fitness values.
  if (selstate == "VAR") {
    # Select a variable amount of indivs. Teil comes from the "fuzzy logic" modell
    nPar <- ceiling(nrow(new1)/teil);
    cat(paste("VAR: How many parental individuals are selected:",nPar, "from",  nrow(new1),"with", ((1/teil)*100), "%\n"))
  }
  # Upper Limit of selected individuals is 100.
  if (nPar > 100){
    nPar = 100
  }
  childsRunID <- sort(sample(new1[[1]], nPar, prob= new1$Parkfitness, replace=F));

  # Pick the parks with those list indeces. (park with all config) and return Run and Rect_ID
  chile = length(childsRunID); child = vector("list",chile);
  for (z in 1:chile){
    child[[z]] <- dplyr::select(fit[[childsRunID[z]]], Run, Rect_ID,Parkfitness)
  }
  ## Create binary code for every parkconfiguration (individual) respective to the whole Grid. (Turbine yes = 1, Turbine no = 0)
  childbin <- vector("list",chile);
  for (i in 1: chile){
    ## For every Child element, assign the total Grid to a binaryvariable[i], and set all binary =0. Assign Run Value as well
    childbin[[i]] <- Grid
    childbin[[i]]$Run <- child[[i]]$Run[[1]]
    childbin[[i]]$bin <- 0
    childbin[[i]]$Fitness <- child[[i]]$Parkfitness[[1]]
    for (e in 1:length(child[[i]][[1]])) {
      ## For every element in a child (turbines) get his Rect_ID and set binaryvariable[i] to 1, where GridId = Rect_ID
      rectid <- child[[i]][e,2][[1]]
      childbin[[i]][childbin[[i]]$ID==rectid,]$bin = 1
    }
  }
  # Create the partens
  parents <- vector("list",(length(childsRunID)/2));
  for (i in 1:(length(childsRunID)/2)) {
    new <- dplyr::arrange(new1[new1$Run %in% childsRunID,]);
    parents[[i]] <- sample(x = sort(childsRunID), 2, replace = F);
    childsRunID <-  childsRunID[!(childsRunID %in% parents[[i]])];
  }
  parall <- unlist(parents)
  # Create the children
  childbindf <- do.call("rbind",childbin); paralli <- vector("list",length(parall));
  for (i in 1:length(parall)){
    paralli[[i]] <- dplyr::select(childbindf[which(childbindf$Run %in% parall[i]),], ID,Run,bin,Fitness)
  }
  parentsall <- data.frame(paralli)
  parents_Fitness <- parentsall[1,c(1,seq(4, length(parentsall),4))]
  parentsall <- parentsall[,c(1,seq(3, length(parentsall),4))];
  parents_new <- list(parentsall,parents_Fitness)
  return(parents_new)
}


