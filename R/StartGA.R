#' @title Create a random initial Population
#' @name StartGA
#' @description  Create \code{nStart} random subselections from the
#' indexed grid and assign binary variable 1 to selected grids.
#' This function initiates the genetic algorithm with a first random
#' population, and will only be needed in the firt iteration.
#' @export
#' @param Grid The data.frame output of "GridFilter" function,
#' with X and Y coordinates and Grid cell IDs. (data.frame)
#' @param n A numeric value indicating the amount of required turbines.
#' (numeric)
#' @param nStart A numeric indicating the amount of randomly generated
#' initial individuals. Default is 100. (numeric)
#'
#' @return Returns a list of \code{nStart} initial individuals,
#' each consisting of \code{n} turbines.
#' Resulting list has the x and y coordinates, the grid cell ID
#' and a binary variable of 1, indicating a turbine in the grid cell.
#' (list)
#'
#' @author Sebastian Gatscha
StartGA           <- function(Grid, n, nStart=100) {
  if (length(Grid$ID) <= n) {
    print("################### GA ERROR MESSAGE ###################")
    print(paste("##### Amount Grid-cells: ", length(Grid$ID),"\n##### Amount of turbines: ", n))
    stop("\n The amount of Grid-cells is smaller or equal the number of turbines requested.\n
         Decrease Resolution (fcrR), number of turbines (n), or Rotorradius (Rotor).")
    cat("Press [enter] to continue")
    readline()
  }
  if (length(Grid$ID) < (2*n)) {
    print("################### GA ERROR MESSAGE ###################")
    print(paste("##### Amount Grid-cells: ", length(Grid$ID),"\n##### Amount of turbines: ", n))
    stop("\n The amount of Grid-cells should at least be double the size of turbines requested.\n
          Decrease Resolution (fcrR), number of turbines (n), or Rotorradius (Rotor).")
    print("Press [enter] to continue")
    readline()
  }
  subsetSel = list(); ids=list();
  ## How many parks for the initial population (10,20, amount of turbines?)
  for (i in 1:nStart){
    ## Assign Binary Variable 0 to all individuals
    Grid$bin <- 0
    ## Randomly assign n values of 1 in the dataframe
    ids[i][[1]] <- sort(sample(x = Grid$ID, size = n, replace = F))
    Grid[Grid$ID %in% ids[i][[1]], ]$bin = 1
    subsetSel[i][[1]] <- Grid[Grid$bin == 1,]
  }
  return(subsetSel)
}
