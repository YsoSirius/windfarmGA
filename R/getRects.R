#' @title Get the Grid-IDs from binary matrix
#' @name getRects
#' @description  Get the grid IDs from the trimmed binary matrix, where the
#' binary code indicates which grid cells are used in the current
#' wind farm constellation.
#'
#' @export
#'
#' @param trimtonOut Input matrix with binary values. (matrix)
#' @param Grid Grid of the considered area (data.frame)
#'
#' @return Returns a list of all individuals with X and Y coordinates
#' and the grid cell ID. (list)
#'
#' @author Sebastian Gatscha
getRects          <- function(trimtonOut, Grid){
  #trimtonOut = mut1
  childli = list();
  len1= dim(trimtonOut)[2]

  for (i in 1:len1) {
    childli[[i]] <- trimtonOut[,i]
  }
  #childall <- do.call("cbind",childli)

  rectidli = list();
  for (u in 1:len1){
    rectidli[[u]] <- which(childli[[u]]==1, arr.ind = T)
  }

  childnew = list()
  for (z in 1:len1) {
    childnew[[z]] <- Grid[rectidli[[z]],];
  }

  return(childnew)
}
