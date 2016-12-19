#' @title Check Input Crossover Method
#' @name readinteger
#' @description  Checks whether the input for \code{\link{crossover1}} is
#' given correctly. If not, a message is prompted which asks to input one
#' of the 2 available crossover methods. The available inputs are "E" and "R".
#' "E" refers to partitioning at equal intervals and "R" refers to random
#' partitioning.
#' @export
#' @return Returns the selected crossover method (character)
#'
#' @author Sebastian Gatscha
readinteger        <- function(){
  cat("\nSelect appropriate Method. Either 'EQU' for equal crossover parts or 'RAN' for random parts.")
  crPaInter <- readline(prompt = "Type 'R' for random and 'E' for equal parts.")

  crPaInter <- toupper(crPaInter)

  if(crPaInter=="R"){crossPart = "RAN"}
  if(crPaInter=="E"){crossPart = "EQU"}

  if  (crPaInter!= "E" & crPaInter !="R") {
    crossPart <- readinteger()
  }

  return(crossPart)
}


