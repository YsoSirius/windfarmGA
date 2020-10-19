# nocov start
#' @title Check Input Crossover Method
#' @name readinteger
#' @description  Checks whether the input for \code{\link{crossover}} is given
#'   correctly. If not, a message is prompted which asks to input one of the 2
#'   available crossover methods. The available inputs are "E" and "R". "E"
#'   refers to partitioning at equal intervals and "R" refers to random
#'   partitioning.
#'
#' @family Helper Functions
#' @return Returns the selected crossover method (character)
#' @examples \dontrun{
#'   readinteger()
#' }
#'
readinteger        <- function(){
  cat("\nSelect appropriate Method. Either 'EQU' for equal crossover parts or 'RAN' for random parts.\n")
  # crPaInter <- readline(prompt = "Type 'R' for random and 'E' for equal parts.")
  cat("Type 'R' for random and 'E' for equal parts.\n")
  crPaInter <- readLines(n = 1, con = getOption("windfarmGA.connection"))
  
  crPaInter <- toupper(crPaInter)

  if (crPaInter == "R") {crossPart <- "RAN"}
  if (crPaInter == "E") {crossPart <- "EQU"}

  if  (crPaInter != "E" & crPaInter != "R") {
    crossPart <- readinteger()
  }

  return(crossPart)
}

#' @title Check Input Selection Method
#' @name readintegerSel
#' @description  Checks whether the input for \code{\link{selection}} is given
#'   correctly. If not, a message is prompted which asks to input one of the 2
#'   available selection methods. The available inputs are "F" and "V". "F"
#'   refers to a fixed percentage of 50% and "V" refers to a variable
#'   percentage, based on the development of the population fitness values.
#'
#' @family Helper Functions
#' @return Returns the selected selection method (character)
#' @examples \dontrun{
#'   readintegerSel()
#' }
readintegerSel        <- function(){
  cat("\nSelect appropriate Selection Method. Either 'FIX' or 'VAR' are available.\n")
  # selstaPr <- readline(prompt = "Type 'F' for a fixed percentage of 50% and 'V' for a variable percentage.")
  
  cat("Type 'F' for a fixed percentage of 50% and 'V' for a variable percentage.\n")
  selstaPr <- readLines(n = 1, con = getOption("windfarmGA.connection"))
  
  selstaPr <- toupper(selstaPr)
  if (selstaPr == "F") {selstate <- "FIX"}
  if (selstaPr == "V") {selstate <- "VAR"}
  
  if  (selstaPr != "F" & selstaPr != "V") {
    selstate <- readintegerSel()
  }
  return(selstate)
}
# nocov end


