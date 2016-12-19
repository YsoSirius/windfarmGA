#' @title Check Input Selection Method
#' @name readintegerSel
#' @description  Checks whether the input for \code{\link{selection1}} is
#' given correctly. If not, a message is prompted which asks to input one
#' of the 2 available selection methods. The available inputs are "F" and "V".
#' "EF refers to a fixed percentage of 50% and "V" refers to a variable
#' percentage, based on the development of the populations fitness values.
#' partitioning.
#'
#' @export
#'
#' @return Returns the selected selection method (character)
#'
#' @author Sebastian Gatscha
readintegerSel        <- function(){
  cat("\nSelect appropriate Selection Method. Either 'FIX' or 'VAR' are available.")
  selstaPr <- readline(prompt = "Type 'F' for a fixed percentage of 50% and 'V' for a variable percentage.")

  selstaPr <- toupper(selstaPr)
  if (selstaPr=="F") {selstate = "FIX"}
  if (selstaPr=="V") {selstate = "VAR"}

  if  (selstaPr!="F" & selstaPr!="V") {
    selstate <- readintegerSel()
  }
  return(selstate)
}

