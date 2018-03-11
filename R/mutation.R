#' @title Mutation Method
#' @name mutation
#' @description  Mutate the genes of every chromosome or individual with
#' low probability.
#'
#' @export
#'
#' @importFrom stats runif
#'
#' @param a The binary matrix of all individuals. (matrix)
#' @param p The mutation rate. (numeric)
#'
#' @return Returns a binary matrix with mutated genes. (matrix)
#'
#' @examples
#' ## Create 4 random individuals with binary values
#' a <- cbind(bin=sample(c(0,1),20,replace=TRUE,prob = c(70,30)),
#'         bin.1=sample(c(0,1),20,replace=TRUE,prob = c(30,70)),
#'         bin.2=sample(c(0,1),20,replace=TRUE,prob = c(30,70)),
#'         bin.3=sample(c(0,1),20,replace=TRUE,prob = c(30,70)))
#' a
#'
#' ## Mutate the individuals with a low percentage
#' aMut <- mutation(a,0.1)
#' ## Check which values are not like the originals
#' a==aMut
#'
#' ## Mutate the individuals with a high percentage
#' aMut <- mutation(a,0.4)
#' ## Check which values are not like the originals
#' a==aMut
#'
#' @author Sebastian Gatscha
mutation          <- function(a,p) {
  ## Probability for every gene to mutate
  for (i in 1:length(a)) {
    ## for every element in the 2 children, get a random number between 1 & 0
    rnd <- runif(n = 1,min = 0,max = 1)
    ## If the random number is smaller than p, the gene is switched
    ## (from 1 to 0 | from 0 to 1)
    if (rnd < p) {
      if (a[i] == "0") {a[i] <- 1} else {a[i] <- 0}
    }
  }
  return(a)
}