#' @title Mutation Method
#' @name mutation
#' @description  Mutate the genes of every chromosome or individual with low
#'   probability.
#'
#' @export
#'
#' @param a The binary matrix of all individuals.
#' @param p The mutation rate.
#' @param seed Set a seed for comparability. Default is NULL
#'
#' @family Genetic Algorithm Functions
#' @return Returns a binary matrix with mutated genes.
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
#' aMut <- mutation(a,0.1, NULL)
#' ## Check which values are not like the originals
#' a==aMut
#'
#' ## Mutate the individuals with a high percentage
#' aMut <- mutation(a,0.4, NULL)
#' ## Check which values are not like the originals
#' a==aMut
#'
mutation          <- function(a, p, seed=NULL) {
  if (!is.null(seed) && !missing(seed)) {
    set.seed(as.integer(seed))
  }
  ## Probability for every gene to mutate
  rnd <- runif(n = length(a), min = 0, max = 1) 
  ## for every element in the 2 children, get a random number between 1 & 0
  ## If the random number is smaller than p, the gene is switched
  ## from 1 to 0 | from 0 to 1
  whichs <- which(rnd < p)
  a[whichs] <- ifelse(a[whichs] == 1, 0, 1)
  return(a)
}