#' @title Mutation Method
#' @name mutation
#' @description Legacy bit-flip mutation on 0/1 chromosomes. The GA loop uses
#'   \code{\link{swap_mutation}} so that every individual keeps exactly \code{n}
#'   turbines.
#'
#' @export
#'
#' @param a The binary matrix of all individuals
#' @param p The mutation rate
#' @param seed Set a seed for comparability. Default is \code{NULL}
#'
#' @family Genetic Algorithm Functions
#' @return Returns a binary matrix with mutated genes.
#'
#' @examples
#' ## Create 4 random individuals with binary values
#' a <- cbind(
#'   bin0 = sample(c(0, 1), 20, replace = TRUE, prob = c(70, 30)),
#'   bin1 = sample(c(0, 1), 20, replace = TRUE, prob = c(30, 70)),
#'   bin2 = sample(c(0, 1), 20, replace = TRUE, prob = c(30, 70)),
#'   bin3 = sample(c(0, 1), 20, replace = TRUE, prob = c(30, 70))
#' )
#' a
#'
#' ## Mutate the individuals with a low percentage
#' aMut <- mutation(a, 0.1, NULL)
#' ## Check which values are not like the originals
#' a == aMut
#'
#' ## Mutate the individuals with a high percentage
#' aMut <- mutation(a, 0.4, NULL)
#' ## Check which values are not like the originals
#' a == aMut
#'
mutation <- function(a, p, seed = NULL) {
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

#' @title Swap mutation of turbine layouts
#' @name swap_mutation
#' @description Replace occupied grid cells with unused ones. The number of
#'   swaps is `max(min_swaps, Binomial(n, p))`, so each individual explores at
#'   least `min_swaps` new cells (default 1). The number of turbines stays `n`.
#'
#' @export
#'
#' @param ids Integer matrix with `n` rows (turbines) and one column per
#'   individual
#' @param grid_ids All valid grid cell IDs
#' @param p Mutation probability per turbine
#' @param seed Set a seed for comparability. Default is `NULL`
#' @param min_swaps Minimum number of swaps per individual. Default is
#'   `getOption("windfarmGA.min_swaps")` (1)
#'
#' @param visit Named visit counts per grid ID. Free cells with fewer visits
#'   are more likely to be chosen. Default is `NULL` (uniform)
#'
#' @family Genetic Algorithm Functions
#' @return Integer matrix of unique grid IDs, same dimension as `ids`
#'
#' @examples
#' ids <- cbind(c(1, 3, 5, 7), c(2, 4, 6, 8))
#' swap_mutation(ids, grid_ids = 1:20, p = 0.5, seed = 1)
swap_mutation <- function(ids, grid_ids, p, seed = NULL, min_swaps = NULL,
                          visit = NULL) {
  if (!is.null(seed) && !missing(seed)) {
    set.seed(as.integer(seed))
  }
  if (is.null(min_swaps)) {
    min_swaps <- getOption("windfarmGA.min_swaps", 1L)
  }
  min_swaps <- as.integer(min_swaps)
  if (!is.matrix(ids)) {
    ids <- matrix(ids, ncol = 1)
  }
  grid_ids <- as.integer(grid_ids)
  n <- nrow(ids)
  n_pop <- ncol(ids)
  out <- ids
  for (j in seq_len(n_pop)) {
    occupied <- as.integer(out[, j])
    free <- setdiff(grid_ids, occupied)
    if (!length(free)) {
      next
    }
    n_mut <- stats::rbinom(1L, n, min(1, max(0, p)))
    n_mut <- max(n_mut, min_swaps)
    n_mut <- min(n_mut, n, length(free))
    if (n_mut < 1) {
      next
    }
    idx <- sample.int(n, n_mut)
    for (i in idx) {
      new_id <- sample_weighted_ids(free, 1L, visit)
      free <- c(free[free != new_id], occupied[i])
      occupied[i] <- new_id
    }
    out[, j] <- occupied
  }
  out
}
