#' @title Crossover Method
#' @name crossover
#' @description The crossover method creates new offspring with the selected
#'   individuals by permutating their genetic codes.
#'
#' @export
#'
#' @param se6 The selected individuals. The output of \code{\link{selection}}
#' @param u The crossover point rate
#' @param uplimit The upper limit of allowed permutations
#' @param crossPart The crossover method. Either "EQU" or "RAN"
#' @param verbose If \code{TRUE}, will print out further information
#' @param seed Set a seed for comparability. Default is \code{NULL}
#'
#' @family Genetic Algorithm Functions
#' @return Returns a binary coded matrix of all permutations and all grid cells,
#'   where 0 indicates no turbine and 1 indicates a turbine in the grid cell.
#'
#' @examples
#' ## Create two random parents with an index and random binary values
#' Parents <- data.frame(
#'   ID = 1:20,
#'   bin = sample(c(0, 1), 20, replace = TRUE, prob = c(70, 30)),
#'   bin.1 = sample(c(0, 1), 20, replace = TRUE, prob = c(30, 70))
#' )
#'
#' ## Create random Fitness values for both individuals
#' FitParents <- data.frame(ID = 1, Fitness = 1000, Fitness.1 = 20)
#'
#' ## Assign both values to a list
#' CrossSampl <- list(Parents, FitParents)
#' ## Cross their data at equal locations with 2 crossover parts
#' crossover(CrossSampl, u = 1.1, uplimit = 300, crossPart = "EQU")
#'
#' ## with 3 crossover parts and equal locations
#' crossover(CrossSampl, u = 2.5, uplimit = 300, crossPart = "EQU")
#'
#' ## or with random locations and 5 crossover parts
#' crossover(CrossSampl, u = 4.9, uplimit = 300, crossPart = "RAN")
#'
crossover <- function(se6, u, uplimit, crossPart = c("EQU", "RAN"), verbose, seed) {
  crossPart <- match.arg(crossPart)
  if (missing(verbose)) {
    verbose <- FALSE
  }
  if (missing(seed)) {
    seed <- NULL
  }
  if (verbose) {
    message(paste("crossover point rate: ", u + 1))
  }

  se6fit <- se6[[2]][1, -1]
  se6 <- se6[[1]]
  se6 <- se6[, -1]
  parid <- sample(1:length(se6))
  z <- seq.int(1, length(parid), 2)

  all <- vector("list", length(z))

  crossPart <- toupper(crossPart)

  sene2fit <- vector(mode = "list", length = length(z))
  for (e in 1:length(z)) {
    r <- z[[e]]
    # Sene ist der genCode des 1ten Elternteils, Sene1 der des 2ten Elternteils
    sene <- se6[, parid[r]]
    sene1 <- se6[, parid[r + 1]]

    senefit <- se6fit[, parid[r]]
    sene1fit <- se6fit[, parid[r + 1]]
    sene2fit[[e]] <- senefit + sene1fit / 2

    if (crossPart == "EQU") {
      ## Equal Parts
      # In how many parts should the genCode be split?
      crosEquPartN <- base::trunc(u + 1)
      t1 <- ceiling(length(sene) / crosEquPartN)
      # split the genCode in equal parts, that are t long.
      # a is from parent1 and b for parent2.
      a <- base::split(sene, as.numeric(gl(length(sene), t1, length(sene))))
      b <- base::split(sene1, as.numeric(gl(length(sene1), t1, length(sene1))))
    }
    if (crossPart == "RAN") {
      ## Random Parts
      # Split the genCode in u parts, that are randomly distributed
      if (!is.null(seed)) {
        set.seed(as.integer(seed))
      }
      u1 <- sort(sample(2:(length(sene) - 1), u, replace = FALSE))
      a <- splitAt(sene, u1)
      b <- splitAt(sene1, u1)
    }

    x1 <- rbind(a, b)
    perm <- permutations(n = 2, r = ncol(x1), v = 1:nrow(x1))
    # perm <- gtools::permutations(n = 2, r = ncol(x1), v = 1:nrow(x1),
    # repeats.allowed = TRUE)

    # for every possible permutation
    permut <- list()
    for (pp in 1:nrow(perm)) {
      # for every col/genetic code pieces take either from
      # parent 1(a) or parent 2(b)
      gclist <- list()
      for (gnp in 1:length(perm[pp, ])) {
        parent01 <- perm[pp, gnp]
        if (parent01 == 1) {
          gc <- a[[gnp]]
        } else {
          gc <- b[[gnp]]
        }
        gclist[[gnp]] <- gc
      }
      permut[[pp]] <- unlist(gclist)
    }
    permut <- do.call("cbind", permut)
    all[[e]] <- permut
  }

  nuCh <- ncol(all[[1]])
  sene2fit_n <- do.call("cbind", sene2fit)
  sene2fit_n <- (sene2fit_n / mean(sene2fit_n))

  fitChi <- rep(x = sene2fit_n, each = nuCh)

  nI <- do.call("cbind", all)

  if (verbose) {
    message(paste("How many parental pairs are at hand: ", length(z)))
    message(paste("How many permutations are possible: ", length(z) *
      (2^(trunc(u) + 1))))
  }

  partaksur <- ncol(nI)
  if (partaksur >= uplimit) {
    partaksur <- uplimit
    if (verbose) {
      message(paste("Population max limit reached: ", uplimit))
    }
  }

  # Select only some of the available permutations.
  # Take fitness value as prop value.
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  partak <- sort(sample(1:length(nI[1, ]), partaksur, prob = fitChi))
  if (verbose) {
    message(paste("How many permutations are selected: ", length(partak)))
  }

  nI <- nI[, partak]
  return(nI)
}

#' @title Split matrices or numeric vectors at specific indices
#' @name splitAt
#' @description The function is used by the crossover method to
#' split a genetic code at certain intervals. See also \code{\link{crossover}}.
#' @param x A numeric variable that represents an individual's
#'   binary genetic code
#' @param pos A numeric value that indicates where to split the genetic code
#'
#' @family Helper Functions
#' @return Returns a list of the split genetic code.
#' @export
#'
#' @examples
#' splitAt(1:100, 20)
#' splitAt(as.matrix(1:100), 20)
#'
splitAt <- function(x, pos) {
  unname(split(x, cumsum(seq_along(x) %in% pos)))
}

#' @title Enumerate the Combinations or Permutations of the Elements of a
#' Vector
#' @name permutations
#' @description permutations enumerates the possible permutations. The
#' function is forked and minified from gtools::permutations
#'
#' @param n Size of the source vector
#' @param r Size of the target vectors
#' @param v Source vector. Defaults to 1:n
#'
#' @family Helper Functions
#' @return Returns a matrix where each row contains a vector of length r.
#'
#' @author Original versions by Bill Venables. Extended to handle repeats.allowed
#' by Gregory R. Warnes
#'
#' @references Venables, Bill. "Programmers Note", R-News, Vol 1/1, Jan. 2001.
#' \url{https://cran.r-project.org/doc/Rnews/}
#'
permutations <- function(n, r, v = 1:n) {
  v <- unique(sort(v))
  sub <- function(n, r, v) {
    if (r == 1) {
      matrix(v, n, 1)
    } else {
      inner <- Recall(n, r - 1, v)
      cbind(
        rep(v, rep(nrow(inner), n)),
        matrix(t(inner),
          ncol = ncol(inner), nrow = nrow(inner) * n,
          byrow = TRUE
        )
      )
    }
  }
  sub(n, r, v[1:n])
}
