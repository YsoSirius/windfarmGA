#' @title Crossover Method
#' @name crossover
#' @description Legacy binary crossover. The GA loop uses
#'   \code{\link{set_crossover}} on unique grid-cell IDs instead. This function
#'   still permutes 0/1 chromosomes (EQU/RAN) and typically needs
#'   \code{\link{trimton}} afterwards.
#'
#' @export
#'
#' @param se6 Legacy binary selection: a list with a grid-ID column plus 0/1
#'   layout columns, and a fitness row. Current \code{\link{selection}}
#'   returns ID matrices; use \code{\link{set_crossover}} in the GA loop.
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
    sene2fit[[e]] <- (senefit + sene1fit) / 2

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

#' @title Set crossover of turbine layouts
#' @name set_crossover
#' @description Combine two layouts of `n` unique grid-cell IDs. Shared sites
#'   are kept; remaining sites are sampled from the parents' exclusive cells
#'   and, with rate `p_inject`, from grid cells that neither parent uses.
#'   Identical parents still get unused cells injected so the search does not
#'   freeze. Every child has exactly `n` turbines.
#'
#' @export
#'
#' @param ids Integer matrix with `n` rows (turbines) and one column per parent
#' @param grid_ids All valid grid cell IDs
#' @param uplimit Maximum number of children. Default is 300
#' @param seed Set a seed for comparability. Default is `NULL`
#' @param verbose If `TRUE`, print the number of children
#' @param p_inject Fraction of non-shared slots filled from unused grid cells.
#'   Default is `getOption("windfarmGA.crossover_inject")` (0.25). At least one
#'   unused cell is injected when any are available.
#' @param grid_xy Optional matrix/data.frame with columns `ID`, `X`, `Y`. If
#'   given, a spatial half-plane crossover is used with probability
#'   `p_spatial`.
#' @param visit Named visit counts per grid ID (undersampled cells preferred)
#' @param p_spatial Probability of spatial (vs set) crossover when `grid_xy`
#'   is given. Default is `getOption("windfarmGA.spatial_crossover")` (0.5)
#'
#' @family Genetic Algorithm Functions
#' @return Integer matrix of unique grid IDs (`n` x children)
#'
#' @examples
#' ids <- cbind(c(1, 3, 5, 7), c(1, 4, 5, 9))
#' set_crossover(ids, grid_ids = 1:20, uplimit = 4, seed = 1)
set_crossover <- function(ids, grid_ids, uplimit = 300, seed = NULL,
                          verbose = FALSE, p_inject = NULL,
                          grid_xy = NULL, visit = NULL, p_spatial = NULL) {
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }
  if (is.null(p_inject)) {
    p_inject <- getOption("windfarmGA.crossover_inject", 0.25)
  }
  if (is.null(p_spatial)) {
    p_spatial <- getOption("windfarmGA.spatial_crossover", 0.5)
  }
  if (!is.matrix(ids)) {
    ids <- matrix(ids, ncol = 1)
  }
  n_par <- ncol(ids)
  if (n_par < 2) {
    return(ids)
  }
  if (n_par %% 2 == 1) {
    ids <- ids[, -n_par, drop = FALSE]
    n_par <- ncol(ids)
  }
  idx <- sample(seq_len(n_par))
  npairs <- n_par / 2
  children <- vector("list", uplimit)
  k <- 0
  pair_i <- 1
  use_xy <- !is.null(grid_xy)
  while (k < uplimit) {
    a <- idx[2 * pair_i - 1]
    b <- idx[2 * pair_i]
    k <- k + 1
    children[[k]] <- set_cross_pick(
      ids[, a], ids[, b], grid_ids, p_inject, grid_xy, visit, p_spatial, use_xy
    )
    if (k >= uplimit) {
      break
    }
    k <- k + 1
    children[[k]] <- set_cross_pick(
      ids[, b], ids[, a], grid_ids, p_inject, grid_xy, visit, p_spatial, use_xy
    )
    pair_i <- pair_i %% npairs + 1
  }
  out <- do.call(cbind, children)
  if (verbose) {
    message("Set-crossover children: ", ncol(out))
  }
  out
}

set_cross_pick <- function(a, b, grid_ids, p_inject, grid_xy, visit,
                           p_spatial, use_xy) {
  if (use_xy && stats::runif(1) < p_spatial) {
    spatial_cross_one(a, b, grid_xy, grid_ids, p_inject, visit)
  } else {
    set_cross_one(a, b, grid_ids, p_inject, visit)
  }
}

set_cross_one <- function(a, b, grid_ids, p_inject = 0.25, visit = NULL) {
  n <- length(a)
  a <- unique(as.integer(a))
  b <- unique(as.integer(b))
  grid_ids <- as.integer(grid_ids)
  common <- intersect(a, b)
  unused <- setdiff(grid_ids, union(a, b))
  exclusive <- setdiff(union(a, b), common)
  keep <- common
  if (length(keep) > n) {
    keep <- keep[seq_len(n)]
  }
  need <- n - length(keep)

  if (need == 0) {
    n_inj <- max(1L, as.integer(round(n * p_inject)))
    n_inj <- min(n_inj, length(unused), n)
    if (n_inj > 0 && p_inject > 0) {
      drop <- sample_weighted_ids(keep, n_inj, visit)
      add <- sample_weighted_ids(unused, n_inj, visit)
      keep <- c(setdiff(keep, drop), add)
    }
    return(sort(keep))
  }

  n_inj <- as.integer(round(need * p_inject))
  if (p_inject > 0 && length(unused) > 0) {
    n_inj <- max(n_inj, 1L)
  }
  n_inj <- min(n_inj, length(unused), need)
  n_ex <- need - n_inj
  if (n_ex > length(exclusive)) {
    n_ex <- length(exclusive)
    n_inj <- min(need - n_ex, length(unused))
  }
  extra <- integer(0)
  if (n_ex > 0) {
    extra <- c(extra, sample_weighted_ids(exclusive, n_ex, visit))
  }
  if (n_inj > 0) {
    extra <- c(extra, sample_weighted_ids(unused, n_inj, visit))
  }
  still <- n - length(keep) - length(extra)
  if (still > 0) {
    leftover <- setdiff(grid_ids, c(keep, extra))
    if (length(leftover) > 0) {
      extra <- c(extra, sample_weighted_ids(leftover, still, visit))
    }
  }
  sort(c(keep, extra))
}

spatial_cross_one <- function(a, b, grid_xy, grid_ids, p_inject, visit) {
  n <- length(a)
  a <- unique(as.integer(a))
  b <- unique(as.integer(b))
  grid_ids <- as.integer(grid_ids)
  xs <- as.numeric(grid_xy[, "X"])
  ys <- as.numeric(grid_xy[, "Y"])
  names(xs) <- names(ys) <- as.character(as.integer(grid_xy[, "ID"]))
  if (stats::runif(1) < 0.5) {
    cut <- stats::runif(1, min(xs), max(xs))
    in_reg <- function(ids) xs[as.character(ids)] >= cut
  } else {
    cut <- stats::runif(1, min(ys), max(ys))
    in_reg <- function(ids) ys[as.character(ids)] >= cut
  }
  child <- unique(c(a[in_reg(a)], b[!in_reg(b)]))
  if (length(child) > n) {
    child <- child[sample.int(length(child), n)]
  }
  if (length(child) < n) {
    need <- n - length(child)
    pool <- setdiff(union(a, b), child)
    n_ex <- min(need, length(pool))
    if (n_ex > 0) {
      child <- c(child, sample_weighted_ids(pool, n_ex, visit))
      need <- n - length(child)
    }
    if (need > 0) {
      rest <- setdiff(grid_ids, child)
      n_inj <- min(need, length(rest))
      if (n_inj > 0) {
        child <- c(child, sample_weighted_ids(rest, n_inj, visit))
      }
    }
  }
  sort(as.integer(child)[seq_len(min(n, length(child)))])
}
