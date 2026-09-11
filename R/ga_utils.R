layout_key <- function(x) {
  if (is.matrix(x) || is.data.frame(x)) {
    col <- if ("ID" %in% colnames(x)) "ID" else "Rect_ID"
    ids <- x[, col]
  } else {
    ids <- x
  }
  paste(sort(as.integer(ids)), collapse = ",")
}

sample_weighted_ids <- function(pool, size, visit = NULL) {
  pool <- as.integer(pool)
  size <- as.integer(size)
  if (size < 1L || !length(pool)) {
    return(integer(0))
  }
  size <- min(size, length(pool))
  if (is.null(visit) || !length(visit)) {
    return(pool[sample.int(length(pool), size)])
  }
  w <- 1 / (1 + as.numeric(visit[as.character(pool)]))
  w[!is.finite(w)] <- 1
  if (sum(w) <= 0) {
    w[] <- 1
  }
  pool[sample.int(length(pool), size, prob = w)]
}

fitness_with_cache <- function(cache, population, ...) {
  n_sel <- length(population)
  keys <- vapply(population, layout_key, character(1))
  fit <- vector("list", n_sel)
  need <- integer(0)
  for (j in seq_len(n_sel)) {
    if (exists(keys[j], envir = cache, inherits = FALSE)) {
      row <- get(keys[j], envir = cache, inherits = FALSE)
      row[, "Run"] <- j
      fit[[j]] <- row
    } else {
      need <- c(need, j)
    }
  }
  if (length(need)) {
    eval_at <- need[!duplicated(keys[need])]
    fresh <- fitness(population = population[eval_at], ...)
    by_key <- list()
    for (k in seq_along(eval_at)) {
      by_key[[keys[eval_at[k]]]] <- fresh[[k]]
    }
    for (j in need) {
      row <- by_key[[keys[j]]]
      row[, "Run"] <- j
      assign(keys[j], row, envir = cache)
      fit[[j]] <- row
    }
  }
  names(fit) <- keys
  attr(fit, "n_new") <- if (length(need)) {
    sum(!duplicated(keys[need]))
  } else {
    0L
  }
  fit
}

elite_offspring <- function(elite_ids, worse_ids, grid_ids, n_mut, n_mix,
                            mut_p) {
  if (!is.matrix(elite_ids)) {
    elite_ids <- matrix(elite_ids, ncol = 1)
  }
  n <- nrow(elite_ids)
  n_el <- ncol(elite_ids)
  kids <- NULL
  n_mut <- as.integer(n_mut)
  n_mix <- as.integer(n_mix)
  if (n_mut > 0L) {
    copies <- elite_ids[, rep(seq_len(n_el), each = n_mut), drop = FALSE]
    kids <- swap_mutation(
      copies, grid_ids, p = mut_p, min_swaps = 1L, visit = NULL
    )
  }
  if (n_mix > 0L && !is.null(worse_ids) && ncol(as.matrix(worse_ids)) > 0) {
    worse_ids <- as.matrix(worse_ids)
    nw <- ncol(worse_ids)
    mix <- matrix(NA_integer_, nrow = n, ncol = n_el * n_mix)
    k <- 0L
    for (e in seq_len(n_el)) {
      for (m in seq_len(n_mix)) {
        k <- k + 1L
        wcol <- worse_ids[, sample.int(nw, 1L)]
        mix[, k] <- set_cross_one(
          elite_ids[, e], wcol, grid_ids, p_inject = 0, visit = NULL
        )
      }
    }
    kids <- cbind(kids, mix)
  }
  kids
}

## Explore, then anneal (refine). Refine is not absorbing: after a hold
## period a short disturbance pulse raises the same GA rates even if the
## max is still creeping up (seasons / intermediate disturbance).
adapt_operator_rates <- function(phase, stall, coverage, generation,
                                 teil, mut_adapt, p_inj,
                                 mut_floor, mut_ceil, mut_target,
                                 inj_lo = 0.2, inj_hi = 0.4,
                                 inj_refine = 0.15,
                                 refine_after = 12L,
                                 refine_min_gen = 18L,
                                 refine_teil = 2.2,
                                 phase_age = 0L,
                                 refine_hold = 25L,
                                 explore_pulse = 10L) {
  pri <- if (stall == 0L) "improved" else "stagnated"
  prev <- phase
  phase_age <- as.integer(phase_age) + 1L
  refine_hold <- max(1L, as.integer(refine_hold))
  explore_pulse <- max(1L, as.integer(explore_pulse))

  if (identical(phase, "refine") && coverage < 0.3) {
    phase <- "explore"
  } else if (identical(phase, "refine") && phase_age >= refine_hold) {
    phase <- "pulse"
  } else if (identical(phase, "pulse") && phase_age >= explore_pulse) {
    phase <- if (coverage >= 0.35) "refine" else "explore"
  } else if (identical(phase, "explore") &&
      generation >= refine_min_gen &&
      stall >= refine_after &&
      coverage >= 0.35) {
    phase <- "refine"
  }
  if (!identical(phase, prev)) {
    phase_age <- 1L
  }

  if (identical(phase, "refine")) {
    p_inj <- max(inj_refine, p_inj - 0.012)
    mut_adapt <- max(mut_target, mut_adapt * 0.95)
    teil <- min(refine_teil, teil + 0.05)
  } else if (identical(phase, "pulse")) {
    p_inj <- min(0.28, p_inj + 0.015)
    mut_adapt <- min(mut_ceil, mut_adapt * 1.03)
    teil <- max(1.6, teil - 0.04)
  } else if (stall == 0L) {
    teil <- teil + 0.04
    mut_adapt <- max(mut_floor, mut_adapt * 0.97)
    p_inj <- max(inj_lo, p_inj - 0.008)
  } else {
    teil <- teil - 0.025
    mut_adapt <- min(mut_ceil, mut_adapt * 1.02)
    p_inj <- min(inj_hi, p_inj + 0.012)
    if (coverage < 0.35) {
      p_inj <- min(inj_hi, p_inj + 0.02)
    }
  }

  teil <- min(4, max(4 / 3, teil))
  if (identical(phase, "refine")) {
    p_inj <- min(inj_hi, max(inj_refine, p_inj))
  } else {
    p_inj <- min(inj_hi, max(min(inj_lo, inj_refine), p_inj))
  }
  mut_adapt <- min(mut_ceil, max(mut_floor, mut_adapt))

  list(
    phase = phase,
    phase_age = phase_age,
    pri = pri,
    teil = round(teil, 3),
    mut_adapt = round(mut_adapt, 5),
    p_inj = round(p_inj, 3)
  )
}

## More elites while refining a plateau (keep good memory). Fewer during
## a disturbance pulse so new lineages can enter the archive.
adapt_elite_n <- function(n_elite, n_pop, phase = "explore", stall = 0L) {
  n_el <- max(1L, as.integer(n_elite))
  n_pop <- max(1L, as.integer(n_pop))
  stall <- as.integer(stall)
  if (identical(phase, "pulse")) {
    n_el <- max(1L, n_el - 1L)
  } else if (identical(phase, "refine")) {
    if (stall >= 20L) {
      n_el <- n_el + 3L
    } else if (stall >= 8L) {
      n_el <- n_el + 2L
    }
  }
  min(n_el, n_pop)
}

## Rook/hex adjacency: cells whose centres sit at the typical grid step.
## Rect diagonals (sqrt(2) * step) are excluded; hex neighbours are kept.
grid_neighbors <- function(grid_xy) {
  ids <- as.integer(grid_xy[, "ID"])
  xy <- cbind(as.numeric(grid_xy[, "X"]), as.numeric(grid_xy[, "Y"]))
  n <- length(ids)
  empty <- stats::setNames(rep(list(integer(0)), n), as.character(ids))
  if (n < 2L) {
    return(empty)
  }
  d <- as.matrix(stats::dist(xy))
  diag(d) <- Inf
  nn <- apply(d, 1, min)
  step <- stats::median(nn[is.finite(nn)])
  if (!is.finite(step) || step <= 0) {
    return(empty)
  }
  adj <- d <= step * 1.2
  out <- lapply(seq_len(n), function(i) ids[which(adj[i, ])])
  stats::setNames(out, as.character(ids))
}

neighbor_swap <- function(ids, neighbors, n_moves = 1L) {
  occupied <- as.integer(ids)
  all_ids <- as.integer(names(neighbors))
  n_moves <- max(1L, as.integer(n_moves))
  for (m in seq_len(n_moves)) {
    free <- setdiff(all_ids, occupied)
    if (!length(free)) {
      break
    }
    movable <- which(vapply(as.character(occupied), function(k) {
      any(neighbors[[k]] %in% free)
    }, logical(1)))
    if (!length(movable)) {
      break
    }
    i <- movable[[sample.int(length(movable), 1L)]]
    dest <- intersect(neighbors[[as.character(occupied[i])]], free)
    occupied[i] <- dest[[sample.int(length(dest), 1L)]]
  }
  occupied
}
