## Time sequential vs PSOCK fitness. Not in the CRAN tarball.
##
##   source("experimental/bench_parallel.R")
##   bench_parallel()
##   bench_parallel(iteration = 80, n_cluster = c(2, 4))
##   bench_parallel(iteration = 80, n_cluster = 6, sequential = FALSE, sec_ref = 358)
##
## Only `fitness()` is parallel (`foreach` over individuals). On this
## small site 4 workers is usually the peak; 8 often dies on Windows
## (PSOCK unserialize / RAM). Short runs can be slower than sequential.

need_parallel_pkgs <- function() {
  ok <- TRUE
  for (pkg in c("parallel", "doParallel", "foreach")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Need ", pkg, ": install.packages(\"", pkg, "\")")
      ok <- FALSE
    }
  }
  ok
}

bench_site <- function() {
  sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
}

bench_wind <- function(n_dir = 8L, ws = 12) {
  data.frame(
    ws = ws,
    wd = seq(0, 360 - 360 / n_dir, length.out = n_dir)
  )
}

run_ga <- function(area, wind, n, iteration, parallel, n_cluster, rotor) {
  genetic_algorithm(
    area = area,
    n = n,
    iteration = iteration,
    wind = wind,
    rotor = rotor,
    rotor_height = 100,
    plot = FALSE,
    verbose = FALSE,
    parallel = parallel,
    n_cluster = n_cluster
  )
}

#' Compare genetic_algorithm() with and without a PSOCK cluster.
#'
#' @param iteration Generations. More work makes parallel more likely to win.
#' @param n Turbines
#' @param n_cluster Integer vector of worker counts
#' @param n_dir Wind directions in the rose
#' @param rotor Rotor radius (m); grid is `rotor * 5`
#' @param sequential Run the sequential baseline. Set `FALSE` and pass
#'   `sec_ref` to skip a long repeat.
#' @param sec_ref Sequential seconds used for speedup when
#'   `sequential = FALSE`
#' @return A data.frame of timings, returned invisibly
bench_parallel <- function(iteration = 8L,
                           n = 12L,
                           n_cluster = 2L,
                           n_dir = 8L,
                           rotor = 30,
                           sequential = TRUE,
                           sec_ref = NULL) {
  if (!requireNamespace("windfarmGA", quietly = TRUE)) {
    stop("load the package first: devtools::load_all() or library(windfarmGA)")
  }
  if (!need_parallel_pkgs()) {
    return(invisible(NULL))
  }

  area <- bench_site()
  wind <- bench_wind(n_dir)
  n_cluster <- unique(pmax(1L, as.integer(n_cluster)))
  cores <- parallel::detectCores()
  if (is.na(cores)) {
    cores <- NA_integer_
  }
  if (any(n_cluster > 4L)) {
    message(
      "Note: on this site 4 workers usually beat 6–8. ",
      "Windows PSOCK often drops connections above ~6."
    )
  }

  message(
    "site ~1.5 km x 1 km, n = ", n, ", iteration = ", iteration,
    ", ", n_dir, " wind dirs, detectCores() = ", cores
  )

  rows <- list()
  sec0 <- NA_real_

  if (isTRUE(sequential)) {
    t0 <- proc.time()[["elapsed"]]
    res0 <- run_ga(area, wind, n, iteration, FALSE, 1L, rotor)
    sec0 <- proc.time()[["elapsed"]] - t0
    rows[[1]] <- data.frame(
      mode = "sequential",
      n_cluster = 1L,
      seconds = round(sec0, 2),
      speedup = 1,
      generations = nrow(res0),
      note = "",
      stringsAsFactors = FALSE
    )
    message(sprintf("  sequential          %6.2f s", sec0))
    gc()
  } else if (!is.null(sec_ref)) {
    sec0 <- as.numeric(sec_ref)
    rows[[1]] <- data.frame(
      mode = "sequential",
      n_cluster = 1L,
      seconds = round(sec0, 2),
      speedup = 1,
      generations = NA_integer_,
      note = "sec_ref",
      stringsAsFactors = FALSE
    )
    message(sprintf("  sequential (ref)    %6.2f s", sec0))
  }

  for (nc in n_cluster) {
    gc()
    t1 <- proc.time()[["elapsed"]]
    res <- tryCatch(
      run_ga(area, wind, n, iteration, TRUE, nc, rotor),
      error = function(e) e
    )
    sec <- proc.time()[["elapsed"]] - t1
    if (inherits(res, "error")) {
      rows[[length(rows) + 1]] <- data.frame(
        mode = "parallel",
        n_cluster = nc,
        seconds = round(sec, 2),
        speedup = NA_real_,
        generations = NA_integer_,
        note = conditionMessage(res),
        stringsAsFactors = FALSE
      )
      message(sprintf("  parallel n_cluster=%-2d FAILED after %.2f s: %s",
                      nc, sec, conditionMessage(res)))
    } else {
      spd <- if (is.finite(sec0) && sec > 0) round(sec0 / sec, 2) else NA_real_
      rows[[length(rows) + 1]] <- data.frame(
        mode = "parallel",
        n_cluster = nc,
        seconds = round(sec, 2),
        speedup = spd,
        generations = nrow(res),
        note = "",
        stringsAsFactors = FALSE
      )
      message(sprintf("  parallel n_cluster=%-2d %6.2f s  (x%s)",
                      nc, sec, if (is.na(spd)) "?" else sprintf("%.2f", spd)))
    }
  }

  out <- do.call(rbind, rows)
  print(out, row.names = FALSE)
  invisible(out)
}
