#' @title Mark a genetic_algorithm result
#' @name as_windfarmGA
#' @description Attach the `windfarmGA` class so [print()] and [plot()]
#'   dispatch. New runs from [genetic_algorithm()] already have this class.
#'   `print()` shows run inputs, the wind table, and each generation that
#'   set a new fitness maximum.
#' @export
#'
#' @param x A result matrix from [genetic_algorithm()].
#' @return `x` with class `windfarmGA`.
as_windfarmGA <- function(x) {
  if (!is_ga_result(x)) {
    stop("x does not look like a genetic_algorithm() result.")
  }
  if (!inherits(x, "windfarmGA")) {
    class(x) <- c("windfarmGA", class(x))
  }
  x
}

is_ga_result <- function(x) {
  is.matrix(x) && any(c("inputData", "allparkcoeff") %in% colnames(x))
}

ga_result_best <- function(x) {
  rec <- ga_result_records(x)
  if (is.null(rec) || !nrow(rec)) {
    return(NULL)
  }
  last <- rec[nrow(rec), , drop = FALSE]
  list(
    n_gen = rec$n_gen[[1]],
    best_gen = last$generation,
    energy = last$energy_kW,
    efficiency = last$efficiency_pct,
    fitness = last$fitness
  )
}

ga_result_inputs <- function(x) {
  inp <- tryCatch(x[1, "inputData"][[1]], error = function(e) NULL)
  if (is.list(inp) && !is.null(inp$Input_Data)) {
    inp <- inp$Input_Data
  }
  inp
}

ga_inp <- function(inp, ...) {
  if (is.null(inp) || is.null(rownames(inp))) {
    return(NA)
  }
  for (nm in c(...)) {
    hit <- rownames(inp) == nm | tolower(rownames(inp)) == tolower(nm)
    if (any(hit)) {
      return(inp[which(hit)[1], 1])
    }
  }
  NA
}

ga_result_wind <- function(x) {
  w <- tryCatch(x[1, "inputWind"][[1]], error = function(e) NULL)
  if (is.null(w)) {
    return(NULL)
  }
  if (is.list(w) && !is.data.frame(w) && !is.null(w$Windspeed_Data)) {
    w <- w$Windspeed_Data
  }
  if (is.list(w) && !is.data.frame(w) && length(w) >= 1L && is.data.frame(w[[1]])) {
    df <- w[[1]]
    if (length(w) >= 2L && !("probab" %in% names(df))) {
      df$probab <- w[[2]]
    }
    w <- df
  }
  if (is.matrix(w)) {
    w <- as.data.frame(w)
  }
  if (!is.data.frame(w) || !nrow(w)) {
    return(NULL)
  }
  w
}

## One row per generation that set a new fitness maximum.
ga_result_records <- function(x) {
  coeff <- tryCatch(
    as.data.frame(do.call("rbind", x[, "allparkcoeff"])),
    error = function(e) NULL
  )
  if (is.null(coeff) || !nrow(coeff)) {
    return(NULL)
  }
  fit <- as.numeric(coeff$maxparkfitness)
  n <- length(fit)
  rec <- rep(-Inf, n)
  rec[1] <- if (is.finite(fit[1])) fit[1] else -Inf
  if (n > 1L) {
    for (i in 2:n) {
      rec[i] <- max(rec[i - 1L], if (is.finite(fit[i])) fit[i] else -Inf)
    }
  }
  prev <- c(-Inf, rec[-n])
  keep <- is.finite(fit) & rec > prev + 1e-8
  if (!any(keep)) {
    return(NULL)
  }
  data.frame(
    generation = which(keep),
    energy_kW = as.numeric(coeff$MaxEnergyRedu[keep]),
    efficiency_pct = as.numeric(coeff$maxParkwirkungsg[keep]),
    fitness = fit[keep],
    n_gen = n,
    stringsAsFactors = FALSE
  )
}

print_ga_inputs <- function(inp) {
  if (is.null(inp)) {
    return(invisible(NULL))
  }
  n <- ga_inp(inp, "Number of turbines")
  rotor <- ga_inp(inp, "Rotorradius")
  hub <- ga_inp(inp, "Rotor Height")
  ref <- ga_inp(inp, "Reference Height")
  grid <- ga_inp(inp, "Grid Method")
  res <- ga_inp(inp, "Resolution")
  prop <- ga_inp(inp, "Percentage of Polygon")
  iter <- ga_inp(inp, "Iterations")
  mut <- ga_inp(inp, "Mutation Rate")
  sel <- ga_inp(inp, "Selection Method")
  elite <- ga_inp(inp, "Elitarism")
  n_el <- ga_inp(inp, "Elite count")
  terr <- ga_inp(inp, "Topographie")
  weib <- ga_inp(inp, "Active Weibull")
  par <- ga_inp(inp, "parallel Processing", "Parallel Processing")
  crs <- ga_inp(inp, "Projection")
  cat("Inputs\n")
  cat(sprintf("  %s turbines   rotor %s m   hub %s m (ref %s m)\n", n, rotor, hub, ref))
  prop_n <- suppressWarnings(as.numeric(prop))
  prop_txt <- if (is.finite(prop_n) && prop_n <= 1.5) {
    paste0(round(100 * prop_n), "%")
  } else if (is.finite(prop_n)) {
    paste0(prop_n, "%")
  } else {
    as.character(prop)
  }
  cat(sprintf("  grid %s @ %s m   %s of site   CRS %s\n", grid, res, prop_txt, crs))
  elite_txt <- if (is.na(n_el)) as.character(elite) else paste0(elite, ", ", n_el, " elites")
  cat(sprintf(
    "  %s generations   mutation %s   selection %s   elitism %s\n",
    iter, mut, sel, elite_txt
  ))
  cat(sprintf("  terrain %s   Weibull %s   parallel %s\n", terr, weib, par))
}

print_ga_wind <- function(wind) {
  cat("Wind\n")
  if (is.null(wind)) {
    cat("  (none stored)\n")
    return(invisible(NULL))
  }
  nms <- names(wind)
  wd <- if (any(grepl("^(wd|dir)", nms, ignore.case = TRUE))) {
    wind[[grep("^(wd|dir)", nms, ignore.case = TRUE)[1]]]
  } else if (ncol(wind) >= 2L) {
    wind[[2]]
  } else {
    NA
  }
  ws <- if (any(grepl("^(ws|speed)", nms, ignore.case = TRUE))) {
    wind[[grep("^(ws|speed)", nms, ignore.case = TRUE)[1]]]
  } else {
    wind[[1]]
  }
  pr <- if (any(grepl("prob", nms, ignore.case = TRUE))) {
    wind[[grep("prob", nms, ignore.case = TRUE)[1]]]
  } else {
    NULL
  }
  tab <- data.frame(wd = wd, ws = ws, stringsAsFactors = FALSE)
  if (!is.null(pr)) {
    tab$probab <- pr
  }
  max_n <- 12L
  if (nrow(tab) > max_n) {
    print(utils::head(tab, max_n), row.names = FALSE, right = FALSE)
    cat(sprintf("  ... %d directions\n", nrow(tab)))
  } else {
    print(tab, row.names = FALSE, right = FALSE)
  }
}

#' @export
#' @method print windfarmGA
#' @rdname as_windfarmGA
print.windfarmGA <- function(x, ...) {
  rec <- ga_result_records(x)
  cat("windfarmGA result\n")
  print_ga_inputs(ga_result_inputs(x))
  print_ga_wind(ga_result_wind(x))
  if (!is.null(rec) && nrow(rec)) {
    cat("New best (fitness)\n")
    show <- rec[, c("generation", "energy_kW", "efficiency_pct", "fitness")]
    print(show, row.names = FALSE, right = FALSE)
    last <- rec[nrow(rec), ]
    cat(sprintf(
      "Best at generation %s / %s\n",
      last$generation, last$n_gen
    ))
  }
  cat("  plot(x, area)   explore_result(x, area)\n")
  invisible(x)
}

#' @export
#' @method plot windfarmGA
#' @rdname as_windfarmGA
#' @param y The site polygon (same as `area` in [genetic_algorithm()]).
#' @param ... Passed to [plot_windfarmGA()].
plot.windfarmGA <- function(x, y, ...) {
  if (missing(y)) {
    stop("plot() needs the site polygon: plot(result, area).")
  }
  plot_windfarmGA(x, y, ...)
}
