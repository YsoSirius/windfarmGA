#' @title Mark a genetic_algorithm result
#' @name as_windfarmGA
#' @description Attach the `windfarmGA` class so [print()] and [plot()]
#'   dispatch. New runs from [genetic_algorithm()] already have this class.
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
  coeff <- tryCatch(
    as.data.frame(do.call("rbind", x[, "allparkcoeff"])),
    error = function(e) NULL
  )
  if (is.null(coeff) || !nrow(coeff)) {
    return(NULL)
  }
  last <- coeff[nrow(coeff), , drop = FALSE]
  list(
    n_gen = nrow(coeff),
    energy = last$MaxEnergyRedu,
    efficiency = last$maxParkwirkungsg,
    fitness = last$maxparkfitness
  )
}

ga_result_inputs <- function(x) {
  tryCatch(x[1, "inputData"][[1]], error = function(e) NULL)
}

#' @export
#' @method print windfarmGA
#' @rdname as_windfarmGA
print.windfarmGA <- function(x, ...) {
  inp <- ga_result_inputs(x)
  best <- ga_result_best(x)
  n_turb <- if (!is.null(inp) && "Number of turbines" %in% rownames(inp)) {
    inp["Number of turbines", ][[1]]
  } else {
    NA
  }
  cat("windfarmGA result\n")
  if (!is.null(best)) {
    cat(sprintf(
      "  %s generations, %s turbines\n  best energy %s kW, efficiency %s %%\n  best fitness %s\n",
      best$n_gen, n_turb,
      format(best$energy, digits = 6),
      format(best$efficiency, digits = 5),
      format(best$fitness, digits = 6)
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
