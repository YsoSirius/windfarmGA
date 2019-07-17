#' @title Plot the evolution of fitness values
#' @name plot_evolution
#' @description  Plot the evolution of energy outputs and efficiency rates over
#'   the whole generations. Plots min, mean and max values.
#' @export
#'
#' @param result The output of \code{\link{windfarmGA}} or
#'   \code{\link{genetic_algorithm}}
#' @param ask Should R wait for interaction for subsequent plotting. Default is
#'   TRUE
#' @param spar A numeric value determining how exact a spline should be drawn.
#'   Default is 0.1
#'
#' @family Plotting Functions
#' @return NULL
#' @examples \donttest{
#' ## Add some data examples from the package
#' load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
#'
#' ## Plot the results of a rectangular grid optimization
#' plot_evolution(resultrect, ask = TRUE, spar = 0.1)
#'}
plot_evolution <- function(result, ask = TRUE, spar = 0.1){
  ## Set the graphical parameters
  parevol <- par(no.readonly = TRUE)
  on.exit(par(parevol))
  par(mfrow = c(1, 1))

  result1 <- as.data.frame(do.call("rbind", result[, 1]))

  plot(result1$minParkwirkungsg, xaxt = "n",
       main = "Park Efficiency per Generation", xlab = "Generation",
       ylab = "Park Efficiency in %", cex = 0.8, cex.main = 0.8,
       col = "red", pch = 20,
       ylim = c(min(result1$minParkwirkungsg), max(result1$maxParkwirkungsg)))
  axis(1, at = 1:nrow(result1), tick = TRUE)
  grid(col = "black")
  points(result1$meanParkwirkungsg, ylab = "MeanxParkwirkungsg", cex = 1.2,
         col = "blue", pch = 20)
  points(result1$maxParkwirkungsg, ylab = "maxParkwirkungsg", cex = 1.2,
         col = "green", pch = 20)
  x <- 1:length(result1$MaxEnergyRedu)

  if (nrow(result) >= 4) {
    lmin <- smooth.spline(x, result1$minParkwirkungsg, spar = spar)
    lines(lmin, col = "red", lwd = 1.2)
    lmea <- smooth.spline(x, result1$meanParkwirkungsg, spar = spar)
    lines(lmea, col = "blue", lwd = 1.2)
    lmax <- smooth.spline(x, result1$maxParkwirkungsg, spar = spar)
    lines(lmax, col = "green", lwd = 1.2)
  }

  par(ask = ask)

  plot(result1$MeanEnergyRedu, xaxt = "n", main = "Energy Yield per Generation",
       xlab = "Generation", ylab = "Energy in kW", cex = 0.8, cex.main = 0.8,
       col = "blue", pch = 20,
       ylim = c(min(result1$MinEnergyRedu), max(result1$MaxEnergyRedu)))
  axis(1, at = 1:nrow(result1), tick = TRUE)
  grid(col = "black")
  points(result1$MaxEnergyRedu, ylab = "maxParkwirkungsg", cex = 1.2,
         col = "green", pch = 20)
  points(result1$MinEnergyRedu, ylab = "minParkwirkungsg", cex = 1.2,
         col = "red", pch = 20)

  if (nrow(result) >= 4) {
    emean <- smooth.spline(x, result1$MeanEnergyRedu, spar = spar)
    lines(emean, col = "blue", lwd = 1.2)
    emax <- smooth.spline(x, result1$MaxEnergyRedu, spar = spar)
    lines(emax, col = "green", lwd = 1.2)
    emin <- smooth.spline(x, result1$MinEnergyRedu, spar = spar)
    lines(emin, col = "red", lwd = 1.2)
  }

  return()
}