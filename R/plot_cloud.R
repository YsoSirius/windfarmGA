#' @title Plot outputs of all generations with standard deviations
#' @name plot_cloud
#' @description  Plot the fitness, efficiency and energy outputs of all
#'   generations and the corresponding standard deviations.
#'
#' @export
#'
#' @param result The output of \code{\link{windfarmGA}} or
#'   \code{\link{genetic_algorithm}}
#' @param pl Should the results be plotted? Default is FALSE
#'
#' @family Plotting Functions
#' @return Returns a data.frame with the values for fitness, efficiency and
#'   energy for all evaluated individuals
#'
#' @examples \donttest{
#' ## Add some data examples from the package
#' load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
#'
#' ## Plot the results of a hexagonal grid optimization
#' plcdf <- plot_cloud(resulthex, TRUE)
#'}
plot_cloud <- function(result, pl = FALSE){
  parcloud <- par(ask = FALSE, no.readonly = TRUE)
  on.exit(par(parcloud))

  clouddata <- result[, 7]
  efficiency_cloud <- lapply(clouddata, function(x) x = x[, 1])
  energy_cloud <- lapply(clouddata, function(x) x = x[, 2])
  fitness_cloud <- lapply(clouddata, function(x) x = x[, 3])

  efficiency_per_gen <- energy_per_gen <- fitness_per_gen <- list()
  for (i in 1:length(clouddata)) {
    l <- length(clouddata[[i]][, "EfficAllDir"])
    efficiency_per_gen[[i]] <- t(as.matrix(rbind(rep(i, l),
                                                 efficiency_cloud[[i]])))
    energy_per_gen[[i]] <- t(as.matrix(rbind(rep(i, l),
                                             energy_cloud[[i]])))
    fitness_per_gen[[i]] <- t(as.matrix(rbind(rep(i, l),
                                              fitness_cloud[[i]])))
  }
  efficiency_per_gen <- do.call("rbind", efficiency_per_gen)
  efficiency_per_genmax <- data.frame(efficiency_per_gen)
  max_effic_per_gen <- aggregate(efficiency_per_genmax,
                                 list(efficiency_per_genmax$X1), max)
  mean_effic_per_gen <- aggregate(efficiency_per_genmax,
                                  list(efficiency_per_genmax$X1), mean)
  min_effic_per_gen <- aggregate(efficiency_per_genmax,
                                 list(efficiency_per_genmax$X1), min)
  sd_effic_per_gen <- aggregate(efficiency_per_genmax,
                                list(efficiency_per_genmax$X1), sd)
  efficiency_per_genmax <- cbind(
    "X1" = max_effic_per_gen[, 2],
    "max" = max_effic_per_gen[, 3],
    "mean" = mean_effic_per_gen[, 3],
    "min" = min_effic_per_gen[, 3],
    "sd" = sd_effic_per_gen[, 3])

  energy_per_gen <- do.call("rbind", energy_per_gen)
  energy_per_genmax <- data.frame(energy_per_gen)
  max_energy_per_gen <- aggregate(energy_per_genmax,
                                 list(energy_per_genmax$X1), max)
  mean_energy_per_gen <- aggregate(energy_per_genmax,
                                  list(energy_per_genmax$X1), mean)
  min_energy_per_gen <- aggregate(energy_per_genmax,
                                 list(energy_per_genmax$X1), min)
  sd_energy_per_gen <- aggregate(energy_per_genmax,
                                list(energy_per_genmax$X1), sd)
  energy_per_genmax <- cbind(
    "X1" = max_energy_per_gen[, 2],
    "max" = max_energy_per_gen[, 3],
    "mean" = mean_energy_per_gen[, 3],
    "min" = min_energy_per_gen[, 3],
    "sd" = sd_energy_per_gen[, 3])

  fitness_per_gen <- do.call("rbind", fitness_per_gen)
  fitness_per_genmax <- data.frame(fitness_per_gen)
  max_fit_per_gen <- aggregate(fitness_per_genmax,
                              list(fitness_per_genmax$X1), max)
  mean_fit_per_gen <- aggregate(fitness_per_genmax,
                               list(fitness_per_genmax$X1), mean)
  min_fit_per_gen <- aggregate(fitness_per_genmax,
                              list(fitness_per_genmax$X1), min)
  sd_fit_per_gen <- aggregate(fitness_per_genmax,
                             list(fitness_per_genmax$X1), sd)
  fitness_per_genmax <- cbind(
    "X1" = max_fit_per_gen[, 2],
    "max" = max_fit_per_gen[, 3],
    "mean" = mean_fit_per_gen[, 3],
    "min" = min_fit_per_gen[, 3],
    "sd" = sd_fit_per_gen[, 3])

  if (pl) {
    par(mfrow = c(2, 3))
    graphics::plot(fitness_per_gen, main = "Fitness", xlab = "Generation",
                   ylab = "Fitnessvalue", pch = 20, col = "red", cex = 1.3)
    if (length(clouddata) >= 4) {
      lf <- stats::smooth.spline(x = fitness_per_gen[, 1],
                                 y = fitness_per_gen[, 2],
                                 spar = 0.1)
      graphics::lines(lf, col = "red", lwd = 1.2)
    }
    graphics::points(x = fitness_per_genmax[, "X1"],
                     y = fitness_per_genmax[, "max"],
                     type = "l", col = "red")
    graphics::points(x = fitness_per_genmax[, "X1"],
                     y = fitness_per_genmax[, "min"],
                     type = "l", col = "red")
    graphics::plot(efficiency_per_gen, main = "Efficiency",
                   xlab = "Generation",
                   ylab = "Efficiency in %", pch = 20, col = "orange",
                   cex = 1.3)
    if (length(clouddata) >= 4) {
      le <- stats::smooth.spline(x = efficiency_per_gen[, 1],
                                 y = efficiency_per_gen[, 2],
                                 spar = 0.1)
      graphics::lines(le, col = "orange", lwd = 1.2)
    }
    graphics::points(x = efficiency_per_genmax[, "X1"],
                     y = efficiency_per_genmax[, "max"],
                     type = "l", col = "orange")
    graphics::points(x = efficiency_per_genmax[, "X1"],
                     y = efficiency_per_genmax[, "min"],
                     type = "l", col = "orange")
    graphics::plot(energy_per_gen, main = "Energy", xlab = "Generation",
                   ylab = "Energy in kW", pch = 20, col = "blue", cex = 1.3)
    if (length(clouddata) >= 4) {
      len <- stats::smooth.spline(x = energy_per_gen[, 1],
                                  y = energy_per_gen[, 2],
                                  spar = 0.1)
      graphics::lines(len, col = "blue", lwd = 1.2)
    }
    graphics::points(x = energy_per_genmax[, "X1"],
                     y = energy_per_genmax[, "max"],
                     type = "l", col = "blue")
    graphics::points(x = energy_per_genmax[, "X1"],
                     y = energy_per_genmax[, "min"],
                     type = "l", col = "blue")

    graphics::plot(x = fitness_per_genmax[, "X1"],
                   y = fitness_per_genmax[, "sd"],
                   main = "Standard Deviation Fitness",
                   xlab = "Generation",
                   ylab = "Standard Deviation of Population", col = "red",
                   type = "b")
    graphics::plot(x = efficiency_per_genmax[, "X1"],
                   y = efficiency_per_genmax[, "sd"],
                   main = "Standard Deviation Efficiency",
                   xlab = "Generation",
                   ylab = "Standard Deviation of Population", col = "orange",
                   type = "b")
    graphics::plot(x = energy_per_genmax[, "X1"],
                   y = energy_per_genmax[, "sd"],
                   main = "Standard Deviation Energy",
                   xlab = "Generation",
                   ylab = "Standard Deviation of Population", col = "blue",
                   type = "b")
  }

  clouddatafull <- cbind(Fitn = fitness_per_genmax,
                         Eff = efficiency_per_genmax,
                         Ene = energy_per_genmax)

  colnames(clouddatafull) <- c("FitX1", "FitMax", "FitMean", "FitMin", "FitSD",
                               "EffX1", "EffMax", "EffMean", "EffMin", "EffSD",
                               "EneX1", "EneMax", "EneMean", "EneMin", "EneSD")

  invisible(clouddatafull)
}