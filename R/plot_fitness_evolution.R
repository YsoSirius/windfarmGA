#' @title Plot the changes of min/mean/max fitness values
#' @name plot_fitness_evolution
#' @description  Plot the evolution of fitness values and the change in the min,
#'   mean and max fitness values to the former generations.
#' @export
#'
#' @param result The output of function \code{\link{windfarmGA}} or
#'   \code{\link{genetic_algorithm}}
#' @param spar A numeric value determining how exact a spline should be drawn.
#'   Default is 0.1
#'
#' @family Plotting Functions
#' @return NULL
#' @examples \donttest{
#' ## Add some data examples from the package
#' load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
#'
#' ## Plot the results of a hexagonal grid optimization
#' plot_fitness_evolution(resulthex, 0.1)
#' }
plot_fitness_evolution <- function(result, spar = 0.1){
  # library(stats)
  oparplotfitness <- par(ask = FALSE, no.readonly = TRUE)
  on.exit(par(oparplotfitness))
  par(mfrow = c(1, 1))
  
  x <- result[, 4]
  x <- x[-c(1)]
  x1 <- do.call("rbind", x)
  par(mar = c(4, 5, 4, 2))
  result <- as.data.frame(do.call("rbind", result[, 1]))

  layout(mat = matrix(c(1, 2, 3, 4, 4, 4), nrow = 2, ncol = 3, byrow = TRUE))
  # Wenn ueber 0, dann ist die Parkfitness besser als die vorige Generation.
  minge <- x1[seq(1, length(x1[,1]), 2), 1]
  minge2 <- x1[seq(2, length(x1[,2]), 2), 1]
  ming3 <- minge - minge2
  ming3 <- c(0, ming3)
  ming3 <- as.data.frame(ming3)
  ming3$farbe <- 0
  ming3$farbe[ming3$ming3 < 0]  <- "red"
  ming3$farbe[ming3$ming3 > 0] <- "green"
  ming3$farbe[ming3$ming3 == 0] <- "orange"
  plot(ming3$ming3, type = "b", col = ming3$farbe, pch = 20, cex = 2,
       xlab = "Generation", ylab = "Beter or Worse")
  title(main = "Minimal Fitness Values",sub = "compared to previous generation",
        col.main = "red")
  abline(0, 0)
  grid(col = "black")

  meange <- x1[seq(1, length(x1[, 1]), 2), 3]
  meange2 <- x1[seq(2, length(x1[, 2]), 2), 3]
  meag3 <- meange - meange2
  meag3 <- c(0, meag3)
  meag3 <- as.data.frame(meag3)
  meag3$farbe <- 0
  meag3$farbe[meag3$meag3 < 0]  <- "red"
  meag3$farbe[meag3$meag3 > 0] <- "green"
  meag3$farbe[meag3$meag3 == 0] <- "orange"
  plot(meag3$meag3, type = "b", col = meag3$farbe, pch = 20, cex = 2,
       xlab = "Generation", ylab = "Beter or Worse")
  title(main = "Mean Fitness Values", sub = "compared to previous generation",
        col.main = "orange")
  abline(0, 0)
  grid(col = "black")

  maxge <- x1[seq(1, length(x1[, 1]), 2), 2]
  maxge2 <- x1[seq(2, length(x1[, 2]), 2), 2]
  mg3 <- maxge - maxge2
  mg3 <- c(0, mg3)
  mg3 <- as.data.frame(mg3)
  mg3$farbe <- 0
  mg3$farbe[mg3$mg3 < 0]  <- "red"
  mg3$farbe[mg3$mg3 > 0] <- "green"
  mg3$farbe[mg3$mg3 == 0] <- "orange"
  plot(mg3$mg3, type = "b", col = mg3$farbe, pch = 20, cex = 2,
       xlab = "Generation", ylab = "Beter or Worse")
  title(main = "Maximal Fitness Values", sub = "compared to previous generation",
        col.main = "darkgreen")
  abline(0, 0)
  grid(col = "black")

  plot(result$minparkfitness, xaxt = "n", main = "Parkfitness per Generation",
       ylab = "Parkfitness in %", xlab = "Generation", cex = 2, col = "red",
       pch = 20, ylim = c(min(result$minparkfitness), max(result$maxparkfitness)))
  axis(1, at = 1:nrow(result),tick = TRUE)
  grid(col = "black")
  points(result$meanparkfitness, ylab = "MeanParkF", cex = 2, 
         col = "blue", pch = 20)
  points(result$maxparkfitness, ylab = "maxParkF", cex = 2, 
         col = "green", pch = 20)
  x <- 1:length(result$maxparkfitness)

  if (nrow(result) >= 4) {
    lmin <- smooth.spline(x,result$minparkfitness, spar = spar)
    lines(lmin, col = "red", lwd = 1.6)
    lmea <- smooth.spline(x,result$meanparkfitness, spar = spar)
    lines(lmea, col = "blue", lwd = 1.6)
    lmax <- smooth.spline(x,result$maxparkfitness, spar = spar)
    lines(lmax, col = "green", lwd = 1.6)
  }

  return()
}