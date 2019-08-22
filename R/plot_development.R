#' @title Plot the progress of populations
#' @name plot_development
#' @description Plot the changes in mean and max fitness values to previous
#' generation.
#'
#' @export
#'
#' @param result The output \code{\link{windfarmGA}} or 
#'  \code{\link{genetic_algorithm}}
#'
#' @family Plotting Functions
#' @return NULL
#' @examples \donttest{
#' plot_development(resultrect)
#' }
plot_development <- function(result){
  parbeorwo <- par(ask = FALSE, no.readonly = TRUE)
  on.exit(par(parbeorwo))
  par(mfrow = c(2, 1))

  beorworse <- do.call("rbind", result[, 9])
  maxdif <- data.frame(
    diff = beorworse[, 1],
    farbe = 0)

  maxdif$farbe[maxdif$diff < 0]  <- "red"
  maxdif$farbe[maxdif$diff > 0] <- "green"
  maxdif$farbe[maxdif$diff == 0] <- "orange"
  plot(maxdif$diff, type = "b", col = maxdif$farbe, pch = 20, cex = 2)
  abline(0, 0)
  title("Max Difference to previous generation")


  meandif <- data.frame(
    diff = beorworse[, 2],
    farbe = 0)
  meandif$farbe[meandif$diff < 0]  <- "red"
  meandif$farbe[meandif$diff > 0] <- "green"
  meandif$farbe[meandif$diff == 0] <- "orange"
  plot(meandif$diff, type = "b", col = meandif$farbe, pch = 20, cex = 2)
  abline(0, 0)
  title("Mean Difference to previous generation")

  return()
}