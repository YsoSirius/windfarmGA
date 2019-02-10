#' @title Plot if previous population was better or worse
#' @name plotbeorwor
#' @description  Plot the changes in mean and max fitness values to previous
#' generation.
#'
#' @export
#'
#' @importFrom graphics plot par abline title
#'
#' @param result The output matrix of \code{\link{windfarmGA}} or
#' \code{\link{genAlgo}}, which has stored all relevant information.
#'
#' @return NULL
#' @examples \donttest{
#' ## Add some data examples from the package
#' load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
#'
#' ## Plot the results of a hexagonal grid optimization
#' result <- resulthex
#' plotbeorwor(result)
#' }
#' @author Sebastian Gatscha
plotbeorwor <- function(result){
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