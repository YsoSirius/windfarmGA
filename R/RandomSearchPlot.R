#' @title RandomSearchPlot Plot the result of a randomized ouput.
#' @name RandomSearchPlot
#' @description Plotting method for the functions \code{\link{RandomSearchTurb}}
#' and \code{\link{RandomSearch}}.
#' 
#' @export
#'
#' @importFrom raster plot 
#'
#' @param resultRS The result of the random functions \code{\link{RandomSearchTurb}}
#' and \code{\link{RandomSearch}}.
#' @param result The result of the function \code{\link{genAlgo}} or
#' \code{\link{windfarmGA}}.
#' @param Polygon1 The Polygon for the wind farm area.
#' @param best How many best candidates to plot.
#' 
#' 
#' @return NULL
#' 
#' @examples \donttest{
#' load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
#'
#' Res = RandomSearchTurb(result = resultrect, Polygon1 = polygon, n=10)
#' RandomSearchPlot(resultRS = Res, result = resultrect, Polygon1 = polygon, best=2)
#' }
#' @author Sebastian Gatscha
RandomSearchPlot <- function(resultRS, result, Polygon1, best) {

  op <- par(no.readonly = TRUE) 
  par(mfrow = c(1, 2))

  resultRS1 <- do.call("rbind", cbind(resultRS))
  a <- resultRS1[,"EnergyOverall"]
  order1 <- order(a, decreasing = TRUE)
  resultRS1 <- resultRS1[order1, ]


  # resBest <- resultRS1[!duplicated(resultRS1$Run) & !duplicated(resultRS1$bestGARun),]
  resBest <- resultRS1[!duplicated(resultRS1[, "Run"]),]
  if (nrow(resBest) < best) {best <- nrow(resBest)}

  resBest <- resBest[1:best,]
  # resBest <- resBest[nrow(resBest):(nrow(resBest)-best),]

  resultRS2 <- list()
  for (nr in 1:nrow(resBest)) {
    resultRS2[[nr]] <- resultRS1[resultRS1[, "Run"] == resBest[, "Run"][nr] & 
                                   resultRS1[, "bestGARun"] == resBest[, "bestGARun"][nr],]
  }
  resultRS1 <- resultRS2

  resultRS1 <- rev(resultRS1)
  resBest <- resBest[order(resBest[,"EnergyOverall"]),]

  resolR <- as.numeric(result[,"inputData"][[1]][,1]["Resolution"])
  PropG <- as.numeric(result[,"inputData"][[1]][,1]["Percentage of Polygon"])

  Polygon1 <- isSpatial(Polygon1)
  Grid <- GridFilter(Polygon1, resol = resolR, prop = PropG)[[2]]
  rbPal1 <- grDevices::colorRampPalette(c("green","red"))
  col2res <- "lightblue"

  for (i in 1:length(resultRS1) ) {
    ## Original GA-result ################
    bestGAR <- resBest[,"bestGARun"][i]
    bestrestGA <- result[bestGAR,]$bestPaEn
    brOrig <- length(levels(factor(bestrestGA[,"AbschGesamt"])))
    if (brOrig > 1) {
      ColOri <- rbPal1(brOrig)[as.numeric(cut(as.numeric(bestrestGA[,"AbschGesamt"]),
                                              breaks = brOrig))]
    } else {
      ColOri <- "green"
    }

    bestrestGA[,"EnergyOverall"] <- round(bestrestGA[,"EnergyOverall"], 2)
    bestrestGA[,"EfficAllDir"] <- round(bestrestGA[,"EfficAllDir"], 2)
    raster::plot(Polygon1, col = col2res, 
                 main = paste("Original - Best Energy:", (best + 1) - i, "\n","Energy Output",
                              bestrestGA[,"EnergyOverall"][[1]],"kW", "\n", "Efficiency:",
                              bestrestGA[,"EfficAllDir"][[1]]))
    raster::plot(Grid, add = TRUE)
    graphics::mtext("Total Wake Effect in %", side = 2)
    graphics::points(bestrestGA[,"X"],bestrestGA[,"Y"],
                     cex = 2, pch = 20, col = ColOri)
    graphics::text(bestrestGA[,"X"], bestrestGA[,"Y"],
                   round(bestrestGA[,"AbschGesamt"],0),
                   cex = 0.8, pos = 1, col = "black")
    distpo <- stats::dist(x = cbind(bestrestGA[,"X"], bestrestGA[,"Y"]),
                          method = "euclidian")
    graphics::mtext(paste("minimal Distance", round(min(distpo), 2)),
                    side = 1, line = 0)
    graphics::mtext(paste("mean Distance", round(mean(distpo), 2)),
                    side = 1, line = 1)
    ################

    ## Random Search Ouput  ################
    EnergyBest <- data.frame(resultRS1[[i]])
    ## Assign the colour depending on the individual wind speed 
    br <- length(levels(factor(EnergyBest[,'AbschGesamt'])))
    if (br > 1) {
      Col <- rbPal1(br)[as.numeric(cut(as.numeric(
        EnergyBest[, 'AbschGesamt']), breaks = br))]
    } else {
      Col <- "green"
    }

    EnergyBest[,'EnergyOverall'] <- round(EnergyBest[,'EnergyOverall'], 2)
    EnergyBest[,'EfficAllDir'] <- round(EnergyBest[,'EfficAllDir'], 2)
    raster::plot(Polygon1, col = col2res,
                 main = paste("Random Search - Best Energy:", (best + 1) - i, 
                              "\n","Energy Output",
                              EnergyBest[,'EnergyOverall'][[1]],"kW", "\n", "Efficiency:",
                              EnergyBest[,'EfficAllDir'][[1]]))
    
    raster::plot(Grid, add = TRUE)
    graphics::mtext("Total Wake Effect in %", side = 2)
    graphics::points(EnergyBest[,'X'],EnergyBest[,'Y'],
                     cex = 2, pch = 20, col = Col)
    graphics::text(EnergyBest[,'X'], EnergyBest[,'Y'],
                   round(EnergyBest[,'AbschGesamt'], 0),
                   cex = 0.8, pos = 1, col = "black")
    distpo <- stats::dist(x = cbind(EnergyBest[,'X'],EnergyBest[,'Y']),
                          method = "euclidian")
    graphics::mtext(paste("minimal Distance", round(min(distpo), 2)),
                    side = 1, line = 0)
    graphics::mtext(paste("mean Distance", round(mean(distpo), 2)),
                    side = 1, line = 1)
    ################
  }


  par(op)
  invisible()
}


