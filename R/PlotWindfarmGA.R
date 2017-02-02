#' @title Plot the results of an optimization run
#' @name PlotWindfarmGA
#' @description  Plot the results of a genetic algorithm run with given inputs.
#' Several plots try to show all relevant influences and behaviours of the
#' algorithm. 8 plot methods are available that can be selected individually.
#'
#' @export
#'
#'
#' @param result An output matrix of the function \code{\link{windfarmGA}},
#' which has stored all relevant information. (matrix)
#' @param Polygon1 The considered area as shapefile. Only required, if the
#' shapefile is already loaded.(SpatialPolygons)
#' @param whichPl Which Plots should be shown 1-8 are possible. If "all" is
#' given, then all plots will be shown.
#' @param best A numeric value indicating how many of the best individuals
#' should be plotted. (numeric)
#' @param plotEn A numeric value that indicates if the best energy or
#' efficiency output should be plotted. If (plotEn==1) plots the best energy
#' solutions and (plotEn==2) plots the best efficiency solutions. (numeric)
#' @param Projection A desired Projection can be used instead
#' of the default Lambert Azimuthal Equal Area Projection. (character)
#'
#' @return NULL
#'
#' @author Sebastian Gatscha

PlotWindfarmGA <- function(result,Polygon1,whichPl,best=1,plotEn=1,Projection){
  # result=resultConfGa1
  op <- par(no.readonly = T)

  if (any(whichPl=="all")){
    whichPl <- c(1,2,3,4,5,6,7,8)
  }

  resol = as.numeric(result[,'inputData'][[1]][,1]['Resolution'][[1]])
  prop = as.numeric(result[,'inputData'][[1]][,1]['Percentage of Polygon'][[1]])

  Grid <- GridFilter(shape = Polygon1,resol = resol, prop = prop ,plotGrid=TRUE)

  if (nrow(result)<4) {
    print("Cannot plot option 2,3,4,5. \n Only option 1,6,7 are available.")
    whichPl <- c(1,6,7)
  }

  ############### PLOTTING OUTPUTS
  if (any(whichPl==1)){
    print("Plots the Best Individual of the GA:")
    plotResult(result = result, Polygon1 = Polygon1, best = best ,plotEn = plotEn,topographie = FALSE,Grid= Grid[[2]]);
    readline(prompt="Press [enter] to continue")
  }
  if (any(whichPl==2)){
    print("Plots the Evolution of the Efficiency and Energy Values:")
    plotEvolution(result,T,0.3)
    readline(prompt="Press [enter] to continue")
  }
  if (any(whichPl==3)){
    print("Show the Influences of Population Size, Selection, Crossover, Mutation")
    plotparkfitness(result,0.1)
    readline(prompt="Press [enter] to continue")
  }
  if (any(whichPl==4)){
    print("Plots the Best Individual of the GA:")
    plotfitnessevolution(result)
    readline(prompt="Press [enter] to continue")

  }
  if (any(whichPl==5)){
    print("Plot all Individual Values of the whole run:")
    plotCloud(result,"TRUE")
    readline(prompt="Press [enter] to continue")

  }
  if (any(whichPl==6)){
    print("Plot the Best Individual with static Google Map Background:")
    GooglePlot(result,Polygon1,Projection)
    print("Plots the Best Individual in Google Chrome with Satelitte Imagery:")
    GoogleChromePlot(result,Polygon1,1,1,Projection)
    readline(prompt="Press [enter] to continue")
  }
  if (any(whichPl==7)){
    print("Plots a Heatmap of all Grid Cells:")
    heatmapGA(result,si=5,Polygon1 = Polygon1)
  }
  par(op)

}


