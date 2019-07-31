#' @title Plot the results of an optimization run
#' @name plot_windfarmGA
#' @description  Plot the results of a genetic algorithm run with given inputs.
#'   Several plots try to show all relevant effects and outcomes of the
#'   algorithm. 6 plot methods are available that can be selected individually.
#'
#' @export
#'
#' @param result The output of \code{\link{windfarmGA}} or
#'   \code{\link{genetic_algorithm}}
#' @param Polygon1 The area as shapefile.
#' @param GridMethod Which grid spacing method was used. Default is
#'   "rectangular". If hexagonal grid cells were used, assign any of the
#'   following arguments: "h","hexa", "hexagonal"
#' @param whichPl Which plots should be shown: 1-6 are possible. The default is
#'   "all" which shows all available plots
#' @param best A numeric value indicating how many of the best individuals
#'   should be plotted
#' @param plotEn A numeric value that indicates if the best energy or efficiency
#'   output is plotted. If (plotEn==1) plots the best energy solutions
#'   and (plotEn==2) plots the best efficiency solutions
#' @param Projection A desired Projection can be used instead of the default
#'   Lambert Azimuthal Equal Area Projection
#' @param weibullsrc A list of Weibull parameter rasters, where the first list
#'   item must be the shape parameter raster `k` and the second item must be the
#'   scale parameter raster `a` of the Weibull distribution. If no list is given,
#'   then rasters included in the package are used instead, which currently only
#'   cover Austria.
#'
#' @family Plotting Functions
#' @return NULL
#' @examples \donttest{
#' library(sp)
#' ## Add some data examples from the package
#' load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
#'
#' ## Plot the results of a hexagonal grid optimization
#' plot_windfarmGA(resulthex, GridMethod = "h", polygon, whichPl = "all", best = 1, plotEn = 1)
#'
#' ## Plot the results of a rectangular grid optimization
#' plot_windfarmGA(resultrect, GridMethod = "r", polygon, whichPl = "all", best = 1, plotEn = 1)
#' }
plot_windfarmGA <- function(result, Polygon1, GridMethod = "r",
                           whichPl = "all", best = 1, plotEn = 1, 
                           Projection, weibullsrc){

  parpplotWindGa <- par(ask = FALSE, no.readonly = TRUE)
  on.exit(par(parpplotWindGa))

  ## DATA #################
  if (any(whichPl == "all")) {
    whichPl <- 1:6
  }
  resol <- as.numeric(result[, 'inputData'][[1]][,1]['Resolution'][[1]])
  prop <- as.numeric(result[, 'inputData'][[1]][,1]['Percentage of Polygon'][[1]])
  Polygon1 <- isSpatial(Polygon1, Projection)
  GridMethod <- toupper(GridMethod)
  if (GridMethod == "HEXAGONAL" | GridMethod == "H" | GridMethod == "HEXA") {
    Grid <- hexa_area(Polygon1 = Polygon1, size = resol / 2, plotTrue = FALSE)
  } else {
    Grid <- grid_area(shape = Polygon1,resol = resol, prop = prop, plotGrid = FALSE)
  }
  if (nrow(result) < 4) {
    if (any(2:5 %in% whichPl)) {
      cat("Cannot plot option 2,3,4,5. \n Only option 1,6 are available.")
      whichPl <- c(1, 6)
    }
  }
  #################

  ## PLOTTING OUTPUTS ####################
  if (any(whichPl == 1)) {
    print("plot_result: Plot the 'best' Individuals of the GA:")
    plot_result(result = result, Polygon1 = Polygon1, best = best , plotEn = plotEn,
               topographie = FALSE, Grid = Grid[[2]], weibullsrc = weibullsrc)
    readline(prompt = "Press [enter] to continue")
  }
  if (any(whichPl == 2)) {
    print("plot_evolution: Plot the Evolution of the Efficiency and Energy Values:")
    plot_evolution(result, TRUE, 0.3)
  }
  if (any(whichPl == 3)) {
    print("plot_parkfitness: Plot the Influence of Population Size, Selection, Crossover, Mutation:")
    plot_parkfitness(result, 0.1)
    readline(prompt = "Press [enter] to continue")
  }
  if (any(whichPl == 4)) {
    print("plot_fitness_evolution: Plot the Changes in Fitness Values:")
    plot_fitness_evolution(result)
    readline(prompt = "Press [enter] to continue")
  }
  if (any(whichPl == 5)) {
    print("plot_cloud: Plot all individual Values of the whole Evolution:")
    plot_cloud(result, TRUE)
    readline(prompt = "Press [enter] to continue")
  }
  if (any(whichPl == 6)) {
    print("plot_heatmap: Plot a Heatmap of all Grid Cells:")
    plot_heatmap(result = result, si = 5)
    # readline(prompt = "Press [enter] to continue")
  }
  # if (any(whichPl==7)){
  #   print("GooglePlot: Plot the 'best' Individual with static Google Map:")
  #   GooglePlot(result,Polygon1,best,plotEn,Projection)
  #   readline(prompt="Press [enter] to continue")
  # }
  # if (any(whichPl==8)){
  #   print("GoogleChromePlot: Plot the 'best' Individual with Leaflet with Satelitte Imagery:")
  #   GoogleChromePlot(result,Polygon1,best,plotEn,Projection)
  # }
  return()
}
