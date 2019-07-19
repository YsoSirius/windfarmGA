#' Deprecated use \code{\link[windfarmGA]{selection}} instead.
#' @description selection1 is replaced by \code{\link[windfarmGA]{selection}}.
#' @inheritParams selection
#' @keywords internal
#' @export
selection1 <- function(fit, Grid, teil, elitism, nelit, 
                       selstate, verbose) {
  .Deprecated(
    new = "selection",
    msg = "selection1 will be replaced by selection"
  )
  selection(fit, Grid, teil, elitism, nelit, selstate, verbose)
}

#' Deprecated use \code{\link[windfarmGA]{crossover}} instead.
#' @description crossover1 is replaced by \code{\link[windfarmGA]{crossover}}.
#' @inheritParams crossover
#' @keywords internal
#' @export
crossover1 <- function(se6, u, uplimit, crossPart, verbose, seed) {
  .Deprecated(
    new = "crossover",
    msg = "crossover1 will be replaced by crossover"
  )
  crossover(se6, u, uplimit, crossPart, verbose, seed)
}

#' Deprecated use \code{\link[windfarmGA]{get_dist_angles}} instead.
#' @description VekWinkelCalc is replaced by
#'   \code{\link[windfarmGA]{get_dist_angles}}.
#' @inheritParams get_dist_angles
#' @keywords internal
#' @export
VekWinkelCalc <- function(t, o, wkl, distanz, polYgon, plotAngles) {
  .Deprecated(
    new = "get_dist_angles",
    msg = "VekWinkelCalc will be replaced by get_dist_angles"
  )
  get_dist_angles(t, o, wkl, distanz, polYgon, plotAngles)
}

#' Deprecated use \code{\link[windfarmGA]{calculate_energy}} instead.
#' @description calculateEn is replaced by
#'   \code{\link[windfarmGA]{calculate_energy}}.
#' @inheritParams calculate_energy
#' @keywords internal
#' @export
calculateEn <- function(sel, referenceHeight, RotorHeight,
                        SurfaceRoughness, wnkl, distanz,
                        polygon1, resol, RotorR, dirSpeed,
                        srtm_crop, topograp, cclRaster, weibull,
                        plotit = FALSE) {
  .Deprecated(
    new = "calculate_energy",
    msg = "calculateEn will be replaced by calculate_energy"
  )
  calculate_energy(sel, referenceHeight, RotorHeight,
                  SurfaceRoughness, wnkl, distanz,
                  polygon1, resol, RotorR, dirSpeed,
                  srtm_crop, topograp, cclRaster, weibull,
                  plotit)
}

#' Deprecated use \code{\link[windfarmGA]{get_grids}} instead.
#' @description getRects is replaced by \code{\link[windfarmGA]{get_grids}}.
#' @inheritParams get_grids
#' @keywords internal
#' @export
getRects <- function(trimtonOut, Grid) {
  .Deprecated(
    new = "get_grids",
    msg = "getRects will be replaced by get_grids"
  )
  get_grids(trimtonOut, Grid)
}

#' Deprecated use \code{\link[windfarmGA]{barometric_height}} instead.
#' @description BaroHoehe is replaced by
#'   \code{\link[windfarmGA]{barometric_height}}.
#' @inheritParams barometric_height
#' @keywords internal
#' @export
BaroHoehe <- function(data, height, po = 101325, ro = 1.225) {
  .Deprecated(
    new = "barometric_height",
    msg = "BaroHoehe will be replaced by barometric_height"
  )
  barometric_height(data, height, po, ro)
}

#' Deprecated use \code{\link[windfarmGA]{grid_area}} instead.
#' @description GridFilter is replaced by \code{\link[windfarmGA]{grid_area}}.
#' @inheritParams grid_area
#' @keywords internal
#' @export
GridFilter <- function(shape, resol = 500, prop = 1, plotGrid = FALSE) {
  .Deprecated(
    new = "grid_area",
    msg = "GridFilter will be replaced by grid_area"
  )
  grid_area(shape, resol, prop, plotGrid)
}

#' Deprecated use \code{\link[windfarmGA]{init_population}} instead.
#' @description StartGA is replaced by
#'   \code{\link[windfarmGA]{init_population}}.
#' @inheritParams init_population
#' @keywords internal
#' @export
StartGA <- function(Grid, n, nStart = 100) {
  .Deprecated(
    new = "init_population",
    msg = "StartGA will be replaced by init_population"
  )
  init_population(Grid, n, nStart)
}

#' Deprecated use \code{\link[windfarmGA]{turbine_influences}} instead.
#' @description InfluPoints is replaced by
#'   \code{\link[windfarmGA]{turbine_influences}}.
#' @inheritParams turbine_influences
#' @keywords internal
#' @export
InfluPoints <- function(t, wnkl, dist, polYgon, dirct,
                        plotAngles = FALSE) {
  .Deprecated(
    new = "turbine_influences",
    msg = "InfluPoints will be replaced by turbine_influences"
  )
  turbine_influences(t, wnkl, dist, polYgon, dirct, plotAngles)
}

#' Deprecated use \code{\link[windfarmGA]{genetic_algorithm}} instead.
#' @description genAlgo is replaced by
#'   \code{\link[windfarmGA]{genetic_algorithm}}.
#' @inheritParams genetic_algorithm
#' @keywords internal
#' @export
genAlgo <- function(Polygon1, GridMethod, Rotor, n, fcrR, referenceHeight,
                    RotorHeight, SurfaceRoughness, Proportionality,
                    iteration, mutr, vdirspe, topograp, elitism, nelit,
                    selstate, crossPart1, trimForce, Projection,
                    sourceCCL, sourceCCLRoughness, weibull, weibullsrc,
                    Parallel, numCluster, verbose = FALSE, plotit = FALSE) {
  .Deprecated(
    new = "genetic_algorithm",
    msg = "genAlgo will be replaced by genetic_algorithm"
  )
  genetic_algorithm(Polygon1, GridMethod, Rotor, n, fcrR, referenceHeight,
                     RotorHeight, SurfaceRoughness, Proportionality,
                     iteration, mutr, vdirspe, topograp, elitism, nelit,
                     selstate, crossPart1, trimForce, Projection,
                     sourceCCL, sourceCCLRoughness, weibull, weibullsrc,
                     Parallel, numCluster, verbose, plotit)
}

#' Deprecated use \code{\link[windfarmGA]{random_search}} instead.
#' @description RandomSearch is replaced by
#'   \code{\link[windfarmGA]{random_search}}.
#' @inheritParams random_search
#' @keywords internal
#' @export
RandomSearch <- function(result, Polygon1, n, best, Plot,
                         GridMethod, max_dist = 2.2) {
  .Deprecated(
    new = "random_search",
    msg = "RandomSearch will be replaced by random_search"
  )
  random_search(result, Polygon1, n, best, Plot, GridMethod, max_dist)
}

#' Deprecated use \code{\link[windfarmGA]{random_search_single}} instead.
#' @description RandomSearchTurb is replaced by
#'   \code{\link[windfarmGA]{random_search_single}}.
#' @inheritParams random_search_single
#' @keywords internal
#' @export
RandomSearchTurb <- function(result, Polygon1, n, Plot, GridMethod,
                             max_dist = 2.2) {
  .Deprecated(
    new = "random_search_single",
    msg = "RandomSearchTurb will be replaced by random_search_single"
  )
  random_search_single(result, Polygon1, n, Plot, GridMethod, max_dist)
}

#' Deprecated use \code{\link[windfarmGA]{plot_random_search}} instead.
#' @description RandomSearchPlot is replaced by
#'   \code{\link[windfarmGA]{plot_random_search}}.
#' @inheritParams plot_random_search
#' @keywords internal
#' @export
RandomSearchPlot <- function(resultRS, result, Polygon1, best) {
  .Deprecated(
    new = "plot_random_search",
    msg = "RandomSearchPlot will be replaced by plot_random_search"
  )
  plot_random_search(resultRS, result, Polygon1, best)
}

#' Deprecated use \code{\link[windfarmGA]{plot_leaflet}} instead.
#' @description leafPlot is replaced by \code{\link[windfarmGA]{plot_leaflet}}.
#' @inheritParams plot_leaflet
#' @keywords internal
#' @export
leafPlot <- function(result, Polygon1, which = 1, orderitems = TRUE, GridPol) {
  .Deprecated(
    new = "plot_leaflet",
    msg = "leafPlot will be replaced by plot_leaflet"
  )
  plot_leaflet(result, Polygon1, which, orderitems, GridPol)
}

#' Deprecated use \code{\link[windfarmGA]{plot_heatmap}} instead.
#' @description heatmapGA is replaced by \code{\link[windfarmGA]{plot_heatmap}}.
#' @inheritParams plot_heatmap
#' @keywords internal
#' @export
heatmapGA <- function(result, si = 2, idistw) {
  .Deprecated(
    new = "plot_heatmap",
    msg = "heatmapGA will be replaced by plot_heatmap"
  )
  plot_heatmap(result, si, idistw)
}

#' Deprecated use \code{\link[windfarmGA]{hexa_area}} instead.
#' @description HexaTex is replaced by \code{\link[windfarmGA]{hexa_area}}.
#' @inheritParams hexa_area
#' @keywords internal
#' @export
HexaTex <- function(Polygon1, size, plotTrue = FALSE) {
  .Deprecated(
    new = "hexa_area",
    msg = "HexaTex will be replaced by hexa_area"
  )
  hexa_area(Polygon1, size, plotTrue)
}

#' Deprecated use \code{\link[windfarmGA]{plot_development}} instead.
#' @description plotbeorwor is replaced by
#'   \code{\link[windfarmGA]{plot_development}}.
#' @inheritParams plot_development
#' @keywords internal
#' @export
plotbeorwor <- function(result) {
  .Deprecated(
    new = "plot_development",
    msg = "plotbeorwor will be replaced by plot_development"
  )
  plot_development(result)
}

#' Deprecated use \code{\link[windfarmGA]{plot_cloud}} instead.
#' @description plotCloud is replaced by \code{\link[windfarmGA]{plot_cloud}}.
#' @inheritParams plot_cloud
#' @keywords internal
#' @export
plotCloud <- function(result, pl = FALSE) {
  .Deprecated(
    new = "plot_cloud",
    msg = "plotCloud will be replaced by plot_cloud"
  )
  plot_cloud(result, pl)
}

#' Deprecated use \code{\link[windfarmGA]{plot_evolution}} instead.
#' @description plotEvolution is replaced by
#'   \code{\link[windfarmGA]{plot_evolution}}.
#' @inheritParams plot_evolution
#' @keywords internal
#' @export
plotEvolution <- function(result, ask = TRUE, spar = 0.1) {
  .Deprecated(
    new = "plot_evolution",
    msg = "plotEvolution will be replaced by plot_evolution"
  )
  plot_evolution(result, ask, spar)
}

#' Deprecated use \code{\link[windfarmGA]{plot_fitness_evolution}} instead.
#' @description plotfitnessevolution is replaced by
#'   \code{\link[windfarmGA]{plot_fitness_evolution}}.
#' @inheritParams plot_fitness_evolution
#' @keywords internal
#' @export
plotfitnessevolution <- function(result, spar = 0.1) {
  .Deprecated(
    new = "plot_fitness_evolution",
    msg = "plotfitnessevolution will be replaced by plot_fitness_evolution"
  )
  plot_fitness_evolution(result, spar)
}

#' Deprecated use \code{\link[windfarmGA]{plot_parkfitness}} instead.
#' @description plotparkfitness is replaced by
#'   \code{\link[windfarmGA]{plot_parkfitness}}.
#' @inheritParams plot_parkfitness
#' @keywords internal
#' @export
plotparkfitness <- function(result, spar = 0.1) {
  .Deprecated(
    new = "plot_parkfitness",
    msg = "plotparkfitness will be replaced by plot_parkfitness"
  )
  plot_parkfitness(result, spar)
}

#' Deprecated use \code{\link[windfarmGA]{plot_result}} instead.
#' @description plotResult is replaced by
#'   \code{\link[windfarmGA]{plot_result}}.
#' @inheritParams plot_result
#' @keywords internal
#' @export
plotResult <- function(result, Polygon1, best = 3, plotEn = 1,
                       topographie = FALSE, Grid, Projection,
                       sourceCCLRoughness, sourceCCL,
                       weibullsrc) {
  .Deprecated(
    new = "plot_result",
    msg = "plotResult will be replaced by plot_result"
  )
  plot_result(result, Polygon1, best, plotEn,
               topographie, Grid, Projection,
               sourceCCLRoughness, sourceCCL,
               weibullsrc)
}

#' Deprecated use \code{\link[windfarmGA]{plot_windfarmGA}} instead.
#' @description PlotWindfarmGA is replaced by
#'   \code{\link[windfarmGA]{plot_windfarmGA}}.
#' @inheritParams plot_windfarmGA
#' @keywords internal
#' @export
PlotWindfarmGA <- function(result, Polygon1, GridMethod = "r",
                           whichPl = "all", best = 1, plotEn = 1, 
                           Projection, weibullsrc) {
  .Deprecated(
    new = "plot_windfarmGA",
    msg = "PlotWindfarmGA will be replaced by plot_windfarmGA"
  )
  plot_windfarmGA(result, Polygon1, GridMethod, whichPl, best, plotEn, 
                  Projection, weibullsrc)
}

#' Deprecated use \code{\link[windfarmGA]{plot_windrose}} instead.
#' @description plotWindrose is replaced by
#'   \code{\link[windfarmGA]{plot_windrose}}.
#' @inheritParams plot_windrose
#' @keywords internal
#' @export
plotWindrose <- function(data, spd, dir, spdres = 2, dirres = 10, spdmin = 1,
                         spdmax = 30, palette = "YlGnBu",
                         spdseq = NULL, plotit = TRUE) {
  .Deprecated(
    new = "plot_windrose",
    msg = "plotWindrose will be replaced by plot_windrose"
  )
  plot_windrose(data, spd, dir, spdres, dirres, spdmin,
                spdmax, palette, spdseq, plotit)
}