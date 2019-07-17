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

#' Deprecated use \code{\link[windfarmGA]{crossover1}} instead.
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

#' Deprecated use \code{\link[windfarmGA]{VekWinkelCalc}} instead.
#' @description VekWinkelCalc is replaced by \code{\link[windfarmGA]{get_dist_angles}}.
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

#' Deprecated use \code{\link[windfarmGA]{calculateEn}} instead.
#' @description calculateEn is replaced by \code{\link[windfarmGA]{calculate_energy}}.
#' @inheritParams calculate_energy
#' @keywords internal
#' @export
calculateEn <- function(sel, referenceHeight, RotorHeight,
                        SurfaceRoughness, wnkl, distanz,
                        polygon1, resol, RotorR, dirSpeed,
                        srtm_crop, topograp, cclRaster, weibull) {
  .Deprecated(
    new = "calculate_energy",
    msg = "calculateEn will be replaced by calculate_energy"
  )
  calculate_energy(sel, referenceHeight, RotorHeight,
                  SurfaceRoughness, wnkl, distanz,
                  polygon1, resol, RotorR, dirSpeed,
                  srtm_crop, topograp, cclRaster, weibull)
}

#' Deprecated use \code{\link[windfarmGA]{getRects}} instead.
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

#' Deprecated use \code{\link[windfarmGA]{BaroHoehe}} instead.
#' @description BaroHoehe is replaced by \code{\link[windfarmGA]{barometric_height}}.
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

#' Deprecated use \code{\link[windfarmGA]{GridFilter}} instead.
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

#' Deprecated use \code{\link[windfarmGA]{StartGA}} instead.
#' @description StartGA is replaced by \code{\link[windfarmGA]{init_population}}.
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


#' Deprecated use \code{\link[windfarmGA]{InfluPoints}} instead.
#' @description InfluPoints is replaced by \code{\link[windfarmGA]{turbine_influences}}.
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