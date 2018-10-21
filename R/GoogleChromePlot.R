#' @title Plot the Best Results in Google Chrome
#' @name GoogleChromePlot
#' @description  Plot the best energy or efficiency solutions in Google Chrome
#' with a satellite background image of Google maps. Input Polygon is
#' not plotted
#'
#' @export
#'
#' @importFrom googleVis gvisMap
#'
#' @param result The output matrix of \code{\link{windfarmGA}} or
#' \code{\link{genAlgo}}, which has stored all relevant information.
#' (matrix)
#' @param Polygon1 The considered area as shapefile (SpatialPolygons)
#' @param best A numeric value indicating the best individual
#' to be plotted. 1 will indicate the solution with highest value. (numeric)
#' @param plotEn A numeric value that indicates if the best energy or
#' efficiency output should be plotted. (plotEn==1) will plot the best
#' energy solution and (plotEn==2) will plot the best efficiency solution.
#' (numeric)
#' @param Projection A desired Projection can be used instead
#' of the default Lambert Azimuthal Equal Area Projection. (character)
#'
#' @return NULL
#' @examples \donttest{
#' ## Add some data examples from the package
#' load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
#'
#' ## Plot the results of a wind farm optimization
#' result <- resultrect
#' Polygon1 <- polygon
#' GoogleChromePlot(result, Polygon1, 1, 1)
#' }
#' @author Sebastian Gatscha
GoogleChromePlot <- function(result,Polygon1,best=1,plotEn=1,Projection) {

  a <- GooglePlot(result,Polygon1,best,plotEn,Projection,plT=F)
  a$latlon <- paste(a$lat, a$lon, sep=":")
  
  a$latlonTip  <- paste(
    "<div><ins><b>Energy Data:</b></ins></div>",
    paste("Efficiency in %", round(a$'Efficiency in %',3), sep=": "), 
    paste("Energy in kW/h", round(a$'Energy in kW/h',3), sep=": "), 
    paste("Wake Influence in %", round(a$'Wake Influence in %',3), sep=": "), 
    "<br><div><ins><b>Location:</b></ins></div>",
    paste("Longitude", a$lon, sep=": "), 
    paste("Latitude", a$lat, sep=": "),
    sep="<br>")
  
  map.gb <- googleVis::gvisMap(a, locationvar="latlon", chartid = "Map", tipvar = "latlonTip",
                               options = list(showTip=T, 
                                              showLine=F, 
                                              enableScrollWheel=TRUE,
                                              mapType="hybrid",
                                              useMapTypeControl=T, 
                                              width=1400,
                                              height=800,
                                              icons=paste0("{","'default': {'normal': 'http://icons.iconarchive.com/",
                                                           "icons/icons-land/weather/48/",
                                                           "Wind-Flag-Storm-icon.png',\n",
                                                           "'selected': 'http://icons.iconarchive.com/",
                                                           "icons/icons-land/weather/64/",
                                                           "Wind-Flag-Storm-icon.png'","}}")
                               )
  )
  plot(map.gb)
}
