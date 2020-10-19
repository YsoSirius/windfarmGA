#' @title Sample values from a raster
#' @name rasterprofile
#' @description Sample a raster along a straight line between 2 points
#'
#' @export
#'
#' @param r A DEM raster
#' @param xy1 A matrix with X and Y coordinates for Point 1
#' @param xy2 A matrix with X and Y coordinates for Points 2
#' @param reso The minimal resolution of the DEM raster. It is 
#'   calculated in \code{viewshed} and passed along.
#' @param plot Plot the process? Default is FALSE
#'
#' @family Viewshed Analysis
#' @return A boolean vector, indicating if Point 1 (xy1) is visible from all
#'   elements of Points 2 (xy2)
#'
rasterprofile <- function(r, xy1, xy2, reso, plot=FALSE){
  if (plot) {
    plot(r)
    points(x = xy2[1], y = xy2[2], col = "blue", pch = 20, cex = 1.4)
    points(x = xy1[1], y = xy1[2], col = "red", pch = 20, cex = 2)
  }
  
  ### sample a raster along a straight line between two points
  ### try to match the sampling size to the raster resolution
  dx = sqrt( (xy1[1] - xy2[1])^2 + (xy1[2] - xy2[2])^2 )
  nsteps = 1 + round(dx / reso)
  xc = xy1[1] + (0:nsteps) * (xy2[1] - xy1[1]) / nsteps
  yc = xy1[2] + (0:nsteps) * (xy2[2] - xy1[2]) / nsteps

  if (plot) {
    points(x = xc, y = yc, col = "red", pch = 20, cex = 1.4)
  }
  
  # rasterVals <- r[raster::cellFromXY(r, cbind(xc,yc))]
  rasterVals <- raster::extract(x = r, y = cbind(xc,yc))
  
  pointsZ <- cbind("x" = xc, "y" = yc, "z" = rasterVals)
  
  if (plot) {
    points(pointsZ[,"x"], pointsZ[,"y"], pch = 20, col = "black")
    text(pointsZ[,"x"], pointsZ[,"y"], pos = 1, pointsZ[,"z"], cex = 0.5)
  }
  
  if (anyNA(pointsZ)) {
    pointsZ <- pointsZ[stats::complete.cases(pointsZ),,drop = FALSE]
  }
  return(pointsZ)
}

#' @title Calculate Visibility between 2 locations
#' @name cansee
#' @description Check if point 1 is visible from point 2 given
#'   a certain elevation model
#'
#' @export
#'
#' @param r A DEM raster
#' @param xy1 A vector/matrix with X and Y coordinates for Point 1
#' @param xy2 A vector/matrix with X and Y coordinates for Point 2
#' @param h1 A numeric giving the extra height offset for Point 1
#' @param h2 A numeric giving the extra height offset for Point 2
#' @param reso The minimal resolution of the DEM raster. It is 
#'   calculated in \code{viewshed} and passed along.
#'
#' @family Viewshed Analysis
#' @return A boolean value, indicating if the point (xy2) is visible
#'
cansee <- function(r, xy1, xy2, h1=0, h2=0, reso){
  ### can xy1 see xy2 on DEM r?
  ### r is a DEM in same x,y, z units
  ### xy1 and xy2 are 2-length vectors of x,y coords
  ### h1 and h2 are extra height offsets
  xyz = rasterprofile(r, xy1, xy2, reso)
  np = length(xyz[, 1]) - 1
  h1 = xyz[, "z"][1] + h1
  h2 = xyz[, "z"][np] + h2
  hpath = h1 + (0:np)*(h2 - h1)/np
  invisible(!any(hpath < xyz[, "z"], na.rm = TRUE))
}


#' @title Calculate Visibility between multiple locations
#' @name viewTo
#' @description Check if a location is visible from multiple locations
#'
#' @export
#'
#' @inheritParams cansee
#' 
#' @family Viewshed Analysis
#' @return A boolean vector, indicating if \code{xy1} is visible from all
#'   elements of \code{xy2}
#'
viewTo <- function(r, xy1, xy2, h1=0, h2=0, reso){
  a <- t(apply(xy2, 1, function(d){
    cansee(r[[1]], xy1 = xy1, xy2 = d, h1, h2, reso)}))
  a[is.na(a)] <- FALSE
  return(as.vector(a))
}


#' @title Calculate visibility
#' @name viewshed
#' @description Calculate visibility for given points in a given area.
#'
#' @export
#'
#' @param r A DEM raster
#' @param shape A SpatialPolygon of the windfarm area.
#' @param turbine_locs Coordinates or SpatialPoint representing the wind
#'   turbines
#' @param h1 A numeric giving the extra height offset of Point 1
#' @param h2 A numeric giving the extra height offset of Point 2
#'
#' @family Viewshed Analysis
#' @return A list of 5, containing the boolean result for every cell, the raster
#'   cell points, a SimpleFeature Polygon of the given area and the DEM raster
#' 
#' @examples \dontrun{
#' library(sp)
#' Polygon1 <- Polygon(rbind(c(4488182, 2667172), c(4488182, 2669343),
#'                           c(4499991, 2669343), c(4499991, 2667172)))
#' Polygon1 <- Polygons(list(Polygon1), 1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+init=epsg:3035"
#' proj4string(Polygon1) <- CRS(Projection)
#' DEM_meter <- getDEM(Polygon1)
#' 
#' turbloc = spsample(DEM_meter[[2]], 10, type = "random");
#' res <- viewshed(r = DEM_meter[[1]], shape=DEM_meter[[2]], turbine_locs = turbloc,  h1=1.8, h2=50)
#' }
viewshed <- function(r, shape, turbine_locs, h1=0, h2=0){
  if (class(shape)[1] == "sf") {
    shape <- as(shape, "Spatial")  
  }
  if (class(turbine_locs)[1] == "SpatialPoints") {
    turbine_locs <- sp::coordinates(turbine_locs)
  }
  
  mw <- methods::as(r, "SpatialPixelsDataFrame")
  mw <- methods::as(mw, "SpatialPolygons")
  sample_xy <- sp::coordinates(mw)
  rownames(sample_xy) <- NULL; colnames(sample_xy) <- c("x1","x2")

  ## Get minimal Raster Resolution
  reso <- min(raster::res(r))
  
  ## xy2 is a matrix of x,y coords
  res <- t(apply(turbine_locs, 1, function(d){
    viewTo(r, xy1 = d, xy2 = sample_xy, h1, h2, reso )
  }))
  
  return(list("Result" = res, 
              "Raster_POI" = sample_xy, 
              "Area" = sf::st_as_sf(shape), 
              "DEM" = r, 
              "Turbines" = turbine_locs))
}

## Geht noch nicht
# viewshed_par <- function(r, shape, turbine_locs, h1=0, h2=0, progress="none"){
#   # r = DEM_meter; shape=shape_meter; turbine_locs = turbloc
#   # h1=0; h2=0;
#   
#   if (class(shape)[1] == "sf") {
#     shape <- as(shape, "Spatial")  
#   }
#   if (class(turbine_locs) == "SpatialPoints") {
#     turbine_locs = sp::coordinates(turbine_locs)
#   }
#   
#   sample_POI <- sp::spsample(shape, n = raster::ncell(r), type = "regular")
#   sample_xy <- sp::coordinates(sample_POI)
#   
#   
#   library(parallel)
#   nCore <- parallel::detectCores()
#   cl <- parallel::makeCluster(nCore)
#   parallel::clusterEvalQ(cl, {
#     library(plyr)
#     library(raster)
#   })
#   parallel::clusterExport(cl, varlist = c("turbine_locs", "sample_xy", 
#                                 "viewTo", "cansee", "rasterprofile", 
#                                 "r", "h1", "h2", "progress"))
#   
#   res <- parallel::parApply(cl = cl, X = turbine_locs, 1, function(d){
#     viewTo(r, xy1 = d, xy2 = sample_xy, h1, h2, progress)
#   })
#   res <- t(res)
#   
#   parallel::stopCluster(cl)
#   
#   if (is.matrix(res)) {
#     res <- res[1:nrow(res),1:nrow(sample_xy)]
#   }
#   if (is.logical(res)) {
#     res[1:nrow(sample_xy)]
#   }
#   
#   return(list("Result"=res, "Raster_POI" = sample_xy, 
#               "Area" = sf::st_as_sf(shape), "DEM" = r, "Turbines" = turbine_locs))
# }
# res <- viewshed_par(r = DEM_meter, shape=shape_meter, turbine_locs = turbloc,  h1=1.8, h2=50)


#' @title Plot viewshed results
#' @name plot_viewshed
#' @description Plot the result of \code{\link{viewshed}}
#'
#' @export
#'
#' @param res The resulting list from viewshed
#' @param legend Plot a legend? Default is FALSE
#' @param ... Is passed along to \code{\link[raster]{plot}}
#'
#' @family Viewshed Analysis
#' @family Plotting Functions
#'
#' @return NULL
#' @examples \dontrun{
#' library(sp)
#' library(raster)
#' Polygon1 <- Polygon(rbind(c(4488182, 2667172), c(4488182, 2669343),
#'                           c(4499991, 2669343), c(4499991, 2667172)))
#' Polygon1 <- Polygons(list(Polygon1), 1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+init=epsg:3035"
#' proj4string(Polygon1) <- CRS(Projection)
#' DEM_meter <- getDEM(Polygon1)
#' 
#' turbloc = spsample(DEM_meter[[2]], 10, type = "random");
#' res <- viewshed(r = DEM_meter[[1]], shape = DEM_meter[[2]], turbine_locs = turbloc,  
#'                 h1 = 1.8, h2 = 50)
#' plot_viewshed(res)
#' 
#' ## ... Arguments are past on to raster::plot
#' plot_viewshed(res, legend = TRUE, interpolate=TRUE, colNA="black", 
#'               col = topo.colors(15))
#' }
plot_viewshed <- function(res, legend=FALSE, ...) {
  raster::plot(res[[4]], ...)
  plot(sf::st_geometry(res[[3]]), add = TRUE)
  points(res[[2]], col="green", pch=20)
  points(res[[5]], cex=1.5, col="black", pch=20)
  if (is.matrix(res[[1]])) {
    points(res[[2]][apply(res[[1]], 2, any),], col="red", pch=20)
  } else {
    points(res[[2]][res[[1]],], col="red", pch=20)
  }
  if (legend) {
    legend(x = "bottomright", y = "topleft", yjust=0, title="Visibility", 
           col = c("black", "green", "red"), 
           legend = c("Turbines", "Not visible","Visible"), pch=20)    
  }
  return()
}




#' @title Plot an interpolated viewshed 
#' @name interpol_view
#' @description Plot an interpolated view of the viewshed analysis
#'
#' @export
#'
#' @param res The result list from viewshed.
#' @param plot Should the result be plotted? Default is TRUE
#' @param breakseq The breaks for value plotting. By default, 5 equal intervals
#'   are generated.
#' @param breakform If 'breakseq' is missing, a sampling function to calculate
#'   the breaks, like \code{\link{quantile}}, fivenum, etc.
#' @param plotDEM Plot the DEM? Default is FALSE
#' @param fun Function used for rasterize. Default is mean
#' @param pal A color palette
#' @param ... Arguments passed on to \code{\link[raster]{plot}}.
#'
#' @family Viewshed Analysis
#' @family Plotting Functions
#' @return An interpolated raster
#' 
#' @examples \dontrun{
#' library(sp)
#' library(raster)
#' Polygon1 <- Polygon(rbind(c(4488182, 2667172), c(4488182, 2669343),
#'                           c(4499991, 2669343), c(4499991, 2667172)))
#' Polygon1 <- Polygons(list(Polygon1), 1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+init=epsg:3035"
#' proj4string(Polygon1) <- CRS(Projection)
#' DEM_meter <- getDEM(Polygon1)
#' 
#' turbloc = spsample(DEM_meter[[2]], 10, type = "random");
#' res <- viewshed(r = DEM_meter[[1]], shape=DEM_meter[[2]], 
#'                 turbine_locs = turbloc,  h1=1.8, h2=50)
#' interpol_view(res, plotDEM = TRUE)
#' 
#' interpol_view(res, breakseq = seq(0,max(colSums(res$Result)),1))
#' interpol_view(res, plotDEM = FALSE, breakform = quantile)
#' interpol_view(res, breakform = factor)
#' 
#' ## Different color palettes
#' interpol_view(res, plotDEM = TRUE, pal=topo.colors)
#' interpol_view(res, plotDEM = TRUE, pal=colorRampPalette(c("white","purple")))
#' 
#' ## ... Arguments are past on to the raster plot method
#' interpol_view(res, plotDEM = TRUE, alpha=0.5)
#' interpol_view(res, plotDEM = FALSE, breakseq = seq(0,10,1), colNA="black")
#' 
#' }
interpol_view <- function(res, plot=TRUE, breakseq, breakform = NULL, 
                          plotDEM=FALSE, fun = mean, pal = NULL, ...) {
  
  if (nrow(res$Result) > 1) {
    res$Result <- apply(res$Result, 2, function(d) {
      sum(d)
    })
  }
  
  visible = raster::rasterize(res$Raster_POI, res$DEM, 
                              field = res$Result, fun = fun)
  rasterpois <- cbind(res$Raster_POI, "z" = res$Result)
  
  if (plot) {
    if (is.null(pal)) {
      pal <- colorRampPalette(c("green","orange","red"))
    }
    maxR = max(rasterpois[,3])

    if (missing(breakseq)) {
      a <- range(rasterpois[,3])
      breakseq <- seq(from = a[1], to = a[2], length.out = 5)
      
      if (!is.null(breakform)) {
        breakseq <- as.numeric(breakform(rasterpois[,3]))
      }
      breakseq <- breakseq[!duplicated(breakseq)]
    } 

    if (!any(breakseq == maxR)) {
      breakseq <- c(breakseq, maxR)
    }
    
    if (plotDEM) {
      raster::plot(res$DEM, legend = FALSE)
      raster::plot(visible, breaks=breakseq, add = TRUE, col=pal(length(breakseq)), ...)
    } else {
      raster::plot(visible, breaks=breakseq, col=pal(length(breakseq)), ...)
    }
    
    points(res$Turbines, pch=20, col="black", cex=1.5)
  }
  return(visible)
}


#' @title getISO3
#' @name getISO3
#' @description Get point values from the \code{\link[rworldmap]{getMap}}
#'
#' @export
#' 
#' @param pp SpatialPoints or matrix
#' @param crs_pp The CRS of the points
#' @param col Which column/s should be returned
#' @param resol The search resolution if high accuracy is needed
#' @param coords The column names of the point matrix
#' @param ask A boolean, to ask which columns can be returned
#' 
#' @family Helper Functions
#' @return A character vector
#' 
#' @examples \dontrun{
#' points = cbind(c(4488182.26267016, 4488852.91748256), 
#' c(2667398.93118627, 2667398.93118627))
#' getISO3(pp = points, ask = TRUE)
#' getISO3(pp = points, crs_pp = 3035)
#' 
#' points <- as.data.frame(points)
#' colnames(points) <- c("x","y")
#' points <- st_as_sf(points, coords = c("x","y"))
#' st_crs(points) <- 3035
#' getISO3(pp = points, crs_pp = 3035)
#' }
getISO3 <- function(pp, crs_pp = 4326, col = "ISO3", resol = "low", 
                    coords = c("LONG", "LAT"), ask=FALSE) {
  # pp= points; col = "ISO3"; crs_pp = 3035; resol = "low"; coords = c("LONG", "LAT")
  # pp = points; col = "?"; crs_pp = 3035; resol = "low"; coords = c("LONG", "LAT"); ask=T
  
  if ("?" %in% col) {ask <- TRUE}
  
  countriesSP <- rworldmap::getMap(resolution=resol)
  
  if (ask == TRUE) {
    print(sort(names(countriesSP)))
    cat("Enter an ISO3 code: ")
    col <- readLines(n = 1, con = getOption("windfarmGA.connection"))
    if (!col %in% sort(names(countriesSP))) {
      stop("Column not found")
    }
  }
  
  ## if sf
  if (class(pp)[1] %in% c("sf")) { 
    pp <- sf::st_coordinates(pp)
  }
  
  pp <- as.data.frame(pp)
  colnames(pp) <- coords
  
  pp <- st_as_sf(pp, coords = coords, crs = crs_pp)
  pp <- st_transform(pp, crs = countriesSP@proj4string@projargs)
  
  pp1 <- as(pp, "Spatial")
  
  # use 'over' to get indices of the Polygons object containing each point 
  worldmap_values <- sp::over(pp1, countriesSP)
  
  # return desired columns of each country
  unique(worldmap_values[, col, drop = FALSE])
}


#' @title Get DEM raster
#' @name getDEM
#' @description Get a DEM raster for a country based on ISO3 code
#'
#' @export
#'
#' @param ISO3 The ISO3 code of the country
#' @param clip boolean, indicating if polygon should be cropped. Default is TRUE
#' @param polygon A Spatial / SimpleFeature Polygon to crop the DEM
#'
#' @family Helper Functions
#' @return A list with the DEM raster, and a SpatialPolygonsDataFrame or NULL if
#'   no polygon is given
#' 
#' @examples \dontrun{
#' library(sp)
#' library(raster)
#' Polygon1 <- Polygon(rbind(c(4488182, 2667172), c(4488182, 2669343),
#'                           c(4499991, 2669343), c(4499991, 2667172)))
#' Polygon1 <- Polygons(list(Polygon1), 1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+init=epsg:3035"
#' proj4string(Polygon1) <- CRS(Projection)
#' DEM_meter <- getDEM(Polygon1)
#' plot(DEM_meter[[1]])
#' plot(DEM_meter[[2]], add=TRUE)
#' }
getDEM <- function(polygon, ISO3 = "AUT", clip = TRUE) {
  # polygon = shape; ISO3 = "AUT"
  PROJ <- "+init=epsg:3035"
  
  # DEM <- getData("SRTM", lon = st_bbox(polygon)[1], lat=st_bbox(polygon)[2])
  DEM <- raster::getData("alt", country = ISO3)
  
  if (clip) {
    ## if data.frame / sp object ? -----------------
    # shape <- st_as_sf(shape)
    if ( inherits(polygon, "SpatialPolygons") || inherits(polygon, "SpatialPolygonsDataFrame") ) {
      polygon <- sf::st_as_sf(polygon)
    }
    shape <- sf::st_transform(polygon, crs = raster::projection(DEM))
    shape_SP <- as(shape, "Spatial")
    
    DEM <- raster::crop(x = DEM, raster::extent(shape_SP))
    # shape_meter <- sf::st_transform(shape, PROJ)
    shape_SP <- sp::spTransform(shape_SP, CRSobj = crs(PROJ))
  }

  DEM_meter <- raster::projectRaster(DEM, crs = PROJ)

  if (clip) {
    return(list(DEM_meter, shape_SP))
  } else {
    return(list(DEM_meter, NULL))
  }
}

