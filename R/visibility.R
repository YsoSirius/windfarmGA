
#' @title Plot visibility
#' @name plot_viewshed
#' @description Calculate and plot visibility for given points in a given area.
#'
#' @export
#'
#' @param r The elevation SpatRaster
#' @param turbine_locs Coordinates, SpatialPoint or SimpleFeature Poiints 
#'   representing the wind turbines
#' @param h1 A single number or numeric vector giving the extra height offsets 
#'   for the \code{turbine_locs}
#' @param h2 The height offset for Point 2
#' @param plot Should the result be plotted. Default is \code{TRUE}
#' @param ... forwarded to \code{terra::plot}
#' 
#' @family Viewshed Analysis
#' @return A mosaiced SpatRaster, representing the visibility for all \code{turbine_locs}
#' 
#' @examples \donttest{
#' library(sf)
#' library(terra)
#' 
#' f <- system.file("ex/elev.tif", package="terra")
#' r <- rast(f)
#' x <- project(r, "EPSG:2169")
#' shape <- sf::st_as_sf(as.polygons(terra::boundaries(x)))
#' plot(shape)
#' st_crs(shape) <- 2169
#' locs = st_sample(shape, 10, type = "random")
#' plot_viewshed(x, locs, h1=0, h2=0, plot=TRUE)
#' }
plot_viewshed <- function(r, turbine_locs, h1=0, h2=0, plot=TRUE, ...) {
  if (inherits(turbine_locs, "Spatial")) {
    turbine_locs <- sf::st_as_sf(turbine_locs)
  }
  if (inherits(turbine_locs, "sf") || inherits(turbine_locs, "sfc")) {
    turbine_locs <- sf::st_coordinates(turbine_locs)
  }
  
  ## Run `terra::viewshed` function for every turbine with its offest height `h1`
  ## Repeat `h1` to nrow turbines.
  h1 <- rep(h1, length.out = nrow(turbine_locs))
  res <- lapply(1:nrow(turbine_locs), function(i) {
    as.numeric(terra::viewshed(r,
                    loc = turbine_locs[i,],
                    observer = h1,
                    target = h2))
  })
  res <- terra::sprc(res)
  res_mosaic <- terra::mosaic(res, fun = "max") 
  if (plot) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mfrow = c(1, 2))
    terra::plot(r, main="Elevation", ...)
    terra::plot(res_mosaic, main="Visibility", ...)
    graphics::points(turbine_locs[,1], turbine_locs[,2], pch=20, col="white", cex=2.2)
    graphics::points(turbine_locs[,1], turbine_locs[,2], pch=20, col="black", cex=1.8)
    # browser()
    # a <- legend(x = "bottomright", legend = "Turbines", pch = 20, col="black")
    # e <- unlist(terra:::get.clip())
    # xy <- terra:::get_legxy(a$rect, e[1:4], "bottomright", NULL)
    # graphics::legend(x = xy[1], y = xy[2], legend = "Turbines", pch = 20, col="black")
  }
  res_mosaic
}

