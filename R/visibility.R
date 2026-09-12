
#' @title Plot visibility
#' @name plot_viewshed
#' @description Calculate and plot visibility for given points in a given area.
#'   `terra::viewshed` needs a projected (metre) raster. Lon/lat DEMs
#'   (typical `elevatr` downloads) are projected automatically.
#'
#' @export
#'
#' @param r The elevation SpatRaster
#' @param turbine_locs Coordinates, SpatialPoint or SimpleFeature Points
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
#' f <- system.file("ex/elev.tif", package = "terra")
#' r <- rast(f)
#' x <- project(r, "EPSG:2169")
#' shape <- sf::st_as_sf(as.polygons(terra::boundaries(x)))
#' plot(shape)
#' st_crs(shape) <- 2169
#' locs <- st_sample(shape, 10, type = "random")
#' plot_viewshed(x, locs, h1 = 0, h2 = 0, plot = TRUE)
#' }
plot_viewshed <- function(r, turbine_locs, h1 = 0, h2 = 0, plot = TRUE, ...) {
  if (!inherits(r, "SpatRaster")) {
    r <- terra::rast(r)
  }
  loc_crs <- NULL
  if (inherits(turbine_locs, "Spatial")) {
    turbine_locs <- sf::st_as_sf(turbine_locs)
  }
  if (inherits(turbine_locs, "sf") || inherits(turbine_locs, "sfc")) {
    loc_crs <- sf::st_crs(turbine_locs)
    turbine_locs <- sf::st_coordinates(turbine_locs)
  }
  turbine_locs <- as.matrix(turbine_locs)[, 1:2, drop = FALSE]

  aligned <- viewshed_project(r, turbine_locs, loc_crs)
  r <- aligned$r
  turbine_locs <- aligned$xy

  h1 <- rep(h1, length.out = nrow(turbine_locs))
  res <- lapply(seq_len(nrow(turbine_locs)), function(i) {
    as.numeric(terra::viewshed(r,
      loc = turbine_locs[i, ],
      observer = h1[[i]],
      target = h2
    ))
  })
  res <- terra::sprc(res)
  res_mosaic <- terra::mosaic(res, fun = "max")
  if (plot) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mfrow = c(1, 2))
    terra::plot(r, main = "Elevation", ...)
    terra::plot(res_mosaic, main = "Visibility", ...)
    graphics::points(turbine_locs[, 1], turbine_locs[, 2], pch = 20, col = "white", cex = 2.2)
    graphics::points(turbine_locs[, 1], turbine_locs[, 2], pch = 20, col = "black", cex = 1.8)
    add_legend("bottomright", legend = "Turbines", pch = 20, col = "black")
  }
  res_mosaic
}

viewshed_metric_crs <- function(r, loc_crs = NULL) {
  if (!is.null(loc_crs) && !is.na(loc_crs) && !sf::st_is_longlat(loc_crs)) {
    wkt <- loc_crs$wkt
    if (!is.null(wkt) && nzchar(wkt)) {
      return(wkt)
    }
  }
  e <- terra::ext(r)
  lon <- mean(c(e$xmin, e$xmax))
  lat <- mean(c(e$ymin, e$ymax))
  zone <- as.integer(floor((lon + 180) / 6) + 1)
  zone <- min(60L, max(1L, zone))
  epsg <- if (lat >= 0) 32600L + zone else 32700L + zone
  paste0("EPSG:", epsg)
}

viewshed_project <- function(r, xy, loc_crs = NULL) {
  src <- terra::crs(r)
  lonlat_r <- isTRUE(terra::is.lonlat(r))
  lonlat_xy <- !is.null(loc_crs) && !is.na(loc_crs) && isTRUE(sf::st_is_longlat(loc_crs))

  if (lonlat_r) {
    target <- viewshed_metric_crs(r, loc_crs)
    r <- terra::project(r, target)
  }
  dest <- terra::crs(r)
  from_crs <- if (!is.null(loc_crs) && !is.na(loc_crs)) {
    loc_crs
  } else if (!is.null(src) && nzchar(as.character(src))) {
    sf::st_crs(src)
  } else {
    NULL
  }
  if ((lonlat_r || lonlat_xy) && !is.null(from_crs)) {
    pts <- sf::st_as_sf(
      data.frame(x = xy[, 1], y = xy[, 2]),
      coords = c("x", "y"),
      crs = from_crs
    )
    pts <- sf::st_transform(pts, dest)
    xy <- sf::st_coordinates(pts)
  }
  list(r = r, xy = xy)
}
