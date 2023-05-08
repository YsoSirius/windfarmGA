#' @title Get topographic rasters
#' 
#' @name terrain_model
#' @description Calculate the SpatRasters needed for the terrain model.
#'
#' @export
#' @inheritParams genetic_algorithm
#' @param plotit Plots the elevation data
#' 
#' @family Terrain Model
#' @return A list of SpatRasters
#'
#' @examples \dontrun{
#' library(sf)
#' Polygon1 <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4651704, 4651704, 4654475, 4654475, 4651704),
#'     c(2692925, 2694746, 2694746, 2692925, 2692925)))), 
#'   crs = 3035
#' ))
#' Polygon_wgs84 <-  sf::st_transform(Polygon1, st_crs(4326))
#' srtm <- elevatr::get_elev_raster(locations = as(Polygon_wgs84, "Spatial"), z = 11)
#' res <- terrain_model(srtm, Polygon1)
#' }
terrain_model <- function(topograp = TRUE, Polygon1, sourceCCL, sourceCCLRoughness, 
                          plotit=FALSE, verbose=FALSE) {
  if (verbose) message("Topography and orography are taken into account.\n")
  if (plotit) {
    par(mfrow = c(3, 1))
  }
  
  ## Land Cover / Surface Roughness ################
  if (missing(sourceCCL) || is.null(sourceCCL)) {
    message("No land cover raster ('sourceCCL') was given. It will be downloaded from ",
            "the EEA-website.\n")
    if (!file.exists("g100_06.tif")) {
      # "https://www.eea.europa.eu/data-and-maps/data/clc-2006-raster-3/clc-2006-100m/g100_06.zip/at_download/file"
      download.file("http://github.com/YsoSirius/windfarm_data/raw/master/clc.zip", 
                    destfile = "clc.zip", 
                    method = "auto")
      unzip("clc.zip")
      unlink("clc.zip")
    }
    ccl <- terra::rast("g100_06.tif")
  } else {
    if (!inherits(sourceCCL, "SpatRaster")) {
      ccl <- terra::rast(sourceCCL)
    } else {
      ccl <- sourceCCL
    }
  }
  cclPoly <- terra::crop(ccl, Polygon1)
  
  ## DEM Data ######################
  if (isTRUE(topograp)) {
    if (!is_elevatr_installed()) {
      stop("The package 'elevatr' is required for this function, but it is not installed.\n",
           "Please install it with `install.packages('elevatr')`")
    }
    Polygon_wgs84 <-  sf::st_transform(Polygon1, st_crs(4326))
    srtm <- tryCatch(elevatr::get_elev_raster(verbose = verbose,
                                              locations = as(Polygon_wgs84, "Spatial"), z = 11),
                     error = function(e) {
                       stop("\nDownloading Elevation data failed for the given Polygon.\n",
                            e, call. = FALSE)
                     })
    srtm <- terra::rast(srtm)
  } else {
    if (!inherits(topograp, "SpatRaster")) {
      srtm <- terra::rast(topograp)
    } else {
      srtm <- topograp
    }
  }
  srtm <- terra::project(srtm, terra::crs(Polygon1, proj=TRUE))
  srtm_crop <- terra::crop(srtm, Polygon1, mask = TRUE)
  
  if (plotit) {
    terra::plot(srtm_crop, main = "Elevation Data")
    plot(Polygon1, add = TRUE, color = "transparent")
  }
  
  roughrast <- terra::terrain(srtm_crop, "roughness")
  if (all(is.na(terra::values(roughrast)))) {
    warning("Cannot calculate a surface roughness. \nMaybe the resolution or ",
            "the area is too small. Roughness values are set to 1.\n")
    terra::values(roughrast) <- 1
  }
  srtm_crop <- list(
    strm_crop = srtm_crop,
    orogr1 = srtm_crop / as.numeric(terra::global(srtm_crop, fun="mean", na.rm = TRUE)),
    roughness = roughrast
  )
  
  # Include Corine Land Cover Raster to get an estimation of Surface Roughness
  if (missing(sourceCCLRoughness) || is.null(sourceCCLRoughness)) {
    path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
    sourceCCLRoughness <- paste0(path, "clc_legend.csv")
  } else {
    if (verbose) {
      message("You are using your own Corine Land Cover legend.")
    }
  }
  
  rauhigkeitz <- utils::read.csv(sourceCCLRoughness,
                                 header = TRUE, sep = ";")
  cclRaster <- terra::classify(cclPoly, matrix(c(rauhigkeitz$GRID_CODE,
                                                 rauhigkeitz$Rauhigkeit_z),
                                               ncol = 2))
  
  if (plotit) {
    terra::plot(cclRaster, main = "Surface Roughness from Corine Land Cover")
  }
  
  return(list(
    "srtm_crop" = srtm_crop,
    "cclRaster" = cclRaster
  ))
}