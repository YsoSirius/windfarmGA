#' @title Get terrainhic rasters
#'
#' @name terrain_model
#' @description Calculate the SpatRasters needed for the terrain model.
#'
#' @export
#' @inheritParams genetic_algorithm
#' @param plot Plot the elevation and roughness rasters
#'
#' @family Terrain Model
#' @return A list of SpatRasters
#'
#' @examples \dontrun{
#' library(sf)
#' area <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4651704, 4651704, 4654475, 4654475, 4651704),
#'     c(2692925, 2694746, 2694746, 2692925, 2692925)
#'   ))),
#'   crs = 3035
#' ))
#' Polygon_wgs84 <- sf::st_transform(area, st_crs(4326))
#' srtm <- elevatr::get_elev_raster(locations = Polygon_wgs84, z = 11)
#' res <- terrain_model(srtm, area)
#' }
terrain_model <- function(terrain = TRUE, area, ccl, ccl_roughness,
                          plot = FALSE, verbose = FALSE) {
  if (verbose) message("Topography and orography are taken into account.\n")
  if (plot) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mfrow = c(3, 1))
  }

  ## Land Cover / Surface Roughness ################
  if (missing(ccl) || is.null(ccl)) {
    message(
      "No land cover raster ('ccl') was given. It will be downloaded from ",
      "the EEA-website.\n"
    )
    if (!file.exists("g100_06.tif")) {
      # "https://www.eea.europa.eu/data-and-maps/data/clc-2006-raster-3/clc-2006-100m/g100_06.zip/at_download/file"
      download.file("http://github.com/YsoSirius/windfarm_data/raw/master/clc.zip",
        destfile = "clc.zip",
        method = "auto"
      )
      unzip("clc.zip")
      unlink("clc.zip")
    }
    ccl <- terra::rast("g100_06.tif")
  } else {
    if (!inherits(ccl, "SpatRaster")) {
      ccl <- terra::rast(ccl)
    } else {
      ccl <- ccl
    }
  }
  cclPoly <- terra::crop(ccl, area)

  ## DEM Data ######################
  if (isTRUE(terrain)) {
    if (!is_elevatr_installed()) {
      stop(
        "The package 'elevatr' is required for this function, but it is not installed.\n",
        "Please install it with `install.packages('elevatr')`"
      )
    }
    polygon_wgs84 <- sf::st_transform(area, st_crs(4326))
    srtm <- tryCatch(elevatr::get_elev_raster(
      verbose = verbose,
      locations = polygon_wgs84, z = 11
    ),
    error = function(e) {
      stop("\nDownloading Elevation data failed for the given Polygon.\n",
        e,
        call. = FALSE
      )
    }
    )
    srtm <- terra::rast(srtm)
  } else {
    if (!inherits(terrain, "SpatRaster")) {
      srtm <- terra::rast(terrain)
    } else {
      srtm <- terrain
    }
  }
  srtm <- terra::project(srtm, terra::crs(area, proj = TRUE))
  srtm_crop <- terra::crop(srtm, area, mask = TRUE)

  if (plot) {
    terra::plot(srtm_crop, main = "Elevation Data")
    plot(area, add = TRUE, color = "transparent")
  }

  roughrast <- terra::terrain(srtm_crop, "roughness")
  if (all(is.na(terra::values(roughrast)))) {
    warning(
      "Cannot calculate a surface roughness. \nMaybe the resolution or ",
      "the area is too small. Roughness values are set to 1.\n"
    )
    terra::values(roughrast) <- 1
  }
  srtm_crop <- list(
    strm_crop = srtm_crop,
    orogr1 = srtm_crop / as.numeric(terra::global(srtm_crop, fun = "mean", na.rm = TRUE)),
    roughness = roughrast
  )

  # Include Corine Land Cover Raster to get an estimation of Surface Roughness
  if (missing(ccl_roughness) || is.null(ccl_roughness)) {
    ccl_roughness <- system.file(
      "extdata", "clc_legend.csv",
      package = "windfarmGA"
    )
    if (!nzchar(ccl_roughness) || !file.exists(ccl_roughness)) {
      stop(
        "CLC roughness legend not found in the package. ",
        "Pass ccl_roughness to a semicolon-separated CSV with columns ",
        "GRID_CODE and Rauhigkeit_z.",
        call. = FALSE
      )
    }
  } else {
    if (verbose) {
      message("You are using your own Corine Land Cover legend.")
    }
  }

  rauhigkeitz <- utils::read.csv(ccl_roughness,
    header = TRUE, sep = ";"
  )
  cclRaster <- terra::classify(cclPoly, matrix(c(
    rauhigkeitz$GRID_CODE,
    rauhigkeitz$Rauhigkeit_z
    ),
    ncol = 2)
  )

  if (plot) {
    terra::plot(srtm_crop$roughness, main = "Elevation Roughness")
    terra::plot(cclRaster, main = "Surface Roughness from Corine Land Cover")
  }

  return(list(
    "srtm_crop" = srtm_crop,
    "cclRaster" = cclRaster
  ))
}
