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
    ), ncol = 2)
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

terrain_fill_na <- function(v) {
  v <- as.numeric(v)
  if (anyNA(v)) {
    m <- mean(v, na.rm = TRUE)
    v[is.na(v)] <- if (is.finite(m)) m else 0
  }
  v
}

terrain_extract_col <- function(r, xy) {
  ex <- terra::extract(x = r, y = xy)
  terrain_fill_na(ex[[ncol(ex)]])
}

terrain_cell_lookup <- function(srtm_crop, ccl_raster, grid, rotor_height) {
  xy <- cbind(as.numeric(grid[, "X"]), as.numeric(grid[, "Y"]))
  elev <- terrain_extract_col(srtm_crop[[1]], xy)
  wind_mult <- terrain_extract_col(srtm_crop[[2]], xy)
  land_z0 <- terrain_extract_col(ccl_raster, xy)
  elev_rough <- terrain_extract_col(srtm_crop[[3]], xy)
  maxres <- max(terra::res(srtm_crop[[3]]))
  z0 <- land_z0 * (1 + elev_rough / maxres)
  k <- 0.5 / log(rotor_height / z0)
  air_rh <- as.numeric(barometric_height(matrix(elev), elev)[, "rh"])
  cells <- data.frame(
    ID = as.integer(grid[, "ID"]),
    X = xy[, 1],
    Y = xy[, 2],
    elevation = elev,
    wind_mult = wind_mult,
    land_z0 = land_z0,
    elev_rough = elev_rough,
    z0 = z0,
    k = k,
    air_rh = air_rh,
    stringsAsFactors = FALSE
  )
  attr(cells, "maxres") <- maxres
  cells
}

terrain_ensure_cells <- function(elevation, ccl_raster, grid, rotor_height) {
  if (!is.list(elevation) || !is.null(elevation$cells)) {
    return(elevation)
  }
  elevation$cells <- terrain_cell_lookup(
    elevation, ccl_raster, grid, rotor_height
  )
  elevation
}

terrain_cells_of <- function(elevation) {
  if (is.list(elevation) && !is.null(elevation$cells)) {
    return(elevation$cells)
  }
  NULL
}

terrain_has_rasters <- function(elevation) {
  is.list(elevation) &&
    length(elevation) >= 3L &&
    inherits(elevation[[1]], "SpatRaster")
}

layout_xy <- function(sel) {
  nms <- colnames(sel)
  if (!is.null(nms) && all(c("X", "Y") %in% nms)) {
    return(cbind(X = as.numeric(sel[, "X"]), Y = as.numeric(sel[, "Y"])))
  }
  cbind(X = as.numeric(sel[, 2]), Y = as.numeric(sel[, 3]))
}

layout_ids <- function(sel) {
  nms <- colnames(sel)
  if (is.null(nms)) {
    return(NULL)
  }
  col <- if ("ID" %in% nms) {
    "ID"
  } else if ("Rect_ID" %in% nms) {
    "Rect_ID"
  } else {
    return(NULL)
  }
  as.integer(sel[, col])
}

match_terrain_rows <- function(xy, ids, cells) {
  if (!is.null(ids) && length(ids) == nrow(xy)) {
    row <- match(ids, as.integer(cells$ID))
    if (!anyNA(row) && length(unique(ids)) == length(ids)) {
      return(row)
    }
  }
  cx <- cells$X
  cy <- cells$Y
  x <- xy[, 1]
  y <- xy[, 2]
  vapply(seq_len(nrow(xy)), function(i) {
    which.min((cx - x[i])^2 + (cy - y[i])^2)
  }, integer(1))
}

terrain_from_extract <- function(xy, elevation, ccl_raster, rotor_height) {
  elev <- terrain_extract_col(elevation[[1]], xy)
  wind_mult <- terrain_extract_col(elevation[[2]], xy)
  land_z0 <- terrain_extract_col(ccl_raster, xy)
  elev_rough <- terrain_extract_col(elevation[[3]], xy)
  maxres <- max(terra::res(elevation[[3]]))
  z0 <- land_z0 * (1 + elev_rough / maxres)
  list(
    wind_mult = wind_mult,
    elevation = elev,
    z0 = z0,
    k = 0.5 / log(rotor_height / z0),
    air_rh = as.numeric(barometric_height(matrix(elev), elev)[, "rh"]),
    land_z0 = land_z0,
    elev_rough = elev_rough,
    maxres = maxres
  )
}

terrain_at_layout <- function(xy, ids, elevation, ccl_raster, rotor_height) {
  cells <- terrain_cells_of(elevation)
  if (!is.null(cells) && nrow(cells)) {
    row <- match_terrain_rows(xy, ids, cells)
    return(list(
      wind_mult = cells$wind_mult[row],
      elevation = cells$elevation[row],
      z0 = cells$z0[row],
      k = cells$k[row],
      air_rh = cells$air_rh[row],
      land_z0 = if ("land_z0" %in% names(cells)) cells$land_z0[row] else NA_real_,
      elev_rough = if ("elev_rough" %in% names(cells)) cells$elev_rough[row] else NA_real_,
      maxres = attr(cells, "maxres"),
      from_cells = TRUE
    ))
  }
  out <- terrain_from_extract(xy, elevation, ccl_raster, rotor_height)
  out$from_cells <- FALSE
  out
}

ga_result_terrain <- function(x) {
  if (is.null(x) || !is.matrix(x) || !"terrainModel" %in% colnames(x)) {
    return(NULL)
  }
  tm <- tryCatch(x[1, "terrainModel"][[1]], error = function(e) NULL)
  if (!is.list(tm) || is.null(tm$srtm_crop)) {
    return(NULL)
  }
  tm
}

terrain_is_dem <- function(terrain) {
  inherits(terrain, c("SpatRaster", "RasterLayer", "stars"))
}

## Stored GA rasters, else a user DEM, else download (`terrain = TRUE`).
terrain_resolve <- function(result, terrain, area, ccl = NULL,
                            ccl_roughness = NULL, plot = FALSE,
                            verbose = FALSE) {
  if (isFALSE(terrain)) {
    return(NULL)
  }
  if (terrain_is_dem(terrain)) {
    return(terrain_model(terrain, area, ccl, ccl_roughness, plot, verbose))
  }
  stored <- ga_result_terrain(result)
  if (!is.null(stored)) {
    return(stored)
  }
  terrain_model(TRUE, area, ccl, ccl_roughness, plot, verbose)
}

plot_terrain_energy <- function(xy, polygon1, srtm_crop, ccl_raster, terr, cexa) {
  x <- xy[, 1]
  y <- xy[, 2]
  maxres <- terr$maxres
  if (!is.finite(maxres[1])) {
    maxres <- max(terra::res(srtm_crop[[3]]))
  }
  land_z0 <- terr$land_z0
  elev_rough <- terr$elev_rough
  if (anyNA(land_z0) && inherits(ccl_raster, "SpatRaster")) {
    land_z0 <- terrain_extract_col(ccl_raster, xy)
  }
  if (anyNA(elev_rough)) {
    elev_rough <- terrain_extract_col(srtm_crop[[3]], xy)
  }

  par(mfrow = c(2, 1))
  plot(srtm_crop[[1]], main = "SRTM Elevation Data")
  points(x, y, pch = 20)
  calibrate::textxy(x, y, labs = round(terr$elevation, 0), cex = cexa)
  plot(sf::st_geometry(polygon1), add = TRUE)
  plot(srtm_crop[[2]], main = "Wind Speed Multipliers")
  points(x, y, pch = 20)
  calibrate::textxy(x, y, labs = round(terr$wind_mult, 3), cex = cexa)
  plot(sf::st_geometry(polygon1), add = TRUE)

  par(mfrow = c(1, 1))
  plot(srtm_crop[[1]], main = "Normal Air Density", col = topo.colors(10))
  points(x, y, pch = 20)
  calibrate::textxy(x, y, labs = rep(1.225, length(x)), cex = cexa)
  plot(sf::st_geometry(polygon1), add = TRUE)
  terra::plot(srtm_crop[[1]], main = "Corrected Air Density", col = topo.colors(10))
  points(x, y, pch = 20)
  calibrate::textxy(x, y, labs = round(terr$air_rh, 4), cex = cexa)
  plot(sf::st_geometry(polygon1), add = TRUE)

  terrain_rough_ras <- srtm_crop[[3]]
  terrain_rough_resample <- terra::resample(terrain_rough_ras, ccl_raster, method = "near")
  modified_rough <- terra::lapp(
    x = c(ccl_raster, terrain_rough_resample),
    fun = function(a, b) a * (1 + b / maxres)
  )
  graphics::par(mfrow = c(1, 1))
  plot(ccl_raster, main = "Corine Land Cover Roughness")
  graphics::points(x, y, pch = 20)
  calibrate::textxy(x, y, labs = round(land_z0, 2), cex = cexa)
  plot(sf::st_geometry(polygon1), add = TRUE)
  plot(x = terrain_rough_ras, main = "Elevation Roughness Indicator")
  graphics::points(x, y, pch = 20)
  calibrate::textxy(x, y, labs = round(1 + (elev_rough / maxres), 2), cex = cexa)
  plot(sf::st_geometry(polygon1), add = TRUE)
  plot(modified_rough, main = "Modified Surface Roughness")
  graphics::points(x, y, pch = 20)
  calibrate::textxy(x, y, labs = round(terr$z0, 2), cex = cexa)
  plot(sf::st_geometry(polygon1), add = TRUE)

  graphics::par(mfrow = c(1, 1))
  plot(x = terrain_rough_ras, main = "Adapted Wake Decay Values - K")
  graphics::points(x, y, pch = 20)
  calibrate::textxy(x, y, labs = round(terr$k, 3), cex = cexa)
  plot(sf::st_geometry(polygon1), add = TRUE)
}
