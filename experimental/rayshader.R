## 3D farm plot with rayshader (experimental, not in the CRAN package).
##
##   source("experimental/rayshader.R")   # not _experiment/rayshader.R
##   plot_farm_3d_from_result(result, area, buffer = 4000, exaggerate = 1)
##   args(plot_farm_3d)  # must list exaggerate, turbine_obj
##
## DEM: `plot_farm_3d_from_result` reuses `result$terrainModel` when
## present (park crop only). Pass `dem` for a wider buffer download.
## Surrounding terrain (`buffer`), black tower+rotor (or a user OBJ).
## OBJ: 3ds Max Z-up is rotated +90° about X (rayshader is Y-up).
## Wind arrow on the upwind DEM rim, above the terrain; wake cones at
## hub height (colored by AbschGesamt). Default zscale is true scale
## (cell size). Do not raise zscale for "more relief" -- that flattens
## the DEM. Use exaggerate > 1 instead.
## Heavy install (rayshader, rgl). Do not add to Suggests.

exp_need <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(TRUE)
  }
  stop("install.packages(\"", pkg, "\")", call. = FALSE)
}

exp_layout_xy <- function(result, which = NULL) {
  if (is.null(which)) {
    which <- nrow(result)
  }
  as.data.frame(result[which, "bestPaEn"][[1]])
}

exp_turbine_xy <- function(turbines, crs) {
  if (inherits(turbines, "sf") || inherits(turbines, "sfc")) {
    if (!is.na(sf::st_crs(turbines)) && !is.na(crs)) {
      turbines <- sf::st_transform(turbines, crs)
    }
    return(unname(sf::st_coordinates(turbines)[, 1:2, drop = FALSE]))
  }
  as.matrix(turbines)[, 1:2, drop = FALSE]
}

## Met bearing 0 = north, 90 = east. Destination in projected metres.
exp_as_xy <- function(xy) {
  matrix(as.numeric(xy), ncol = 2L)
}

exp_dest <- function(xy, bearing, dist) {
  xy <- exp_as_xy(xy)
  rad <- as.numeric(bearing) * pi / 180
  cbind(
    xy[, 1] + dist * sin(rad),
    xy[, 2] + dist * cos(rad)
  )
}

## Same mapping as rayshader::generate_surface / transform_into_heightmap_coords.
## rgl is Y-up: cbind(x_east, altitude / zscale, z_south).
exp_xy_to_rgl <- function(xy, altitude, ext, mat, zscale) {
  xy <- exp_as_xy(xy)
  n <- nrow(xy)
  altitude <- rep(as.numeric(altitude), length.out = n)
  nr <- nrow(mat)
  nc <- ncol(mat)
  fx <- (xy[, 1] - ext[["xmin"]]) / (ext[["xmax"]] - ext[["xmin"]])
  fy <- (xy[, 2] - ext[["ymin"]]) / (ext[["ymax"]] - ext[["ymin"]])
  row_i <- fx * (nr - 1)
  col_i <- (1 - fy) * (nc - 1)
  cbind(
    row_i - (nr - 1) / 2,
    altitude / zscale,
    col_i - (nc - 1) / 2
  )
}

exp_lines_rgl <- function(xy, altitude, ext, mat, zscale, color, lwd = 2) {
  xyz <- exp_xy_to_rgl(xy, altitude, ext, mat, zscale)
  rgl::lines3d(xyz, color = color, lwd = lwd)
}

exp_points_rgl <- function(xy, altitude, ext, mat, zscale, color, size = 8) {
  xyz <- exp_xy_to_rgl(xy, altitude, ext, mat, zscale)
  rgl::points3d(xyz, color = color, size = size)
}

## Upwind rim of the DEM (outside the park), in the comes-from direction.
exp_upwind_rim <- function(ext, wd) {
  dcx <- (ext[["xmin"]] + ext[["xmax"]]) / 2
  dcy <- (ext[["ymin"]] + ext[["ymax"]]) / 2
  rad <- as.numeric(wd) * pi / 180
  dx <- sin(rad)
  dy <- cos(rad)
  rx <- 0.42 * (ext[["xmax"]] - ext[["xmin"]])
  ry <- 0.42 * (ext[["ymax"]] - ext[["ymin"]])
  tx <- if (abs(dx) < 1e-9) Inf else rx / abs(dx)
  ty <- if (abs(dy) < 1e-9) Inf else ry / abs(dy)
  t <- min(tx, ty)
  matrix(c(dcx + t * dx, dcy + t * dy), nrow = 1L)
}

exp_dominant_wd <- function(wind) {
  if (is.null(wind) || isFALSE(wind)) {
    return(NULL)
  }
  if (is.list(wind) && !is.data.frame(wind) && !is.matrix(wind)) {
    if (!is.null(wind$Windspeed_Data)) {
      wind <- wind$Windspeed_Data
    } else if (length(wind)) {
      wind <- wind[[1]]
    }
  }
  if (is.numeric(wind) && is.null(dim(wind))) {
    return(as.numeric(wind[1]) %% 360)
  }
  wind <- as.data.frame(wind)
  nms <- names(wind)
  wd_col <- grep("^(wd|dir)", nms, ignore.case = TRUE, value = TRUE)[1]
  if (is.na(wd_col)) {
    wd_col <- nms[min(2L, ncol(wind))]
  }
  pr_col <- grep("prob", nms, ignore.case = TRUE, value = TRUE)[1]
  wd <- as.numeric(wind[[wd_col]]) %% 360
  if (is.na(pr_col)) {
    return(wd[1])
  }
  pr <- as.numeric(wind[[pr_col]])
  wd[which.max(pr)]
}

exp_wake_cols <- function(wake, n) {
  if (is.null(wake)) {
    return(rep("#f5f5f5", n))
  }
  wake <- as.numeric(wake)
  if (length(wake) != n) {
    wake <- rep(wake, length.out = n)
  }
  rng <- range(wake, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) < 1e-9) {
    return(rep("#f1c40f", n))
  }
  u <- (wake - rng[1]) / diff(rng)
  u[!is.finite(u)] <- 0
  grDevices::rgb(
    grDevices::colorRamp(c("#27ae60", "#f1c40f", "#c0392b"))(u),
    maxColorValue = 255
  )
}

exp_site_dem <- function(area, dem = NULL, z = 11, buffer = 3000) {
  exp_need("terra")
  exp_need("sf")
  area <- windfarmGA::isSpatial(area)
  pad <- sf::st_buffer(area, dist = buffer)
  if (!is.null(dem)) {
    if (!inherits(dem, "SpatRaster")) {
      dem <- terra::rast(dem)
    }
  } else {
    exp_need("elevatr")
    wgs <- sf::st_transform(pad, 4326)
    dem <- terra::rast(elevatr::get_elev_raster(locations = wgs, z = z, clip = "bbox"))
  }
  dem <- terra::project(dem, terra::crs(area))
  dem <- terra::crop(dem, terra::vect(pad))
  dem
}

exp_rayshader_matrix <- function(dem, max_dim = 450) {
  exp_need("rayshader")
  n <- max(terra::ncol(dem), terra::nrow(dem))
  if (n > max_dim) {
    fact <- ceiling(n / max_dim)
    dem <- terra::aggregate(dem, fact = fact, fun = "mean")
  }
  mat <- rayshader::raster_to_matrix(dem, verbose = FALSE)
  e <- terra::ext(dem)
  ext <- c(
    xmin = as.numeric(e[1]),
    xmax = as.numeric(e[2]),
    ymin = as.numeric(e[3]),
    ymax = as.numeric(e[4])
  )
  list(mat = mat, dem = dem, ext = ext)
}

exp_obj_height <- function(path) {
  ln <- readLines(path, warn = FALSE)
  v <- ln[startsWith(ln, "v ")]
  if (!length(v)) {
    return(1)
  }
  z <- vapply(strsplit(trimws(sub("^v\\s+", "", v)), "\\s+"), function(p) {
    as.numeric(p[min(3L, length(p))])
  }, numeric(1))
  h <- max(z, na.rm = TRUE) - min(z, na.rm = TRUE)
  if (!is.finite(h) || h <= 0) 1 else h
}

exp_render_path <- function(xy, mat, ext, zscale, color, offset = 8,
                            altitude = NULL, linewidth = 2) {
  xy <- exp_as_xy(xy)
  if (is.null(altitude)) {
    altitude <- offset
  }
  exp_lines_rgl(xy, altitude, ext, mat, zscale, color, lwd = linewidth)
}

exp_elev_along <- function(xy, dem, na_fill) {
  v <- terra::vect(xy, type = "points", crs = terra::crs(dem))
  e <- as.numeric(terra::extract(dem, v, ID = FALSE)[[1]])
  e[is.na(e)] <- na_fill
  e
}

## Sample a polyline every `step` metres so 3D edges follow the DEM
## instead of cutting through hills as straight chords between vertices.
exp_densify_xy <- function(xy, step) {
  xy <- exp_as_xy(xy)
  if (nrow(xy) < 2L || !is.finite(step) || step <= 0) {
    return(xy)
  }
  parts <- vector("list", nrow(xy) - 1L)
  for (i in seq_len(nrow(xy) - 1L)) {
    a <- xy[i, ]
    b <- xy[i + 1L, ]
    d <- sqrt((b[1] - a[1])^2 + (b[2] - a[2])^2)
    n <- max(2L, as.integer(ceiling(d / step)) + 1L)
    t <- seq(0, 1, length.out = n)
    if (i > 1L) {
      t <- t[-1L]
    }
    parts[[i]] <- cbind(a[1] + t * (b[1] - a[1]), a[2] + t * (b[2] - a[2]))
  }
  do.call(rbind, parts)
}

## 3ds Max OBJ is Z-up; rayshader rgl is Y-up. Rotate +90° about X, then yaw.
## Scale is in rgl units (metres / zscale), origin stays on the surface.
exp_place_obj_turbines <- function(path, xy, elev, hub, wind_to, mat, ext, zscale,
                                   color = "black") {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  n <- nrow(xy)
  sc <- hub / (zscale * exp_obj_height(path))
  yaw <- if (is.null(wind_to)) 0 else wind_to
  ang <- matrix(c(90, yaw, 0), nrow = n, ncol = 3, byrow = TRUE)
  xyz <- exp_xy_to_rgl(xy, elev, ext, mat, zscale)
  rayshader::render_obj(
    path,
    xyz = xyz,
    zscale = zscale,
    color = color,
    load_material = FALSE,
    obj_zscale = FALSE,
    swap_yz = FALSE,
    scale = c(sc, sc, sc),
    angle = ang
  )
  invisible(TRUE)
}

## Vertical tower along rgl Y, rotor disk facing downwind.
exp_draw_turbine <- function(xy, elev, hub, rotor, wind_to, mat, ext, zscale,
                             color = "black", turbine_obj = NULL) {
  xy <- exp_as_xy(xy)
  base <- exp_xy_to_rgl(xy, elev, ext, mat, zscale)
  tip <- base
  tip[2] <- base[2] + hub / zscale
  rad <- max(hub / zscale * 0.025, 0.15)
  cyl <- try(
    rgl::cylinder3d(center = rbind(base, tip), radius = rad, closed = -2),
    silent = TRUE
  )
  if (!inherits(cyl, "try-error")) {
    rgl::shade3d(cyl, color = color)
  } else {
    rgl::segments3d(rbind(base, tip), color = color, lwd = 5)
  }
  rgl::points3d(tip, color = color, size = 8)
  if (rotor <= 0) {
    return(invisible(NULL))
  }
  wt <- (if (is.null(wind_to)) 270 else wind_to) * pi / 180
  r_rgl <- rotor / zscale
  right <- c(-cos(wt), 0, -sin(wt))
  up <- c(0, 1, 0)
  th <- seq(0, 2 * pi, length.out = 25)
  ring <- cbind(
    tip[1] + r_rgl * (cos(th) * right[1] + sin(th) * up[1]),
    tip[2] + r_rgl * (cos(th) * right[2] + sin(th) * up[2]),
    tip[3] + r_rgl * (cos(th) * right[3] + sin(th) * up[3])
  )
  rgl::lines3d(ring, color = color, lwd = 2)
  for (b in 0:2) {
    ang <- b * 2 * pi / 3
    blade <- rbind(
      tip,
      c(
        tip[1] + r_rgl * (cos(ang) * right[1] + sin(ang) * up[1]),
        tip[2] + r_rgl * (cos(ang) * right[2] + sin(ang) * up[2]),
        tip[3] + r_rgl * (cos(ang) * right[3] + sin(ang) * up[3])
      )
    )
    rgl::lines3d(blade, color = color, lwd = 3)
  }
  invisible(NULL)
}

#' 3D site + turbines, optional wind arrow and wake cones.
#'
#' @param buffer Extra terrain around the site (m). The DEM is not masked
#'   to the polygon, so hills outside the park stay visible.
#' @param wind Comes-from direction, rose, or `NULL`. Arrow sits on the
#'   **upwind DEM rim**, well above the terrain; cones point **downwind**.
#'   Lighting `sunangle` follows this if unset.
#' @param wake Numeric wake per turbine (`AbschGesamt`). Colors **cones**
#'   green → red. Turbines stay black.
#' @param wake_angle Jensen cone half-angle (degrees).
#' @param wake_length Cone length (m). Default `15 * 2 * rotor` (15 D).
#' @param exaggerate `1` = true scale (cell size in metres). `>1` stretches
#'   the vertical axis (terrain and turbines together). Do not raise
#'   `zscale` for "more relief" -- that flattens the DEM.
#' @param turbine_obj Optional OBJ path (units: metres), yawed downwind.
#'   If `NULL`, a black tower + rotor disk is drawn.
plot_farm_3d <- function(area, turbines, hub_height = 100, dem = NULL,
                         z = 11, zscale = NULL, texture = "imhof4",
                         sunangle = NULL, zoom = 0.7, max_dim = 450,
                         water = FALSE, buffer = 3000,
                         wind = NULL, wake = NULL, wake_angle = 20,
                         wake_length = NULL, rotor = 50,
                         site_outline = TRUE, exaggerate = 1,
                         turbine_obj = NULL, turbine_color = "black", ...) {
  exp_need("rayshader")
  exp_need("rgl")
  area <- windfarmGA::isSpatial(area)
  dem <- exp_site_dem(area, dem = dem, z = z, buffer = buffer)
  packed <- exp_rayshader_matrix(dem, max_dim = max_dim)
  mat <- packed$mat
  ext <- packed$ext
  cell_m <- terra::res(packed$dem)[1]
  if (is.null(zscale)) {
    zscale <- cell_m / max(exaggerate, 1e-6)
    relief <- diff(range(mat, na.rm = TRUE))
    message(sprintf(
      "true scale: 1 cell = %.0f m = 1 scene unit, zscale = %.1f. DEM relief %.0f m, hub %.0f m. A %.0f m hill on a wide map looks almost flat -- that is 1:1, not a bug. Use exaggerate = 3 to stretch height.",
      cell_m, zscale, relief, hub_height, relief
    ))
  }
  xy <- matrix(as.numeric(exp_turbine_xy(turbines, sf::st_crs(area))), ncol = 2L)
  n <- nrow(xy)
  pts <- terra::vect(xy, type = "points", crs = terra::crs(packed$dem))
  elev <- as.numeric(terra::extract(packed$dem, pts, ID = FALSE)[[1]])
  na_fill <- min(terra::values(packed$dem), na.rm = TRUE)
  elev[is.na(elev)] <- na_fill
  cols <- exp_wake_cols(wake, n)
  wd <- exp_dominant_wd(wind)
  if (is.null(sunangle)) {
    sunangle <- if (is.null(wd)) 315 else wd
  }
  if (is.null(wake_length)) {
    wake_length <- 15 * 2 * rotor
  }

  if (rgl::cur3d() != 0) {
    rgl::close3d()
  }
  rayshader::sphere_shade(mat, texture = texture, sunangle = sunangle, zscale = zscale) |>
    rayshader::plot_3d(mat, zscale = zscale, zoom = zoom, water = water, ...)

  if (isTRUE(site_outline)) {
    ring <- sf::st_coordinates(sf::st_cast(sf::st_geometry(area), "POLYGON"))[, 1:2]
    ring <- exp_densify_xy(ring, step = max(cell_m / 2, 10))
    exp_render_path(
      ring, mat, ext, zscale, "#222222",
      altitude = exp_elev_along(ring, packed$dem, na_fill) + 8,
      linewidth = 2
    )
  }

  wind_to <- if (is.null(wd)) NULL else (wd + 180) %% 360
  obj_ok <- FALSE
  if (!is.null(turbine_obj) && file.exists(turbine_obj)) {
    obj_ok <- isTRUE(tryCatch(
      exp_place_obj_turbines(
        turbine_obj, xy, elev, hub_height, wind_to,
        mat, ext, zscale, turbine_color
      ),
      error = function(e) {
        warning("render_obj failed: ", conditionMessage(e), "; using stick turbines.")
        FALSE
      }
    ))
  }
  if (!obj_ok) {
    for (i in seq_len(n)) {
      exp_draw_turbine(
        xy[i, ], elev[i], hub_height, rotor, wind_to,
        mat, ext, zscale,
        color = turbine_color
      )
    }
  }

  if (!is.null(wd)) {
    dem_w <- min(ext[["xmax"]] - ext[["xmin"]], ext[["ymax"]] - ext[["ymin"]])
    z_arrow <- max(mat, na.rm = TRUE) + max(3 * hub_height, 0.06 * dem_w)
    anchor <- exp_upwind_rim(ext, wd)
    shaft_len <- 0.22 * dem_w
    tail <- exp_dest(anchor, wd, 0.45 * shaft_len)
    head <- exp_dest(anchor, wind_to, 0.55 * shaft_len)
    shaft <- rbind(tail, head)
    exp_render_path(shaft, mat, ext, zscale, "#ecf0f1", altitude = z_arrow, linewidth = 6)
    left <- exp_dest(head, wind_to + 150, 0.08 * dem_w)
    right <- exp_dest(head, wind_to - 150, 0.08 * dem_w)
    head3 <- rbind(left, head, right)
    exp_render_path(head3, mat, ext, zscale, "#ecf0f1", altitude = z_arrow, linewidth = 5)

    for (i in seq_len(n)) {
      src <- xy[i, , drop = FALSE]
      for (ang in c(wind_to - wake_angle, wind_to + wake_angle)) {
        dest <- exp_dest(src, ang, wake_length)
        xs <- cbind(
          seq(src[1], dest[1], length.out = 8),
          seq(src[2], dest[2], length.out = 8)
        )
        exp_render_path(
          xs, mat, ext, zscale, cols[i],
          altitude = exp_elev_along(xs, packed$dem, na_fill) + hub_height,
          linewidth = 2
        )
      }
    }
  }
  invisible(list(
    dem = packed$dem, heightmap = mat, turbines = xy,
    elev = elev, wind_from = wd, colors = cols
  ))
}

#' `plot_farm_3d` from a `genetic_algorithm()` result.
plot_farm_3d_from_result <- function(result, area, which = NULL, dem = NULL,
                                     z = 11, buffer = 3000,
                                     exaggerate = 1, turbine_obj = NULL,
                                     ...) {
  layout <- exp_layout_xy(result, which)
  inp <- result[1, "inputData"][[1]]
  if (is.list(inp) && !is.null(inp$Input_Data)) {
    inp <- inp$Input_Data
  }
  hub <- as.numeric(inp["Rotor Height", 1])
  rotor <- as.numeric(inp["Rotorradius", 1])
  xy <- as.matrix(layout[, c("X", "Y")])
  wake <- if ("AbschGesamt" %in% names(layout)) layout$AbschGesamt else layout[, "AbschGesamt"]
  wind <- result[1, "inputWind"][[1]]
  if (is.null(dem)) {
    tm <- tryCatch(windfarmGA:::ga_result_terrain(result), error = function(e) NULL)
    if (!is.null(tm) && !is.null(tm$srtm_crop)) {
      dem <- tm$srtm_crop[[1]]
    }
  }
  plot_farm_3d(
    area, xy,
    hub_height = hub, dem = dem, z = z, buffer = buffer,
    wind = wind, wake = wake, rotor = rotor,
    wake_angle = getOption("windfarmGA.max_angle", 20),
    exaggerate = exaggerate,
    turbine_obj = turbine_obj,
    ...
  )
}

if (FALSE) {
  library(sf)
  library(windfarmGA)
  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  plot_farm_3d_from_result(resultrect, sp_polygon, buffer = 4000)
}

message("loaded experimental/rayshader.R  (Y-up turbines, arrow on DEM rim)")
