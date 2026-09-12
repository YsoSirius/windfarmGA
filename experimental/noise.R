## ISO 9613-2 outdoor noise map (experimental, not in the CRAN package).
##
##   source("experimental/noise.R")
##   noise_from_result(result, area, lwa = 105)   # uses the GA wind rose
##   noise_map(area, turbines, wind = 90)         # wind from the east
##
## ISO 9613-2 itself is downwind only (worst case). Wind direction still
## matters physically: downwind rays bend to the ground, upwind a shadow
## grows with distance. `wind` adds that extra loss (not stretching
## distance). A rose is energy-weighted over directions.
##
## Not a legal immission study. No octave spectrum. Not in fitness / Suggests.

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

#' ISO 9613-2 Adiv + Aatm + Agr (alternative method, eq. 10).
#'
#' Adiv = 20 log10(d) + 11  (d in m, spherical, d0 = 1 m).
#' Aatm = alpha * d / 1000  (default alpha ~ 2 dB/km, ~500 Hz / 10 C / 70 % RH).
#' Agr  = max(0, 4.8 - (2 hm / d) * (17 + 300 / d)), hm = (hs + hr) / 2.
#'
#' @return A-weighted immission level L_AT in dB for each distance.
iso9613_level <- function(d, lwa = 105, hs = 100, hr = 4, alpha = 2) {
  d <- pmax(as.numeric(d), 1)
  hs <- as.numeric(hs)
  hr <- as.numeric(hr)
  Adiv <- 20 * log10(d) + 11
  Aatm <- alpha * d / 1000
  hm <- (hs + hr) / 2
  Agr <- 4.8 - (2 * hm / d) * (17 + 300 / d)
  Agr <- pmax(Agr, 0)
  as.numeric(lwa) - Adiv - Aatm - Agr
}

#' Energetic sum of levels (incoherent sources).
iso9613_sum <- function(levels) {
  10 * log10(rowSums(10^(0.1 * levels)))
}

#' Met bearing of the vector (dx east, dy north): 0 = north, 90 = east.
exp_bearing_from_north <- function(dx, dy) {
  (atan2(dx, dy) * (180 / pi)) %% 360
}

#' Extra attenuation vs ISO-downwind. 0 downwind, `upwind_db` fully upwind
#' at long range. Shadow grows as `1 - exp(-d / upwind_scale)`.
#' `wind_from` is meteorological (where the wind comes from).
iso9613_upwind <- function(dx, dy, d_xy, wind_from, upwind_db = 12,
                           upwind_scale = 500) {
  wind_to <- (as.numeric(wind_from) + 180) %% 360
  brg <- exp_bearing_from_north(dx, dy)
  delta <- ((brg - wind_to + 180) %% 360) - 180
  frac <- (1 - cos(delta * pi / 180)) / 2
  grow <- 1 - exp(-pmax(d_xy, 0) / upwind_scale)
  upwind_db * frac * grow
}

exp_wind_rose <- function(wind) {
  if (is.null(wind)) {
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
    wd <- as.numeric(wind) %% 360
    return(data.frame(wd = wd, probab = 100 / length(wd)))
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
    pr <- rep(100 / length(wd), length(wd))
  } else {
    pr <- as.numeric(wind[[pr_col]])
    pr <- pr * (100 / sum(pr))
  }
  data.frame(wd = wd, probab = pr)
}

#' Raster LAeq map for a site and turbine XY (projected metres).
#'
#' @param area Site polygon (`sf`), projected in metres.
#' @param turbines `sf` points or XY matrix in the same CRS.
#' @param lwa A-weighted sound power (dB). Recycled per turbine.
#' @param hub_height Source height hs (m).
#' @param receiver_height Receiver height hr (m). TA Lärm often uses 4 m.
#' @param res Cell size (m).
#' @param buffer Extra margin around the site (m) so dwellings off-site show.
#' @param alpha Atmospheric absorption (dB/km).
#' @param dem Optional elevation `SpatRaster` (same CRS) for barrier screening.
#' @param barrier If `TRUE`, add `abar_hidden` dB where `terra::viewshed`
#'   from the hub does not see the cell. Not the full ISO barrier formula.
#' @param abar_hidden Extra attenuation (dB) for screened cells.
#' @param wind `NULL` = ISO downwind everywhere (conservative). A number
#'   is `wd` (comes-from, 0 = N). A data.frame `wd` / `probab` (or a GA
#'   `inputWind` cell) is energy-weighted over the rose.
#' @param upwind_db Extra loss (dB) fully upwind at long range. Not ISO.
#' @param upwind_scale Distance (m) at which the upwind shadow builds up.
#' @param plot Draw the map.
noise_map <- function(area, turbines, lwa = 105, hub_height = 100,
                      receiver_height = 4, res = 50, buffer = 1000,
                      alpha = 2, dem = NULL, barrier = FALSE,
                      abar_hidden = 10, wind = NULL, upwind_db = 12,
                      upwind_scale = 500, plot = TRUE) {
  exp_need("terra")
  exp_need("sf")
  area <- windfarmGA::isSpatial(area)
  if (isTRUE(sf::st_is_longlat(area))) {
    stop("area must be projected in metres (not lon/lat).")
  }
  crs <- sf::st_crs(area)
  xy_t <- exp_turbine_xy(turbines, crs)
  n_t <- nrow(xy_t)
  lwa <- rep(as.numeric(lwa), length.out = n_t)
  hs <- rep(as.numeric(hub_height), length.out = n_t)

  pad <- sf::st_buffer(area, dist = buffer)
  bb <- sf::st_bbox(pad)
  r <- terra::rast(
    xmin = bb[["xmin"]], xmax = bb[["xmax"]],
    ymin = bb[["ymin"]], ymax = bb[["ymax"]],
    resolution = res,
    crs = terra::crs(area)
  )
  xy <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  dx <- outer(xy[, 1], xy_t[, 1], "-")
  dy <- outer(xy[, 2], xy_t[, 2], "-")
  d_xy <- sqrt(dx^2 + dy^2)
  d <- sqrt(d_xy^2 + outer(rep(receiver_height, nrow(xy)), hs, "-")^2)

  lp <- vapply(seq_len(n_t), function(j) {
    iso9613_level(d[, j], lwa = lwa[j], hs = hs[j], hr = receiver_height, alpha = alpha)
  }, numeric(nrow(xy)))
  if (is.null(dim(lp))) {
    lp <- matrix(lp, ncol = 1L)
  }

  if (isTRUE(barrier)) {
    if (is.null(dem)) {
      stop("barrier = TRUE needs a DEM (SpatRaster) in the site CRS.")
    }
    if (!inherits(dem, "SpatRaster")) {
      dem <- terra::rast(dem)
    }
    dem <- terra::project(dem, r)
    dem <- terra::resample(dem, r)
    for (j in seq_len(n_t)) {
      vis <- terra::viewshed(
        dem,
        loc = xy_t[j, ],
        observer = hs[j],
        target = receiver_height
      )
      hidden <- as.numeric(terra::values(vis)[, 1]) < 0.5
      hidden[is.na(hidden)] <- TRUE
      lp[hidden, j] <- lp[hidden, j] - abar_hidden
    }
  }

  if (isFALSE(wind)) {
    wind <- NULL
  }
  rose <- exp_wind_rose(wind)
  if (is.null(rose)) {
    laeq <- iso9613_sum(lp)
    wind_lab <- "ISO downwind (all directions)"
  } else {
    acc <- 0
    for (i in seq_len(nrow(rose))) {
      a_w <- iso9613_upwind(dx, dy, d_xy, rose$wd[i], upwind_db, upwind_scale)
      laeq_i <- iso9613_sum(lp - a_w)
      acc <- acc + (rose$probab[i] / 100) * 10^(0.1 * laeq_i)
    }
    laeq <- 10 * log10(acc)
    if (nrow(rose) == 1L) {
      wind_lab <- sprintf("wind from %s° (upwind extra ≤ %s dB)", rose$wd[1], upwind_db)
    } else {
      wind_lab <- sprintf("rose-weighted (%s dirs, upwind extra ≤ %s dB)", nrow(rose), upwind_db)
    }
  }
  terra::values(r) <- laeq
  names(r) <- "LAeq_dB"

  if (plot) {
    terra::plot(
      r,
      main = sprintf("ISO 9613-2 sketch  LWA = %s dB(A)\n%s", paste(unique(lwa), collapse = "/"), wind_lab),
      col = hcl.colors(20, "YlOrRd", rev = TRUE)
    )
    plot(sf::st_geometry(area), add = TRUE, border = "grey20")
    points(xy_t[, 1], xy_t[, 2], pch = 24, bg = "white", cex = 1.2)
    try(terra::contour(r, levels = c(35, 40, 45, 50), add = TRUE, labcex = 0.7), silent = TRUE)
  }
  r
}

#' Noise map from a `genetic_algorithm()` result (best layout of `which`).
#' Uses `inputWind` unless `wind` is set. `wind = FALSE` forces ISO downwind.
noise_from_result <- function(result, area, which = NULL, lwa = 105,
                              receiver_height = 4, res = 50, buffer = 1000,
                              alpha = 2, dem = NULL, barrier = FALSE,
                              wind = NULL, upwind_db = 12, upwind_scale = 500,
                              plot = TRUE) {
  layout <- exp_layout_xy(result, which)
  inp <- result[1, "inputData"][[1]]
  hub <- as.numeric(inp["Rotor Height", 1])
  xy <- as.matrix(layout[, c("X", "Y")])
  if (is.null(wind)) {
    wind <- result[1, "inputWind"][[1]]
  } else if (isFALSE(wind)) {
    wind <- NULL
  }
  noise_map(
    area = area,
    turbines = xy,
    lwa = lwa,
    hub_height = hub,
    receiver_height = receiver_height,
    res = res,
    buffer = buffer,
    alpha = alpha,
    dem = dem,
    barrier = barrier,
    wind = wind,
    upwind_db = upwind_db,
    upwind_scale = upwind_scale,
    plot = plot
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
  set.seed(1)
  turbines <- sf::st_sample(area, 8)
  r <- noise_map(area, turbines, lwa = 105, hub_height = 100, res = 40, wind = 90)
  ## From a GA run (uses the stored rose):
  ## noise_from_result(result, area, lwa = 105)
}
