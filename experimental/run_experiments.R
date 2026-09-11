## Test the GitHub-only extras and plot_viewshed() from a GA result.
## From the package root in R:
##   source("experimental/run_experiments.R")
##   run_experiments()              # skip interactive apps
##   run_experiments(interactive = TRUE)
##   viewshed_from_result(result, area)

need_pkg <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(TRUE)
  }
  message("Skip: install.packages(\"", pkg, "\")")
  FALSE
}

best_layout_xy <- function(result, which = 1L, by = c("energy", "efficiency")) {
  by <- match.arg(by)
  col <- if (identical(by, "energy")) "bestPaEn" else "bestPaEf"
  if (!col %in% colnames(result)) {
    stop("result has no ", col, " column.")
  }
  which <- as.integer(which)
  which <- min(max(1L, which), nrow(result))
  as.data.frame(result[which, col][[1]])
}

#' Viewshed for one stored layout (last generation = overall best energy).
#' @param result genetic_algorithm() output
#' @param area site polygon (same CRS as the run)
#' @param which generation index; default last
#' @param by "energy" (bestPaEn) or "efficiency" (bestPaEf)
#' @param z elevatr zoom (10–12 is usually enough)
viewshed_from_result <- function(result, area,
                                 which = nrow(result),
                                 by = "energy",
                                 z = 11,
                                 h1 = NULL,
                                 h2 = 1.7) {
  if (!need_pkg("elevatr") || !need_pkg("terra") || !need_pkg("sf")) {
    return(invisible(NULL))
  }
  layout <- best_layout_xy(result, which = which, by = by)
  inp <- result[1, "inputData"][[1]]
  if (is.null(h1)) {
    h1 <- as.numeric(inp["Rotor Height", ][[1]])
  }
  poly <- windfarmGA::isSpatial(area)
  xy <- as.matrix(layout[, c("X", "Y")])
  pts <- sf::st_as_sf(
    data.frame(X = xy[, 1], Y = xy[, 2]),
    coords = c("X", "Y"),
    crs = sf::st_crs(poly)
  )

  poly_wgs <- sf::st_transform(poly, 4326)
  dem <- elevatr::get_elev_raster(locations = poly_wgs, z = z, clip = "bbox")
  dem <- terra::rast(dem)
  dem <- terra::project(dem, terra::crs(poly))
  dem <- terra::crop(dem, terra::vect(poly), mask = TRUE)
  pts_dem <- pts

  message(
    "Viewshed: generation ", which, ", ", nrow(pts),
    " turbines, observer height h1 = ", h1, " m (hub), target h2 = ", h2, " m"
  )
  windfarmGA::plot_viewshed(dem, pts_dem, h1 = h1, h2 = h2, plot = TRUE)
}

run_experiments <- function(interactive = FALSE,
                            result = NULL,
                            area = NULL) {
  if (!requireNamespace("windfarmGA", quietly = TRUE)) {
    stop("load the package first: devtools::load_all() or library(windfarmGA)")
  }

  if (is.null(result) || is.null(area)) {
    result <- windfarmGA::resultrect
    area <- windfarmGA::sp_polygon
    message("Using package data resultrect / sp_polygon")
  }
  result <- windfarmGA::as_windfarmGA(result)

  message("\n== print / plot / options ==")
  print(result)
  windfarmGA::ga_options()
  curve <- data.frame(
    ws = c(0, 3, 4, 12, 25, 26),
    power = c(0, 0, 80, 2000, 2000, 0)
  )
  windfarmGA::plot_power_curve(curve)

  message("\n== plot_viewshed from best layout ==")
  vs <- try(
    viewshed_from_result(result, area, which = nrow(result)),
    silent = TRUE
  )
  if (inherits(vs, "try-error")) {
    message("Viewshed failed (often no elevation download): ", vs)
  }

  if (isTRUE(interactive)) {
    message("\n== explore_result (Shiny, close the app to continue) ==")
    if (need_pkg("shiny") && need_pkg("ggplot2")) {
      windfarmGA::explore_result(result, area)
    }

    root <- if (file.exists("experimental/draw_shape.R")) {
      "experimental"
    } else {
      file.path("..", "experimental")
    }
    message("\n== draw_shape (draw a polygon, then Esc/Done) ==")
    source(file.path(root, "draw_shape.R"), local = TRUE)
    message("Call draw_shape() yourself if you want a new site.")

    message("\n== circle_overlap_app ==")
    source(file.path(root, "circle_overlap_app.R"), local = TRUE)
    if (need_pkg("shiny") && need_pkg("ggplot2") && need_pkg("ggforce")) {
      circle_overlap_app()
    }
  } else {
    message("\nInteractive apps skipped. run_experiments(interactive = TRUE)")
  }

  message("\nDone. rayshader / noise stay in local _experiment/ — source those files yourself.")
  invisible(list(result = result, viewshed = if (inherits(vs, "try-error")) NULL else vs))
}

if (identical(environment(), globalenv()) && !length(sys.frames())) {
  message("Functions loaded. Example:\n  run_experiments()\n  viewshed_from_result(result, area)")
}

# run_experiments(interactive = T)
