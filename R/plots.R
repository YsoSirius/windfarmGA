#' @title Plot a Windrose
#' @name plot_windrose
#' @description  Plot a wind rose of the wind data frame.
#'
#' @export
#'
#' @param data A data.frame containing the wind information
#' @param spd The column of the wind speeds in "data"
#' @param dir The column of the wind directions in "data"
#' @param spdres The increment of the wind speed legend. Default is 2
#' @param dirres The size of the wind sectors. Default is 10
#' @param spdmin Minimum wind speed. Default is 1
#' @param spdmax Maximal wind speed. Default is 30
#' @param palette A color palette used for drawing the wind rose
#' @param spdseq A wind speed sequence, that is used for plotting
#' @param plot Should the windrose be plotted? Default is TRUE
#' @param plot Deprecated alias for \code{plot}.
#'
#' @family Plotting Functions
#' @return A ggplot2 wind rose plot, returned invisibly.
#'
#' @examples
#' ## Exemplary Input Wind speed and direction data frame
#' # Uniform wind speed and single wind direction
#' data.in <- data.frame(ws = 12, wd = 0)
#' windrosePlot <- plot_windrose(
#'   data = data.in, spd = data.in$ws,
#'   dir = data.in$wd
#' )
#'
#' # Random wind speeds and random wind directions
#' data.in <- data.frame(
#'   ws = sample(1:25, 10),
#'   wd = sample(1:260, 10)
#' )
#' windrosePlot <- plot_windrose(
#'   data = data.in, spd = data.in$ws,
#'   dir = data.in$wd
#' )
#'
plot_windrose <- function(data, spd, dir, spdres = 2, dirres = 10, spdmin = 1,
                          spdmax = 30, palette = "YlGnBu",
                          spdseq = NULL, plot = TRUE) {
  if (!is_ggplot2_installed()) {
    stop(
      "The package 'ggplot2' is required for this function, but it is not installed.\n",
      "Please install it with `install.packages('ggplot2')`"
    )
  }

  if (!missing(data) && exists("data")) {
    # Assume that we've been given a data frame. Lets find the correct columns
    if (length(colnames(data))) {
      accep_speed <- c("SPEED", "GESCH", "V", "WS")
      accep_direc <- c("DIR", "RICHT", "WD")
      sum_col_match <- sum(sapply(
        c(accep_speed, accep_direc), grepl,
        toupper(colnames(data))
      ))
      if (sum_col_match >= 2) {
        speed_match <- which(sapply(
          lapply(accep_speed, grepl, toupper(colnames(data))),
          any
        ))
        direc_match <- which(sapply(
          lapply(accep_direc, grepl, toupper(colnames(data))),
          any
        ))

        speed_index <- which(grepl(
          accep_speed[speed_match],
          toupper(colnames(data))
        ))
        direc_index <- which(grepl(
          accep_direc[direc_match],
          toupper(colnames(data))
        ))
        data[, c(speed_index[1], direc_index[1])]

        spd <- colnames(data)[speed_index]
        dir <- colnames(data)[direc_index]
      } else {
        col_numeric <- which(sapply(data[1, ], is.numeric))
        data <- data[, col_numeric]
        colnames(data) <- c("spd", "dir")
        spd <- "spd"
        dir <- "dir"
      }
    } else {
      col_numeric <- which(sapply(data[1, ], is.numeric))
      data <- data[, col_numeric]
      colnames(data) <- c("spd", "dir")
      spd <- "spd"
      dir <- "dir"
    }
  } else if (!missing(spd) && !missing(dir) &&
    is.numeric(spd) && is.numeric(dir)) {
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd, dir = dir)
    spd <- "spd"
    dir <- "dir"
  }

  # Tidy up input data #################
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA

  # figure out the wind speed bins #################
  if (missing(spdseq) || is.null(spdseq)) {
    spdseq <- seq(spdmin, spdmax, spdres)
  }

  # get some information about the number of bins, etc. #################
  seq_length <- length(spdseq)
  colorpal_n <- seq_length - 1

  # create the color map #################
  wind_colorpal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(
    min(max(3, colorpal_n), min(9, colorpal_n)), palette
  ))(colorpal_n)

  if (max(data[[spd]], na.rm = TRUE) > spdmax) {
    speed_brks <- c(spdseq, max(data[[spd]], na.rm = TRUE))
    speed_labls <- c(
      paste(
        c(spdseq[1:seq_length - 1]), "-",
        c(spdseq[2:seq_length])
      ),
      paste(spdmax, "-", max(data[[spd]], na.rm = TRUE))
    )
    wind_colorpal <- c(wind_colorpal, "grey50")
  } else {
    speed_brks <- spdseq
    speed_labls <- paste(
      c(spdseq[1:seq_length - 1]), "-",
      c(spdseq[2:seq_length])
    )
  }
  speed_bins <- cut(
    x = data[[spd]], breaks = speed_brks,
    labels = speed_labls, ordered_result = TRUE
  )

  # figure out the wind direction bins #################
  dir_brks <- c(-dirres / 2, seq(dirres / 2, 360 - dirres / 2,
    by = dirres
  ), 360 + dirres / 2)
  dir_labls <- c(
    paste(360 - dirres / 2, "-", dirres / 2),
    paste(
      seq(dirres / 2, 360 - 3 * dirres / 2,
        by = dirres
      ), "-",
      seq(3 * dirres / 2, 360 - dirres / 2, by = dirres)
    ),
    paste(360 - dirres / 2, "-", dirres / 2)
  )
  # assign each wind direction to a bin
  dir_bins <- cut(data[[dir]],
    breaks = dir_brks,
    ordered_result = TRUE
  )
  levels(dir_bins) <- dir_labls
  data$dir_bins <- dir_bins

  # create the plot #################
  plot_windrose <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = dir_bins,
      fill = speed_bins
    )
  ) +
    ggplot2::geom_bar() +
    ggplot2::scale_x_discrete(drop = FALSE, labels = ggplot2::waiver()) +
    ggplot2::coord_polar(start = -((dirres / 2) / 360) * 2 * pi) +
    ggplot2::scale_fill_manual(
      name = "Wind Speed (m/s)",
      values = wind_colorpal,
      drop = FALSE
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = 0.3),
      panel.border = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      strip.background = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(0, 0, 0, 0), "lines")
    )


  if (plot) {
    print_ggplot_or_font_hint(plot_windrose)
  }

  # return the handle to the wind rose #################
  invisible(plot_windrose)
}


#' @title Plot the best results
#' @name plot_result
#' @description Draw the best layout(s) on the site. Turbine labels are the
#'   total wake in percent. Default is the single best energy layout.
#'
#' @export
#'
#' @inheritParams genetic_algorithm
#' @param result The output of \code{\link{genetic_algorithm}}
#' @param best How many distinct best layouts to draw. Default is 1.
#' @param plot_en A numeric value that indicates if the best energy or efficiency
#'   output should be plotted. \code{1} plots the best energy solutions
#'   and \code{2} plots the best efficiency solutions
#' @param terrain Draw terrain rasters for the best layout. Reuses
#'   `result$terrainModel` when present; `TRUE` downloads only if
#'   nothing is stored. A DEM raster rebuilds the model.
#' @param plot_grid If `TRUE` (default) the used grid is added. You can also
#'   pass another Simple Feature object
#'
#' @family Plotting Functions
#' @return Returns a data.frame of the best (energy/efficiency) individual
#'   during all iterations
#'
#' @examples \dontrun{
#' ## Add some data examples from the package
#' library(sf)
#' area <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)
#'   ))),
#'   crs = 3035
#' ))
#'
#' ## Plot the results of a hexagonal grid optimization
#' plot_result(resulthex, area, best = 1, plot_en = 1, terrain = FALSE)
#'
#' ## Plot the results of a rectangular grid optimization
#' plot_result(resultrect, area, best = 1, plot_en = 1, terrain = FALSE)
#' }
plot_result <- function(result, area, best = 1, plot_en = 1,
                        terrain = FALSE, plot_grid = TRUE,
                        ccl_roughness = NULL, ccl = NULL,
                        weibull_src = NULL) {
  terrainhie <- terrain
  Grid <- plot_grid
  ## Check plot_en, set par() and color palette ##############
  if (!plot_en %in% c(1, 2)) {
    stop(
      "plot_en must be either 1 or 2. \n",
      "1 - plots the best energy output. \n",
      "2 - plots the best efficiency output."
    )
  }
  ## set Graphic Params ###############
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(1, 1), mar = c(4.5, 4.5, 3.5, 1.5), mgp = c(2.5, 0.8, 0))
  rbPal1 <- grDevices::colorRampPalette(c("#27AE60", "#C0392B"))
  result_inputs <- result[1, "inputData"][[1]]

  ## Check Projections and reference systems ####
  area <- isSpatial(area)
  Projection <- result_inputs["Projection", ][[1]]
  Projection <- tryCatch(as.integer(Projection),
    warning = function(e) Projection,
    error = function(e) Projection
  )
  if (is.na(st_crs(area))) {
    message("Polygon is not projected. The spatial reference WGS 84 (EPSG:4326) is assumed.")
    st_crs(area) <- 4326
  }
  area <- sf::st_transform(area, st_crs(Projection))

  ## Check Weibull Rasters #########
  if (is.null(weibull_src)) {
    weibull_src <- NULL
    col2res <- "lightblue"
  } else {
    wbl_crs <- weibull_src[[1]]
    if (!inherits(wbl_crs, "SpatRaster")) {
      wbl_crs <- terra::rast(wbl_crs)
    }
    PolyCrop <- sf::st_transform(area, sf::st_crs(wbl_crs))
    if (inherits(weibull_src, "list") && length(weibull_src) == 2) {
      wblcroped <- lapply(weibull_src, function(x) {
        if (!inherits(x, "SpatRaster")) {
          x <- terra::rast(x)
        }
        terra::crop(x, PolyCrop, mask = TRUE)
      })
      Erwartungswert <- wblcroped[[2]] * (gamma(1 + (1 / values(wblcroped[[1]]))))
    } else if (length(weibull_src) == 1) {
      if (!inherits(weibull_src[[1]], "SpatRaster")) {
        weibull_src[[1]] <- terra::rast(weibull_src[[1]])
      }
      wblcroped <- terra::crop(weibull_src[[1]], PolyCrop, mask = TRUE)
      Erwartungswert <- wblcroped[[1]]
    } else if (inherits(weibull_src, "RasterLayer")) {
      wblcroped <- terra::crop(terra::rast(weibull_src), PolyCrop, mask = TRUE)
      Erwartungswert <- wblcroped
    }
    col2res <- "transparent"
    alpha <- 0.9
    Erwartungswert <- terra::project(Erwartungswert, terra::crs(area))
  }

  ## Check & Make Grid #############
  if (isTRUE(Grid)) {
    cellsize <- as.numeric(result_inputs["Resolution", ][[1]])
    if (toupper(result_inputs["Grid Method", ][[1]]) == "RECTANGULAR") {
      Grid <- grid_area(area,
        size = cellsize,
        prop = as.numeric(result_inputs["Percentage of Polygon", ][[1]])
      )[[2]]
    } else {
      Grid <- hexa_area(area, size = cellsize)[[2]]
    }
  }


  ## Check Terrain Modell #########
  if (isTRUE(terrainhie) || terrain_is_dem(terrainhie)) {
    terrain_data <- terrain_resolve(
      result, terrainhie, area, ccl, ccl_roughness,
      plot = is.null(ga_result_terrain(result)) && !terrain_is_dem(terrainhie),
      verbose = FALSE
    )
    cclRaster <- terrain_data$cclRaster
    orogr1 <- terrain_data$srtm_crop$orogr1
    srtm_crop <- terrain_data$srtm_crop$strm_crop
  }

  ## Set Argments for Best Energy/Efficiency Windfarm ##########
  if (plot_en == 1) {
    filter_col <- "EnergyOverall"
    listind <- 2
    title <- "Energy"
  }
  if (plot_en == 2) {
    filter_col <- "EfficAllDir"
    listind <- 3
    title <- "Efficiency"
  }

  ## Get Energy/Efficiency Output for every windfarm ###########
  energy_order <- unlist(lapply(result[, listind], function(x) x[, filter_col][[1]]))
  energy_order <- order(energy_order, decreasing = FALSE)

  ## Order List by Energy/Efficiency Output #########
  result <- result[, listind][energy_order]
  ledup <- length(result)

  ## Check for Duplicates #########
  rectid <- lapply(result, function(x) x[, "Rect_ID"])
  rectidt <- !duplicated(rectid)
  result <- result[rectidt]
  ndif <- length(result)
  if (ndif < ledup) {
    message(ndif, " distinct layouts (", ledup - ndif, " duplicates skipped)")
  }

  ## Check for enough results #########
  if (ndif < best) {
    message("Fewer distinct layouts than `best`; showing ", max(1, trunc(ndif / 2)), ".")
    best <- trunc(ndif / 2)
  }
  if (best == 0) best <- 1

  ## Pick the `best` results to plot and Loop over #########
  result <- result[(length(result) - best + 1):(length(result))]
  for (i in seq_along(result)) {
    ## Get result ###########
    best_result <- data.frame(result[[i]])
    best_result$EnergyOverall <- round(best_result[, "EnergyOverall"], 2)
    best_result$EfficAllDir <- round(best_result[, "EfficAllDir"], 2)

    ## Color code locations #########
    br <- length(levels(factor(best_result[, "AbschGesamt"])))
    if (br > 1) {
      Col <- rbPal1(br)[as.numeric(cut(as.numeric(
        best_result[, "AbschGesamt"]
      ), breaks = br))]
    } else {
      Col <- "green"
    }

    rank_i <- (best + 1) - i
    par(mfrow = c(1, 1), ask = FALSE)
    plot(st_geometry(area),
      col = col2res,
      main = sprintf(
        "%s layout #%d   |   %s kW   |   %s%% efficiency",
        title, rank_i, best_result$EnergyOverall[[1]], best_result$EfficAllDir[[1]]
      ),
      cex.main = 1
    )
    if (best > 1 && i > 1) {
      par(ask = TRUE)
    }

    ## Plot Weibull Data ###########
    if (!is.null(weibull_src)) {
      terra::plot(Erwartungswert,
        alpha = alpha, legend = TRUE, axes = FALSE,
        useRaster = TRUE, add = TRUE,
        legend.lab = "Mean Wind Speed"
      )
    }

    ## Plot Grid  ###########
    if (inherits(Grid, "sf") || inherits(Grid, "sfc")) {
      plot(Grid, add = TRUE)
    }

    ## Plot Turbines and additional Info ###########
    graphics::points(best_result[, "X"], best_result[, "Y"],
      cex = 2, pch = 20, col = Col
    )
    graphics::text(best_result[, "X"], best_result[, "Y"],
      round(best_result[, "AbschGesamt"], 0),
      cex = 0.75, pos = 1, col = "black"
    )

    distpo <- stats::dist(
      x = cbind(best_result[, "X"], best_result[, "Y"]),
      method = "euclidian"
    )
    graphics::mtext(
      sprintf("Wake %% at turbines   |   min dist %s   mean dist %s",
              round(min(distpo), 1), round(mean(distpo), 1)),
      side = 1, line = 2, cex = 0.85
    )

    ## Plot Terrain Model  ###########
    if (isTRUE(terrainhie)) {
      par(ask = TRUE)
      sel1 <- best_result[, 1:2]
      plot_terrain(result_inputs, sel1, area, orogr1, srtm_crop, cclRaster)
    }
  }

  ## Reset par() and return best windfarm  ###########
  invisible(best_result)
}
plot_terrain <- function(inputs, sel1, polygon1, orogr1, srtm_crop, cclRaster) {
  ## Plot DEM and windspeed multiplier ############
  orogrnum <- terra::extract(x = orogr1, y = as.matrix(sel1))
  windpo <- 1 * orogrnum
  ## Get Elevation of Turbine Locations to estimate the air density at the resulting height
  heightWind <- terra::extract(x = srtm_crop, y = as.matrix(sel1))
  ## set Graphic Params ###############
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(1, 2))
  cexa <- 0.7
  terra::plot(srtm_crop, main = "Elevation Data")
  graphics::points(sel1[, "X"], sel1[, "Y"], pch = 20)
  calibrate::textxy(sel1[, "X"], sel1[, "Y"],
    labs = round(heightWind[[1]], 0), cex = cexa
  )
  plot(polygon1, add = TRUE)
  terra::plot(orogr1, main = "Wind Speed Multipliers")
  points(sel1[, "X"], sel1[, "Y"], pch = 20)
  calibrate::textxy(sel1[, "X"], sel1[, "Y"],
    labs = round(windpo[[1]], 3), cex = cexa
  )
  plot(polygon1, add = TRUE)

  ## Get Air Density and Pressure from Height Values #########
  HeighttoBaro <- matrix(heightWind[[1]])
  colnames(HeighttoBaro) <- "HeighttoBaro"
  air_dt <- barometric_height(matrix(HeighttoBaro), HeighttoBaro)
  terra::plot(srtm_crop,
    main = "Normal Air Density",
    col = topo.colors(10)
  )
  points(sel1[, "X"], sel1[, "Y"], pch = 20)
  calibrate::textxy(sel1[, "X"], sel1[, "Y"],
    labs = rep(1.225, nrow(sel1)), cex = cexa
  )
  plot(polygon1, add = TRUE)
  terra::plot(srtm_crop,
    main = "Corrected Air Density",
    col = topo.colors(10)
  )
  points(sel1[, "X"], sel1[, "Y"], pch = 20)
  calibrate::textxy(sel1[, "X"], sel1[, "Y"],
    labs = round(air_dt[, "rh"], 2), cex = cexa
  )
  plot(polygon1, add = TRUE)

  ## CorineLandCover Roughness values ##################
  surface_roughness0 <- terra::extract(x = cclRaster, y = as.matrix(sel1))
  surface_roughness1 <- terra::extract(
    x = terra::terrain(srtm_crop, "roughness"),
    y = as.matrix(sel1)
  )
  surface_roughness <- surface_roughness0 * (1 + (surface_roughness1[[1]] / max(terra::res(srtm_crop))))
  elrouind <- terra::terrain(srtm_crop, "roughness")
  elrouindn <- terra::resample(elrouind, cclRaster, method = "near")
  modSurf <- cclRaster * (1 + (values(elrouindn) / max(terra::res(srtm_crop))))

  par(mfrow = c(1, 2))
  terra::plot(cclRaster, main = "Corine Land Cover Roughness")
  points(sel1[, "X"], sel1[, "Y"], pch = 20)
  calibrate::textxy(sel1[, "X"], sel1[, "Y"],
    labs = round(surface_roughness0[[1]], 2), cex = cexa
  )
  plot(polygon1, add = TRUE)
  terra::plot(
    x = elrouindn,
    main = "Elevation Roughness Indicator"
  )
  points(sel1[, "X"], sel1[, "Y"], pch = 20)
  calibrate::textxy(sel1[, "X"], sel1[, "Y"],
    labs = round((surface_roughness1[[1]]), 2), cex = cexa
  )
  plot(polygon1, add = TRUE)
  terra::plot(modSurf, main = "Modified Surface Roughness")
  points(sel1[, "X"], sel1[, "Y"], pch = 20)
  calibrate::textxy(sel1[, "X"], sel1[, "Y"],
    labs = round((surface_roughness[[1]]), 2), cex = cexa
  )
  plot(polygon1, add = TRUE)

  ## Wake Decay Constant #############
  rotor_height <- as.integer(inputs["Rotor Height", ])
  k_raster <- terra::app(modSurf, function(x) {
    0.5 / (log(rotor_height / x))
  })
  # New Wake Decay Constant calculated with new surface roughness values, according to CLC
  k <- 0.5 / (log(rotor_height / surface_roughness))
  terra::plot(k_raster, main = "Adapted Wake Decay Constant - K")
  points(sel1[, "X"], sel1[, "Y"], pch = 20)
  calibrate::textxy(sel1[, "X"], sel1[, "Y"], labs = round(k[[1]], 3), cex = cexa)
  plot(polygon1, add = TRUE)
}

ga_series <- function(result) {
  rslt <- as.data.frame(do.call("rbind", result[, "allparkcoeff"]))
  n <- nrow(rslt)
  pad_n <- function(x) {
    x <- as.numeric(x)
    if (!length(x)) {
      return(rep(NA_real_, n))
    }
    if (length(x) < n) {
      x <- c(x, rep(utils::tail(x, 1), n - length(x)))
    }
    x[seq_len(n)]
  }

  mut <- if ("mut_rate" %in% colnames(result)) {
    pad_n(unlist(result[, "mut_rate"]))
  } else {
    rep(NA_real_, n)
  }

  sc <- if ("selcross" %in% colnames(result)) result[, "selcross"] else result[, 8]
  cross_raw <- pad_n(vapply(sc, function(x) as.numeric(x)[1], numeric(1)))
  teil <- pad_n(vapply(sc, function(x) as.numeric(x)[2], numeric(1)))
  mx <- suppressWarnings(max(cross_raw, na.rm = TRUE))
  inject <- if (is.finite(mx) && mx > 1) cross_raw / 10 else cross_raw
  inject[!is.finite(inject)] <- NA_real_
  inject <- pmax(0, inject)

  sel_pct <- 100 / teil
  sel_pct[!is.finite(sel_pct)] <- NA_real_

  coverage <- rep(NA_real_, n)
  bw <- tryCatch(
    do.call("rbind", result[, if ("beorwor" %in% colnames(result)) "beorwor" else 9]),
    error = function(e) NULL
  )
  if (!is.null(bw)) {
    bw <- as.matrix(bw)
    if (ncol(bw) >= 2) {
      coverage <- pad_n(bw[, 2])
      cmx <- suppressWarnings(max(coverage, na.rm = TRUE))
      if (is.finite(cmx) && cmx <= 1.5) {
        coverage <- coverage * 100
      }
    }
  }

  rec <- cummax(rslt$maxparkfitness)
  improved <- c(TRUE, rec[-1] > rec[-n] + 1e-8)

  list(
    n = n,
    gen = seq_len(n),
    fit_max = rslt$maxparkfitness,
    fit_mean = rslt$meanparkfitness,
    fit_min = rslt$minparkfitness,
    ene_max = rslt$MaxEnergyRedu,
    ene_mean = rslt$MeanEnergyRedu,
    ene_min = rslt$MinEnergyRedu,
    eff_max = rslt$maxParkwirkungsg,
    eff_mean = rslt$meanParkwirkungsg,
    eff_min = rslt$minParkwirkungsg,
    inject = inject,
    mut = mut,
    sel_pct = sel_pct,
    coverage = coverage,
    improved = improved
  )
}

ga_elite_n <- function(result) {
  n_el <- 3L
  if ("inputData" %in% colnames(result)) {
    inp <- tryCatch(result[1, "inputData"][[1]], error = function(e) NULL)
    if (!is.null(inp) && "Elite count" %in% rownames(inp)) {
      n_el <- suppressWarnings(as.integer(inp["Elite count", ][[1]]))
    }
  }
  if (!is.finite(n_el) || n_el < 1L) {
    n_el <- 3L
  }
  n_el
}

print_ggplot_or_font_hint <- function(p) {
  tryCatch(
    print(p),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("font_info", msg, fixed = TRUE) ||
        grepl("unused argument \\(weight", msg)) {
        stop(
          "ggplot2 4 / textshaping need a current 'systemfonts' ",
          "(font_info() gained a weight argument).\n",
          "Update with install.packages(\"systemfonts\")",
          call. = FALSE
        )
      }
      stop(e)
    }
  )
}

ga_plot_theme <- function(legend = "right") {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = legend,
      legend.justification = if (identical(legend, "bottom")) "center" else "top",
      legend.direction = if (identical(legend, "bottom")) "horizontal" else "vertical",
      legend.title = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 12)
    )
}

ga_result_grid <- function(result, area) {
  result_inputs <- result[1, "inputData"][[1]]
  area <- isSpatial(area)
  Projection <- result_inputs["Projection", ][[1]]
  Projection <- tryCatch(as.integer(Projection),
    warning = function(e) Projection,
    error = function(e) Projection
  )
  if (is.na(sf::st_crs(area))) {
    sf::st_crs(area) <- 4326
  }
  area <- sf::st_transform(area, sf::st_crs(Projection))
  cellsize <- as.numeric(result_inputs["Resolution", ][[1]])
  prop <- as.numeric(result_inputs["Percentage of Polygon", ][[1]])
  if (toupper(result_inputs["Grid Method", ][[1]]) == "RECTANGULAR") {
    Grid <- grid_area(area, size = cellsize, prop = prop)
  } else {
    Grid <- hexa_area(area, size = cellsize)
  }
  list(polygon = area, grid_xy = Grid[[1]], grid_poly = Grid[[2]])
}

maybe_plotly <- function(plots, use) {
  if (!isTRUE(use) || !is_plotly_installed()) {
    return(NULL)
  }
  tryCatch({
    ply <- lapply(plots, function(p) {
      plotly::ggplotly(p, tooltip = c("x", "y", "colour"))
    })
    if (length(ply) == 1L) {
      return(ply[[1]])
    }
    plotly::layout(
      plotly::subplot(
        ply, nrows = length(ply), shareX = TRUE, titleY = TRUE, margin = 0.08
      ),
      legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left")
    )
  }, error = function(e) NULL)
}

use_plotly <- function(flag) {
  if (is.null(flag)) {
    is_plotly_installed()
  } else {
    isTRUE(flag) && is_plotly_installed()
  }
}

pause_next_plot <- function(ask) {
  if (isTRUE(ask)) {
    invisible(readline("Press [enter] for the next plot"))
  }
}

show_plot_pages <- function(plots, ask = FALSE, plotly = FALSE) {
  plots <- plots[!vapply(plots, is.null, logical(1))]
  if (!length(plots)) {
    return(invisible(plots))
  }
  use_ply <- isTRUE(plotly) && !isTRUE(ask)
  if (use_ply) {
    ply <- maybe_plotly(plots, TRUE)
    if (!is.null(ply)) {
      print(ply)
      return(invisible(ply))
    }
  }
  for (i in seq_along(plots)) {
    if (i > 1L) {
      pause_next_plot(ask)
    }
    print(plots[[i]])
  }
  invisible(plots)
}

#' @title Plot the results of an optimization run
#' @name plot_windfarmGA
#' @description Draw the useful summary plots of a GA run, one after another:
#'   best layout, fitness, operator rates, population, cells, efficiency,
#'   and the cell heatmap. In an interactive session every page waits for
#'   Enter so nothing is overwritten in the Plots pane.
#'
#' @export
#'
#' @inheritParams plot_result
#' @param which_plot `"all"` (default) shows `result`, `progress`, `population`
#'   and `heatmap`. Or a character vector (`"result"`, `"progress"`,
#'   `"population"`, `"heatmap"`, `"evolution"`) or the numbers 1-4.
#' @param ask If `TRUE`, wait for Enter between pages. Default is `TRUE`
#'   in an interactive session.
#' @param plotly If `TRUE`, draw fitness and rates with plotly (hover).
#'   Used only when `ask` is `FALSE` and plotly is installed.
#'
#' @family Plotting Functions
#' @return Returns NULL. Used for plotting
#' @examples \dontrun{
#' library(sf)
#' area <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)
#'   ))),
#'   crs = 3035
#' ))
#'
#' plot_windfarmGA(resulthex, area)
#' plot_windfarmGA(resultrect, area, which_plot = "progress")
#' }
plot_windfarmGA <- function(result, area, which_plot = "all",
                            best = 1, plot_en = 1,
                            weibull_src = NULL, ask = NULL, plotly = NULL) {
  whichPl <- which_plot
  oldpar <- graphics::par(ask = FALSE, no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  if (is.null(ask)) {
    ask <- interactive()
  }
  area <- isSpatial(area)

  if (length(whichPl) == 1 && identical(tolower(as.character(whichPl)), "all")) {
    whichPl <- c("result", "progress", "population", "heatmap")
  }
  if (is.numeric(whichPl)) {
    lab <- c("result", "progress", "population", "heatmap")
    whichPl <- unique(lab[pmin(pmax(as.integer(whichPl), 1L), 4L)])
  }
  whichPl <- unique(tolower(as.character(whichPl)))
  whichPl[whichPl %in% c("parkfitness", "fitness")] <- "progress"
  whichPl[whichPl %in% c("cell", "cells")] <- "heatmap"
  whichPl[whichPl %in% c("census", "pop")] <- "population"
  whichPl <- whichPl[whichPl %in% c(
    "result", "progress", "population", "evolution", "heatmap"
  )]

  for (i in seq_along(whichPl)) {
    pg <- whichPl[[i]]
    if (pg == "result") {
      plot_result(
        result = result, area = area, best = best, plot_en = plot_en,
        terrain = FALSE, plot_grid = TRUE, weibull_src = weibull_src
      )
    } else if (pg == "progress") {
      plot_parkfitness(result, interactive = use_plotly(plotly), ask = ask)
    } else if (pg == "population") {
      plot_population(result, interactive = use_plotly(plotly), ask = ask)
    } else if (pg == "evolution") {
      plot_evolution(result, ask = FALSE)
    } else if (pg == "heatmap") {
      plot_cell_heatmap(result, area)
    }
    if (i < length(whichPl)) {
      pause_next_plot(ask)
    }
  }
  invisible(NULL)
}

#' @title Heatmap of probed grid cells
#' @name plot_cell_heatmap
#' @description Count how often each grid cell appeared in an evaluated layout
#'   over all generations (`allCoords` in the GA result). Never-tried cells
#'   stay light gray. Useful to see whether the search covered the area or
#'   stuck to a few sites.
#'
#' @export
#'
#' @inheritParams plot_result
#' @param log If `TRUE`, color the counts on a `log1p` scale so a few elite
#'   cells do not dominate the palette. Default is `TRUE`
#' @param plot If `FALSE`, only return the counts. Default is `TRUE`
#' @param plot Deprecated alias for `plot`.
#'
#' @family Plotting Functions
#' @return A `data.frame` with grid ID, coordinates and visit count
#'   (`n_probed`), returned invisibly.
#' @examples \donttest{
#' plot_cell_heatmap(resultrect, sp_polygon)
#' }
plot_cell_heatmap <- function(result, area, log = TRUE, plot = TRUE) {
  if (!"allCoords" %in% colnames(result)) {
    stop("result has no allCoords column. Run genetic_algorithm() first.")
  }
  parks <- do.call("rbind", result[, "allCoords"])
  if (is.null(parks) || !nrow(parks) || !"Rect_ID" %in% colnames(parks)) {
    stop("allCoords has no Rect_ID column.")
  }
  counts <- table(as.integer(parks[, "Rect_ID"]))

  result_inputs <- result[1, "inputData"][[1]]
  area <- isSpatial(area)
  Projection <- result_inputs["Projection", ][[1]]
  Projection <- tryCatch(as.integer(Projection),
    warning = function(e) Projection,
    error = function(e) Projection
  )
  if (is.na(sf::st_crs(area))) {
    sf::st_crs(area) <- 4326
  }
  area <- sf::st_transform(area, sf::st_crs(Projection))

  cellsize <- as.numeric(result_inputs["Resolution", ][[1]])
  prop <- as.numeric(result_inputs["Percentage of Polygon", ][[1]])
  if (toupper(result_inputs["Grid Method", ][[1]]) == "RECTANGULAR") {
    Grid <- grid_area(area, size = cellsize, prop = prop)
  } else {
    Grid <- hexa_area(area, size = cellsize)
  }
  grid_xy <- Grid[[1]]
  grid_poly <- Grid[[2]]
  ncell <- nrow(grid_xy)
  vis <- integer(ncell)
  m <- match(as.integer(names(counts)), grid_xy[, "ID"])
  ok <- !is.na(m)
  vis[m[ok]] <- as.integer(counts)[ok]

  out <- data.frame(
    ID = grid_xy[, "ID"],
    X = grid_xy[, "X"],
    Y = grid_xy[, "Y"],
    n_probed = vis
  )

  if (isTRUE(plot)) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    graphics::par(mar = c(4, 4, 4, 6) + 0.1)

    cols <- rep("#F0F0F0", ncell)
    pal <- grDevices::colorRampPalette(c("#FEE8C8", "#FDBB84", "#E34A33", "#7F0000"))
    pos <- vis > 0
    if (any(pos)) {
      v <- vis[pos]
      if (isTRUE(log)) {
        v <- log1p(v)
      }
      pal_n <- 100
      ramp <- pal(pal_n)
      if (length(unique(v)) == 1) {
        cols[pos] <- ramp[pal_n]
      } else {
        idx <- as.integer(cut(v, breaks = pal_n, include.lowest = TRUE))
        cols[pos] <- ramp[idx]
      }
    }

    n_zero <- sum(vis == 0)
    main <- "Grid cells probed during the GA"
    sub <- sprintf(
      "%d / %d cells never tried (%.0f%%)",
      n_zero, ncell, 100 * n_zero / ncell
    )
    plot(sf::st_geometry(area), col = "white", border = "grey30",
         main = main, sub = sub)
    plot(grid_poly, col = cols, border = "white", lwd = 0.4, add = TRUE)
    plot(sf::st_geometry(area), add = TRUE, border = "grey20", col = NA)

    n_leg <- 6
    mx <- max(vis)
    leg_vals <- unique(round(seq(0, mx, length.out = n_leg)))
    if (isTRUE(log)) {
      leg_v <- log1p(leg_vals)
      if (max(leg_v) > 0) {
        leg_idx <- pmax(1L, pmin(100L, as.integer(1 + 99 * leg_v / max(leg_v))))
      } else {
        leg_idx <- rep(1L, length(leg_vals))
      }
    } else {
      if (mx > 0) {
        leg_idx <- pmax(1L, pmin(100L, as.integer(1 + 99 * leg_vals / mx)))
      } else {
        leg_idx <- rep(1L, length(leg_vals))
      }
    }
    leg_cols <- pal(100)[leg_idx]
    leg_cols[leg_vals == 0] <- "#F0F0F0"
    graphics::legend(
      "topright", legend = as.character(leg_vals), fill = leg_cols,
      title = if (isTRUE(log)) "n (log colors)" else "n probed",
      bty = "n", cex = 0.8, inset = 0.02
    )
  }
  invisible(out)
}

#' @title Layouts evaluated in one generation
#' @name generation_layouts
#' @description Return every individual that was fitness-evaluated in a given
#'   generation (`allCoords`). Duplicate layouts (same cell IDs) are flagged.
#' @export
#'
#' @inheritParams plot_result
#' @param generation Generation index (1 = first). Default is the last
#'   generation in `result`.
#'
#' @family Plotting Functions
#' @return A list with `turbines` (one row per turbine), `layouts` (one row
#'   per individual) and `generation`.
#' @examples \donttest{
#' generation_layouts(resultrect, generation = 10)
#' }
generation_layouts <- function(result, generation = NULL) {
  if (!"allCoords" %in% colnames(result)) {
    stop("result has no allCoords column. Run genetic_algorithm() first.")
  }
  n_gen <- nrow(result)
  if (is.null(generation)) {
    generation <- n_gen
  }
  generation <- as.integer(generation)
  if (generation < 1L || generation > n_gen) {
    stop("generation must be between 1 and ", n_gen, ".")
  }
  parks <- result[generation, "allCoords"][[1]]
  if (is.null(parks) || !nrow(parks)) {
    stop("Generation ", generation, " has no stored layouts.")
  }
  parks <- as.data.frame(parks)
  if (!"Run" %in% names(parks)) {
    stop("allCoords has no Run column.")
  }
  runs <- unique(parks$Run)
  layouts <- do.call(rbind, lapply(runs, function(r) {
    one <- parks[parks$Run == r, , drop = FALSE]
    data.frame(
      Run = r,
      EnergyOverall = one$EnergyOverall[[1]],
      EfficAllDir = one$EfficAllDir[[1]],
      Parkfitness = one$Parkfitness[[1]],
      n_turb = nrow(one),
      ids = paste(sort(as.integer(one$Rect_ID)), collapse = ","),
      stringsAsFactors = FALSE
    )
  }))
  layouts <- layouts[order(layouts$Parkfitness, decreasing = TRUE), , drop = FALSE]
  layouts$rank <- seq_len(nrow(layouts))
  layouts$unique <- !duplicated(layouts$ids)
  layouts$elite <- layouts$rank <= ga_elite_n(result)
  list(turbines = parks, layouts = layouts, generation = generation)
}

#' @title Plot all layouts of one generation
#' @name plot_generation
#' @description Occupancy map of one generation, then the distinct layouts
#'   as small maps. In an interactive session each page waits for Enter so
#'   you can step through every distinct layout (`n_show` maps per page).
#'   Non-interactive calls only draw the first `n_show` maps.
#' @export
#'
#' @inheritParams plot_result
#' @param generation Generation index (1 = first). Default is the last
#'   generation.
#' @param n_show Distinct layouts per page (and, if `ask` is `FALSE`, how
#'   many to draw in total). Default is 6. Set to 0 to skip the maps.
#' @param interactive Use plotly for the occupancy map when available.
#' @param ask If `TRUE`, wait for Enter and page through all distinct
#'   layouts. Default is `TRUE` in an interactive session.
#'
#' @family Plotting Functions
#' @return The list from \code{\link{generation_layouts}}, invisibly.
#' @examples \donttest{
#' plot_generation(resultrect, sp_polygon, generation = 10)
#' }
plot_generation <- function(result, area, generation = NULL,
                            n_show = 6, interactive = NULL, ask = NULL) {
  dat <- generation_layouts(result, generation)
  if (is.null(ask)) {
    ask <- interactive()
  }
  interactive <- if (isTRUE(ask)) FALSE else use_plotly(interactive)
  site <- ga_result_grid(result, area)
  parks <- dat$turbines
  lay <- dat$layouts
  best_run <- lay$Run[1]
  best_xy <- parks[parks$Run == best_run, , drop = FALSE]

  counts <- table(as.integer(parks$Rect_ID))
  grid_xy <- site$grid_xy
  vis <- integer(nrow(grid_xy))
  m <- match(as.integer(names(counts)), grid_xy[, "ID"])
  ok <- !is.na(m)
  vis[m[ok]] <- as.integer(counts)[ok]
  n_unique <- sum(lay$unique)
  n_ind <- nrow(lay)
  n_show <- as.integer(n_show)

  if (is_ggplot2_installed()) {
    grid_sf <- sf::st_as_sf(site$grid_poly)
    grid_sf$n_used <- vis[seq_len(nrow(grid_sf))]
    p_occ <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = site$polygon, fill = "white", color = "grey30") +
      ggplot2::geom_sf(
        data = grid_sf, ggplot2::aes(fill = n_used),
        color = "white", linewidth = 0.25
      ) +
      ggplot2::geom_point(
        data = parks, ggplot2::aes(X, Y, color = Parkfitness),
        alpha = 0.25, size = 1.4
      ) +
      ggplot2::geom_point(
        data = best_xy, ggplot2::aes(X, Y),
        color = "black", size = 2.4
      ) +
      ggplot2::scale_fill_gradient(
        low = "#F0F0F0", high = "#E34A33", name = "Individuals\nusing cell"
      ) +
      ggplot2::scale_color_gradient(
        low = "#AED6F1", high = "#1B4F72", name = "Fitness"
      ) +
      ggplot2::coord_sf() +
      ggplot2::labs(
        x = NULL, y = NULL,
        title = sprintf(
          "Generation %d   %d individuals   %d distinct layouts",
          dat$generation, n_ind, n_unique
        ),
        subtitle = paste(
          "Fill = how many layouts used the cell. Points = all turbines (best in black).",
          if (isTRUE(ask) && n_show > 0L) "Enter for the next page of layouts." else ""
        )
      ) +
      ga_plot_theme()

    ply <- maybe_plotly(list(p_occ), interactive)
    if (!is.null(ply)) {
      print(ply)
    } else {
      print(p_occ)
    }

    uniq <- lay[lay$unique, , drop = FALSE]
    n_draw <- if (isTRUE(ask)) nrow(uniq) else min(n_show, nrow(uniq))
    if (n_show > 0L && n_draw > 0L) {
      n_pages <- as.integer(ceiling(n_draw / n_show))
      for (pg in seq_len(n_pages)) {
        pause_next_plot(ask)
        from <- (pg - 1L) * n_show + 1L
        to <- min(pg * n_show, n_draw)
        top <- uniq[from:to, , drop = FALSE]
        top_turb <- parks[parks$Run %in% top$Run, , drop = FALSE]
        top_turb$rank <- top$rank[match(top_turb$Run, top$Run)]
        top_turb$lab <- sprintf(
          "#%d  fit %.3f%s",
          top_turb$rank, top_turb$Parkfitness,
          ifelse(top$elite[match(top_turb$Run, top$Run)], "  elite", "")
        )
        p_small <- ggplot2::ggplot() +
          ggplot2::geom_sf(data = site$polygon, fill = "grey96", color = "grey40") +
          ggplot2::geom_point(
            data = top_turb, ggplot2::aes(X, Y),
            color = "#1B4F72", size = 2
          ) +
          ggplot2::facet_wrap(~lab) +
          ggplot2::coord_sf() +
          ggplot2::labs(
            x = NULL, y = NULL,
            title = sprintf(
              "Generation %d  layouts %d-%d of %d%s",
              dat$generation, from, to, n_unique,
              if (n_pages > 1L) sprintf("  (page %d/%d)", pg, n_pages) else ""
            )
          ) +
          ga_plot_theme() +
          ggplot2::theme(legend.position = "none")
        print(p_small)
      }
    }
  } else {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    plot(sf::st_geometry(site$polygon), col = "white", border = "grey30",
         main = sprintf("Generation %d  (%d layouts)", dat$generation, n_ind))
    plot(site$grid_poly, add = TRUE, border = "grey80")
    graphics::points(parks$X, parks$Y, pch = 16, cex = 0.6, col = "#2980B980")
    graphics::points(best_xy$X, best_xy$Y, pch = 16, cex = 1.4, col = "black")
  }
  invisible(dat)
}

nindiv_vec <- function(x) {
  if (is.null(x)) {
    return(numeric(0))
  }
  nms <- names(x)
  if (is.null(nms) && !is.null(dim(x))) {
    nms <- colnames(x)
  }
  v <- as.numeric(x)
  names(v) <- nms
  v
}

nindiv_pick <- function(v, key, legacy, pos) {
  if (key %in% names(v)) {
    return(unname(v[[key]]))
  }
  if (legacy %in% names(v)) {
    return(unname(v[[legacy]]))
  }
  if (length(v) >= pos) {
    return(unname(v[[pos]]))
  }
  NA_real_
}

census_from_parks <- function(parks, n_el, need_keys = TRUE) {
  if (is.null(parks) || !length(parks)) {
    return(NULL)
  }
  parks <- as.data.frame(parks)
  if (!nrow(parks) || !"Run" %in% names(parks) || !"Rect_ID" %in% names(parks)) {
    return(NULL)
  }
  rid <- as.integer(parks$Rect_ID)
  run <- parks$Run
  id_by_run <- split(rid, run)
  n_ind <- length(id_by_run)
  keys <- NULL
  if (isTRUE(need_keys)) {
    keys <- vapply(
      id_by_run, function(x) paste(sort.int(x), collapse = ","), character(1)
    )
  }
  fit_col <- if ("Parkfitness" %in% names(parks)) {
    "Parkfitness"
  } else if ("EnergyOverall" %in% names(parks)) {
    "EnergyOverall"
  } else {
    NA_character_
  }
  cells_elite <- NA_real_
  if (!is.na(fit_col)) {
    fit_first <- vapply(split(parks[[fit_col]], run), function(z) z[[1]], numeric(1))
    ord <- order(fit_first, decreasing = TRUE)
    if (!is.null(keys)) {
      ord <- ord[!duplicated(keys[ord])]
    }
    top <- ord[seq_len(min(as.integer(n_el), length(ord)))]
    cells_elite <- length(unique(unlist(id_by_run[top], use.names = FALSE)))
  }
  n_dist <- if (is.null(keys)) NA_real_ else length(unique(keys))
  list(
    evaluated = n_ind,
    distinct = n_dist,
    duplicates = if (is.na(n_dist)) NA_real_ else n_ind - n_dist,
    cells = length(unique(rid)),
    cells_elite = cells_elite
  )
}

#' @title Population size and diversity per generation
#' @name population_census
#' @description Counts evaluated individuals, distinct layouts, duplicates
#'   dropped before the next generation, selected parents, elites, elite
#'   offspring and grid cells (this generation and cumulative).
#' @export
#'
#' @inheritParams plot_result
#'
#' @family Plotting Functions
#' @return A data.frame with one row per generation.
#' @examples \donttest{
#' population_census(resultrect)
#' }
population_census <- function(result) {
  n <- nrow(result)
  out <- data.frame(
    generation = seq_len(n),
    evaluated = NA_real_,
    distinct = NA_real_,
    duplicates = NA_real_,
    selected = NA_real_,
    crossover = NA_real_,
    mutated = NA_real_,
    elites = NA_real_,
    elite_kids = NA_real_,
    cells = NA_real_,
    cells_elite = NA_real_,
    cells_cum = NA_real_,
    stringsAsFactors = FALSE
  )
  has_dropped <- FALSE
  if ("nindiv" %in% colnames(result)) {
    for (i in seq_len(n)) {
      v <- nindiv_vec(tryCatch(result[i, "nindiv"][[1]], error = function(e) NULL))
      out$evaluated[i] <- nindiv_pick(v, "evaluated", "nindivfit", 1L)
      out$selected[i] <- nindiv_pick(v, "selected", "nindivsel", 2L)
      out$crossover[i] <- nindiv_pick(v, "crossover", "nindivcros", 3L)
      out$mutated[i] <- nindiv_pick(v, "mutated", "nindivmut", 4L)
      if ("duplicates" %in% names(v)) {
        out$duplicates[i] <- unname(v[["duplicates"]])
        has_dropped <- TRUE
      }
      if ("elites" %in% names(v)) {
        out$elites[i] <- unname(v[["elites"]])
      }
      if ("elite_kids" %in% names(v)) {
        out$elite_kids[i] <- unname(v[["elite_kids"]])
      }
      if ("cells_cum" %in% names(v)) {
        out$cells_cum[i] <- unname(v[["cells_cum"]])
      }
      if ("cells" %in% names(v)) {
        out$cells[i] <- unname(v[["cells"]])
      }
      if ("cells_elite" %in% names(v)) {
        out$cells_elite[i] <- unname(v[["cells_elite"]])
      }
    }
  }
  if (all(is.na(out$elites))) {
    out$elites <- as.numeric(ga_elite_n(result))
  }
  need_keys <- !has_dropped && anyNA(out$duplicates)
  need_cells <- anyNA(out$cells) || anyNA(out$cells_elite) || anyNA(out$cells_cum)
  if ("allCoords" %in% colnames(result) && (need_keys || need_cells)) {
    seen <- integer(0)
    n_el <- ga_elite_n(result)
    have_cum <- !anyNA(out$cells_cum)
    for (i in seq_len(n)) {
      fill_i <- (need_keys && is.na(out$distinct[i])) ||
        is.na(out$cells[i]) || is.na(out$cells_elite[i]) ||
        (!have_cum && is.na(out$cells_cum[i]))
      if (!fill_i) {
        next
      }
      parks <- tryCatch(result[i, "allCoords"][[1]], error = function(e) NULL)
      one <- census_from_parks(parks, n_el, need_keys = need_keys)
      if (is.null(one)) {
        next
      }
      if (is.na(out$evaluated[i])) {
        out$evaluated[i] <- one$evaluated
      }
      if (is.na(out$distinct[i])) {
        out$distinct[i] <- one$distinct
      }
      if (!has_dropped && is.na(out$duplicates[i])) {
        out$duplicates[i] <- one$duplicates
      }
      if (is.na(out$cells[i])) {
        out$cells[i] <- one$cells
      }
      if (is.na(out$cells_elite[i])) {
        out$cells_elite[i] <- one$cells_elite
      }
      if (!have_cum) {
        parks <- as.data.frame(parks)
        seen <- union(seen, unique(as.integer(parks$Rect_ID)))
        out$cells_cum[i] <- length(seen)
      }
    }
  }
  out$dup_share <- ifelse(
    is.finite(out$evaluated) & out$evaluated > 0 & is.finite(out$duplicates),
    100 * out$duplicates / pmax(out$evaluated + out$duplicates, 1),
    NA_real_
  )
  out
}

#' @title Plot population size, cells and efficiency
#' @name plot_population
#' @description Three pages: (1) individuals / parents / elites,
#'   (2) unique grid cells this generation, in the elites, and ever probed,
#'   (3) park efficiency. Each page waits for Enter in an interactive session.
#'   If the grid has e.g. 70 cells and the whole population still touches
#'   every cell, "this generation" and "ever probed" both sit at 70; the
#'   elite line is then the one that shrinks toward the best sites.
#' @export
#'
#' @inheritParams plot_result
#' @param interactive Use plotly when `ask` is `FALSE` and plotly is installed.
#' @param ask If `TRUE`, wait for Enter between pages (Plots pane). Default
#'   is `TRUE` in an interactive session.
#'
#' @family Plotting Functions
#' @return A plotly object, or a list of ggplots, invisibly. The census
#'   table is attached as attribute `census`.
#' @examples \donttest{
#' plot_population(resultrect)
#' }
plot_population <- function(result, interactive = NULL, ask = NULL) {
  cen <- population_census(result)
  if (is.null(ask)) {
    ask <- interactive()
  }
  interactive <- use_plotly(interactive)
  if (!is_ggplot2_installed()) {
    stop(
      "The package 'ggplot2' is required for this function, but it is not installed.\n",
      "Please install it with `install.packages('ggplot2')`"
    )
  }

  series <- list(
    Evaluated = cen$evaluated,
    Selected = cen$selected,
    `Duplicates dropped` = cen$duplicates,
    Elites = cen$elites,
    `Elite offspring` = cen$elite_kids
  )
  if (!any(cen$duplicates > 0, na.rm = TRUE)) {
    series$`Duplicates dropped` <- NULL
  }
  if (!any(is.finite(cen$elite_kids))) {
    series$`Elite offspring` <- NULL
  }
  pop_df <- do.call(rbind, lapply(names(series), function(nm) {
    data.frame(
      generation = cen$generation,
      count = series[[nm]],
      series = nm,
      stringsAsFactors = FALSE
    )
  }))
  pop_df$series <- factor(pop_df$series, levels = names(series))
  pop_df <- pop_df[is.finite(pop_df$count), , drop = FALSE]

  pal <- c(
    Evaluated = "#1B4F72",
    Selected = "#8E44AD",
    `Duplicates dropped` = "#C0392B",
    Elites = "#E67E22",
    `Elite offspring` = "#27AE60"
  )

  p_pop <- ggplot2::ggplot(
    pop_df, ggplot2::aes(generation, count, color = series)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_color_manual(values = pal[names(series)]) +
    ggplot2::labs(
      x = "Generation", y = "Count",
      title = "Population"
    ) +
    ga_plot_theme(legend = "bottom")

  n_grid <- suppressWarnings(max(cen$cells_cum, cen$cells, na.rm = TRUE))
  cell_series <- list(
    `This generation` = cen$cells,
    Elites = cen$cells_elite,
    `Ever probed` = cen$cells_cum
  )
  if (!any(is.finite(cen$cells_elite))) {
    cell_series$Elites <- NULL
  }
  cell_df <- do.call(rbind, lapply(names(cell_series), function(nm) {
    data.frame(
      generation = cen$generation,
      value = cell_series[[nm]],
      series = nm,
      stringsAsFactors = FALSE
    )
  }))
  cell_df$series <- factor(cell_df$series, levels = names(cell_series))
  cell_df <- cell_df[is.finite(cell_df$value), , drop = FALSE]
  cell_pal <- c(
    `This generation` = "#2980B9",
    Elites = "#E67E22",
    `Ever probed` = "#1A7A4C"
  )

  p_div <- ggplot2::ggplot(
    cell_df, ggplot2::aes(generation, value, color = series)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_color_manual(values = cell_pal[names(cell_series)]) +
    ggplot2::labs(
      x = "Generation", y = "Grid cells",
      title = if (is.finite(n_grid)) {
        sprintf("Cells used   (grid has %d cells)", as.integer(n_grid))
      } else {
        "Cells used"
      },
      subtitle = paste(
        "This generation: distinct sites in the whole population.",
        "Elites: sites in the best few layouts (shrinks when they agree).",
        "Ever probed: running union over the run. Both pop and cumulative",
        "sit at the grid size once every cell has been tried."
      )
    ) +
    ga_plot_theme(legend = "bottom") +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8, color = "grey30"))

  s <- tryCatch(ga_series(result), error = function(e) NULL)
  p_eff <- NULL
  if (!is.null(s)) {
    eff_df <- data.frame(
      generation = rep(s$gen, 3),
      efficiency = c(s$eff_max, s$eff_mean, s$eff_min),
      series = factor(
        rep(c("Max", "Mean", "Min"), each = s$n),
        levels = c("Max", "Mean", "Min")
      )
    )
    p_eff <- ggplot2::ggplot(
      eff_df, ggplot2::aes(generation, efficiency, color = series)
    ) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::scale_color_manual(
        values = c(Max = "#1B4F72", Mean = "#2980B9", Min = "#7FB3D5")
      ) +
      ggplot2::labs(
        x = "Generation", y = "Efficiency (%)",
        title = "Park efficiency"
      ) +
      ga_plot_theme(legend = "bottom")
  }

  plots <- list(population = p_pop, cells = p_div, efficiency = p_eff)
  shown <- show_plot_pages(plots, ask = ask, plotly = interactive)
  attr(shown, "census") <- cen
  invisible(shown)
}

wake_cone_one <- function(x, y, wd_from, half_deg, length_m, n = 16L) {
  down <- (as.numeric(wd_from) + 180) %% 360
  angs <- seq(down - half_deg, down + half_deg, length.out = n)
  rad <- angs * pi / 180
  sf::st_polygon(list(cbind(
    c(x, x + length_m * sin(rad), x),
    c(y, y + length_m * cos(rad), y)
  )))
}

leaflet_wind_for_cones <- function(wind) {
  if (is.null(wind) || !is.data.frame(wind) || !nrow(wind)) {
    return(NULL)
  }
  nms <- names(wind)
  wd <- if (any(grepl("^(wd|dir)", nms, ignore.case = TRUE))) {
    wind[[grep("^(wd|dir)", nms, ignore.case = TRUE)[1]]]
  } else if (ncol(wind) >= 2L) {
    wind[[2]]
  } else {
    return(NULL)
  }
  pr <- if (any(grepl("prob", nms, ignore.case = TRUE))) {
    as.numeric(wind[[grep("prob", nms, ignore.case = TRUE)[1]]])
  } else {
    rep(1, length(wd))
  }
  pr[!is.finite(pr)] <- 0
  if (sum(pr) <= 0) {
    pr[] <- 1
  }
  pr <- pr / sum(pr)
  tab <- data.frame(wd = as.numeric(wd), prob = pr)
  tab <- tab[is.finite(tab$wd), , drop = FALSE]
  if (!nrow(tab)) {
    return(NULL)
  }
  if (nrow(tab) > 6L) {
    tab <- tab[order(tab$prob, decreasing = TRUE), , drop = FALSE]
    tab <- tab[seq_len(6L), , drop = FALSE]
    tab$prob <- tab$prob / sum(tab$prob)
  }
  tab
}

leaflet_wake_length <- function(poly, rotor) {
  rotor <- as.numeric(rotor)
  if (!is.finite(rotor) || rotor <= 0) {
    rotor <- 50
  }
  bb <- sf::st_bbox(poly)
  diag <- sqrt((bb$xmax - bb$xmin)^2 + (bb$ymax - bb$ymin)^2)
  if (!is.finite(diag) || diag <= 0) {
    return(12 * 2 * rotor)
  }
  max(200, min(12 * 2 * rotor, 0.45 * diag))
}

leaflet_wake_cones <- function(xy, wind_tab, half_deg, length_m, crs, farbe) {
  n <- nrow(xy)
  n_dir <- nrow(wind_tab)
  geom <- vector("list", n * n_dir)
  turb <- integer(n * n_dir)
  wd <- numeric(n * n_dir)
  prob <- numeric(n * n_dir)
  col <- character(n * n_dir)
  k <- 0L
  for (i in seq_len(n)) {
    for (j in seq_len(n_dir)) {
      k <- k + 1L
      geom[[k]] <- wake_cone_one(
        xy[i, 1], xy[i, 2], wind_tab$wd[j], half_deg, length_m
      )
      turb[k] <- i
      wd[k] <- wind_tab$wd[j]
      prob[k] <- wind_tab$prob[j]
      col[k] <- farbe[i]
    }
  }
  sf::st_sf(
    turb = turb,
    wd = wd,
    prob = prob,
    farbe = col,
    geometry = sf::st_sfc(geom, crs = crs)
  )
}

leaflet_match_cells <- function(layout_df, cells) {
  if (is.null(cells) || !is.data.frame(cells) || !nrow(cells)) {
    return(NULL)
  }
  ids <- if ("Rect_ID" %in% names(layout_df)) {
    layout_df$Rect_ID
  } else if ("ID" %in% names(layout_df)) {
    layout_df$ID
  } else {
    return(NULL)
  }
  cells[match(as.integer(ids), as.integer(cells$ID)), , drop = FALSE]
}

leaflet_turbine_popup <- function(wake_pct, cells_row = NULL) {
  lines <- sprintf("Total wake: <b>%s%%</b>", format(as.numeric(wake_pct), digits = 3))
  if (is.null(cells_row) || !NROW(cells_row)) {
    return(paste(lines, collapse = "<br/>"))
  }
  r <- cells_row[1, ]
  add <- function(ok, txt) {
    if (isTRUE(ok)) {
      lines <<- c(lines, txt)
    }
  }
  add(is.finite(r$elevation), sprintf("Elevation: %s m", round(r$elevation, 0)))
  add(is.finite(r$wind_mult), sprintf("Wind multiplier: %s", format(r$wind_mult, digits = 3)))
  add(is.finite(r$z0), sprintf("z<sub>0</sub>: %s m", format(r$z0, digits = 3)))
  add(is.finite(r$k), sprintf("Wake decay k: %s", format(r$k, digits = 3)))
  add(is.finite(r$air_rh), sprintf("Air density: %s kg/m<sup>3</sup>", format(r$air_rh, digits = 3)))
  paste(lines, collapse = "<br/>")
}

leaflet_prepare_terrain <- function(tm) {
  if (is.null(tm) || !is.list(tm) || is.null(tm$srtm_crop)) {
    return(NULL)
  }
  if (!requireNamespace("raster", quietly = TRUE)) {
    return(list(cells = terrain_cells_of(tm$srtm_crop)))
  }
  to_ll <- function(r) {
    if (!inherits(r, "SpatRaster")) {
      return(NULL)
    }
    r <- terra::project(r, "EPSG:4326")
    raster::raster(r)
  }
  list(
    elevation = to_ll(tm$srtm_crop[[1]]),
    wind_mult = to_ll(tm$srtm_crop[[2]]),
    cells = terrain_cells_of(tm$srtm_crop)
  )
}

#' @title Plot a wind warm with leaflet
#' @name plot_leaflet
#' @description Plot a resulting wind farm on a leaflet map. Wakes are
#'   downwind cones (Jensen search angle), not circles. Terrain rasters
#'   from `result$terrainModel` are optional overlay layers.
#'
#' @export
#'
#' @inheritParams plot_result
#' @param which A numeric value, indicating which individual to plot. The
#'   default is 1. Combined with \code{orderitems = TRUE} this will show the
#'   best performing wind farm.
#' @param orderitems A logical value indicating whether the results should be
#'   ordered by energy values \code{TRUE} or chronologically \code{FALSE}
#' @param grid Optional grid polygons. By default they are rebuilt from
#'   `result` and `area`. You can pass the polygon element of
#'   [grid_area()] or [hexa_area()].
#' @param wind Optional wind table (`ws`, `wd`, `probab`). Defaults to
#'   the table stored in `result`.
#' @param terrain Optional output of `leaflet_prepare_terrain()`.
#'   Defaults to `result$terrainModel` when present.
#'
#' @return Returns a leaflet map.
#'
#' @examples \dontrun{
#' ## Plot the best wind farm on a leaflet map (ordered by energy values)
#' plot_leaflet(result = resulthex, area = sp_polygon, which = 1)
#'
#' ## Plot the last wind farm (ordered by chronology).
#' plot_leaflet(
#'   result = resulthex, area = sp_polygon, orderitems = FALSE,
#'   which = 1
#' )
#'
#' ## Plot the best wind farm on a leaflet map with the rectangular Grid
#' Grid <- grid_area(sp_polygon, size = 150, prop = 0.4)
#' plot_leaflet(
#'   result = resultrect, area = sp_polygon, which = 1,
#'   grid = Grid[[2]]
#' )
#'
#' ## Plot the last wind farm with hexagonal Grid
#' Grid <- hexa_area(sp_polygon, size = 75)
#' plot_leaflet(
#'   result = resulthex, area = sp_polygon, which = 1,
#'   grid = Grid[[2]]
#' )
#' }
plot_leaflet <- function(result, area, which = 1, orderitems = TRUE, grid = NULL,
                         wind = NULL, terrain = NULL) {
  GridPol <- grid
  if (!is_leaflet_installed()) {
    stop(
      "The package 'leaflet' is required for this function, but it is not installed.\n",
      "Please install it with `install.packages('leaflet')`"
    )
  }

  ## Check Polygon and CRS ##############
  poly1 <- isSpatial(area = area)
  if (is.na(st_crs(poly1))) {
    projection <- result[, "inputData"][[1]]["Projection", ][[1]]
    projection <- tryCatch(as.numeric(projection),
      warning = function(e) projection,
      error = function(e) projection
    )
    st_crs(poly1) <- st_crs(projection)
  }
  proj_pol <- st_crs(poly1)

  ## Order Items and pick `best` ##############
  if (which > nrow(result)) {
    cat(paste("Maximum possible number for 'which': ", nrow(result)))
    which <- nrow(result)
  }

  if (orderitems) {
    a <- sapply(result[, 2], FUN = function(i) {
      subset.matrix(i,
        subset = c(TRUE, rep(FALSE, nrow(i) - 1)),
        select = "EnergyOverall"
      )
    })
    order1 <- order(a, decreasing = TRUE)
    result <- result[order1, , drop = FALSE]
    beste <- which
  } else {
    beste <- ""
  }

  proj_longlat <- 4326

  if (is.null(wind)) {
    wind <- ga_result_wind(result)
  }
  if (is.null(terrain)) {
    terrain <- leaflet_prepare_terrain(ga_result_terrain(result))
  }
  inp <- ga_result_inputs(result)
  rotor <- suppressWarnings(as.numeric(ga_inp(inp, "Rotorradius")))
  half_wake <- getOption("windfarmGA.max_angle", 20)

  ## Grid-Function ##############
  if (!is.null(GridPol)) {
    if (is.na(st_crs(GridPol))) {
      st_crs(GridPol) <- st_crs(proj_pol)
    }
  } else {
    cellsize <- as.numeric(result[, "inputData"][[1]]["Resolution", ][[1]])
    if (result[, "inputData"][[1]]["Grid Method", ][[1]] != "h") {
      GridPol <- grid_area(poly1, cellsize,
        prop = as.numeric(result[, "inputData"][[1]]["Percentage of Polygon", ][[1]]),
        plot_grid = FALSE
      )[[2]]
    } else {
      GridPol <- hexa_area(poly1, cellsize, plot_grid = FALSE)[[2]]
    }
  }
  GridPol <- st_transform(GridPol, st_crs(proj_longlat))

  ## Pick a Windfarm and Project to WGS84 ##############
  result <- result[, "bestPaEn"][[which]]
  result <- data.frame(result, stringsAsFactors = FALSE)
  xy_m <- cbind(as.numeric(result$X), as.numeric(result$Y))
  xysp <- st_as_sf(result, coords = c("X", "Y"))
  st_crs(xysp) <- proj_pol
  resultxy <- st_coordinates(st_transform(xysp, proj_longlat))
  result$X <- resultxy[, 1]
  result$Y <- resultxy[, 2]

  poly1 <- st_transform(poly1, proj_longlat)

  ## Get Coordinates for Title ##############
  bbx <- st_bbox(poly1)
  title_locat <- c(mean(bbx[c(1, 3)]), max(bbx[c(2, 4)]))

  ## Color Coding ##############
  result$AbschGesamt <- round(result$AbschGesamt, 1)
  col_cir <- grDevices::colorRampPalette(c(
    "green", "yellow",
    "red", "darkred"
  ))
  br <- length(levels(factor(result$AbschGesamt)))
  if (br > 1) {
    color_pal <- col_cir(br)
  } else {
    color_pal <- "green"
  }
  pal <- leaflet::colorFactor(color_pal,
    domain = sort(result$AbschGesamt),
    ordered = TRUE,
    reverse = FALSE
  )
  result$farbe <- pal(result$AbschGesamt)


  ## Turbine Icons ###########
  turbine_icon <- leaflet::iconList(
    turbine_icon = leaflet::makeIcon(
      iconUrl = paste0(
        system.file(package = "windfarmGA"),
        "/extdata/windturdk.png"
      ),
      iconWidth = 30, iconHeight = 50
    )
  )
  cells_at <- leaflet_match_cells(result, if (is.list(terrain)) terrain$cells else NULL)
  list_popup <- vapply(seq_len(nrow(result)), function(i) {
    cr <- if (is.null(cells_at)) NULL else cells_at[i, , drop = FALSE]
    leaflet_turbine_popup(result$AbschGesamt[i], cr)
  }, character(1))

  wind_tab <- leaflet_wind_for_cones(wind)
  cones <- NULL
  if (!is.null(wind_tab) && nrow(xy_m)) {
    poly_m <- st_transform(poly1, proj_pol)
    cones <- leaflet_wake_cones(
      xy_m, wind_tab, half_wake,
      leaflet_wake_length(poly_m, rotor),
      proj_pol, result$farbe
    )
    cones <- st_transform(cones, st_crs(proj_longlat))
  }

  ## Plot a Leaflet Map ###################
  overlay_group <- c("Wake cones", "Title", "Polygon", "Turbines", "Grid")
  if (!is.null(terrain$elevation)) {
    overlay_group <- c("Elevation", overlay_group)
  }
  if (!is.null(terrain$wind_mult)) {
    overlay_group <- c("Wind multiplier", overlay_group)
  }
  opaycity <- 0.4
  map <-
    leaflet::leaflet() %>%
    leaflet::addTiles(group = "OSM") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    ## Write a Popup with the energy output
    leaflet::addPopups(title_locat[1], (title_locat[2] + 0.0002),
      group = "Title",
      popup = paste(
        beste, "<b>Best Wind Farm with: ",
        round(as.numeric(
          result[, "EnergyOverall"][[1]]
        ), 2),
        "kWh</b>"
      ),
      options = leaflet::popupOptions(
        closeButton = TRUE, closeOnClick = FALSE
      )
    ) %>%
    ## Add the Polygon
    leaflet::addPolygons(
      data = poly1, group = "Polygon",
      fill = TRUE, fillOpacity = 0.4
    ) %>%
    ## Add the Genetic Algorithm Space
    leaflet::addPolygons(
      data = GridPol, group = "Grid", weight = 1,
      opacity = opaycity,
      fill = TRUE, fillOpacity = 0
    ) %>%
    ## Add the turbine symbols
    leaflet::addMarkers(
      lng = result$X, lat = result$Y,
      icon = turbine_icon[1], popup = list_popup,
      group = "Turbines"
    ) %>%
    leaflet::addLegend(
      position = "topleft",
      pal = pal,
      values = result$AbschGesamt,
      labFormat = leaflet::labelFormat(suffix = "%"),
      opacity = 1, title = "Total Wake Effect",
      layerId = "Legend"
    ) %>%
    ## Layers control
    leaflet::addLayersControl(
      baseGroups = c(
        "OSM",
        "Satellite"
      ),
      overlayGroups = overlay_group,
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )

  if (!is.null(cones) && nrow(cones)) {
    max_p <- max(cones$prob, na.rm = TRUE)
    if (!is.finite(max_p) || max_p <= 0) {
      max_p <- 1
    }
    map <- leaflet::addPolygons(
      map,
      data = cones,
      group = "Wake cones",
      fillColor = cones$farbe,
      color = cones$farbe,
      weight = 1,
      opacity = 0.35,
      fillOpacity = 0.16 + 0.15 * (cones$prob / max_p),
      stroke = TRUE
    )
  } else {
    map <- leaflet::addCircleMarkers(
      map,
      lng = result$X, lat = result$Y,
      radius = pmax(6, round(as.numeric(result$AbschGesamt), 2) / 10),
      color = result$farbe,
      stroke = TRUE, fillOpacity = 0.8,
      group = "Wake cones"
    )
  }
  if (is.list(terrain)) {
    add_ll_raster <- function(map, r, group, pal) {
      if (is.null(r) || !inherits(r, "Raster")) {
        return(map)
      }
      v <- raster::values(r)
      v <- v[is.finite(v)]
      if (!length(v)) {
        return(map)
      }
      rng <- range(v)
      pad <- max(diff(rng) * 0.1, 1e-6)
      leaflet::addRasterImage(
        map, r,
        colors = leaflet::colorNumeric(
          pal,
          domain = c(rng[1] - pad, rng[2] + pad),
          na.color = "transparent"
        ),
        opacity = 0.5, group = group
      )
    }
    map <- add_ll_raster(
      map, terrain$elevation, "Elevation", grDevices::terrain.colors(12)
    )
    map <- add_ll_raster(
      map, terrain$wind_mult, "Wind multiplier", "YlOrRd"
    )
    if (!is.null(terrain$elevation)) {
      map <- leaflet::hideGroup(map, "Elevation")
    }
    if (!is.null(terrain$wind_mult)) {
      map <- leaflet::hideGroup(map, "Wind multiplier")
    }
  }

  map
}

#' @title Fitness and operator rates
#' @name plot_parkfitness
#' @description Fitness (max / mean / min) and the three operator rates.
#'   Rates are percentages: **Selection** = share of the population used as
#'   parents, **Crossover inject** = unused cells mixed into children,
#'   **Mutation** = chance that a turbine is swapped to a free cell.
#'   The legend sits outside and follows the typical vertical order of the
#'   lines (selection highest, then inject, mutation lowest). Plotly hover
#'   is used only when \code{ask} is \code{FALSE} and plotly is installed.
#' @export
#'
#' @inheritParams plot_result
#' @param spar Unused, kept so existing calls do not break.
#' @param interactive Use plotly when `ask` is `FALSE` and plotly is installed.
#' @param ask If `TRUE`, wait for Enter between the fitness page and the
#'   rates page. Default is `TRUE` in an interactive session.
#'
#' @family Plotting Functions
#' @return A plotly object, or a list of ggplots, invisibly.
#' @examples \donttest{
#' plot_parkfitness(resulthex)
#' }
plot_parkfitness <- function(result, spar = 0.1, interactive = NULL,
                             ask = NULL) {
  s <- ga_series(result)
  if (is.null(ask)) {
    ask <- interactive()
  }
  interactive <- use_plotly(interactive)
  last_imp <- max(which(s$improved))
  n_imp <- sum(s$improved)

  if (!is_ggplot2_installed()) {
    stop(
      "The package 'ggplot2' is required for this function, but it is not installed.\n",
      "Please install it with `install.packages('ggplot2')`"
    )
  }

  fit_df <- data.frame(
    generation = s$gen,
    Max = s$fit_max,
    Mean = s$fit_mean,
    Min = s$fit_min
  )
  fit_long <- data.frame(
    generation = rep(s$gen, 3),
    fitness = c(s$fit_max, s$fit_mean, s$fit_min),
    series = factor(
      rep(c("Max", "Mean", "Min"), each = s$n),
      levels = c("Max", "Mean", "Min")
    )
  )
  imp_df <- fit_df[s$improved, , drop = FALSE]

  p_fit <- ggplot2::ggplot(fit_long, ggplot2::aes(generation, fitness, color = series)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(
      data = imp_df, ggplot2::aes(generation, Max, color = "New max"),
      size = 2, inherit.aes = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c(
        Max = "#1B4F72", Mean = "#2980B9", Min = "#7FB3D5", `New max` = "#E67E22"
      ),
      breaks = c("Max", "Mean", "Min", "New max")
    ) +
    ggplot2::labs(
      x = "Generation", y = "Fitness",
      title = sprintf(
        "Fitness   last new max: gen %d / %d   (%d improvements)",
        last_imp, s$n, n_imp
      )
    ) +
    ga_plot_theme()

  rate_df <- data.frame(
    generation = rep(s$gen, 3),
    rate = c(s$sel_pct, s$inject * 100, s$mut * 100),
    operator = factor(
      rep(c("Selection", "Crossover inject", "Mutation"), each = s$n),
      levels = c("Selection", "Crossover inject", "Mutation")
    )
  )
  p_rate <- ggplot2::ggplot(
    rate_df, ggplot2::aes(generation, rate, color = operator)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(
      values = c(
        Selection = "#1A7A4C",
        `Crossover inject` = "#C0392B",
        Mutation = "#7D3C98"
      )
    ) +
    ggplot2::labs(
      x = "Generation", y = "Rate (%)",
      title = "Operator rates",
      subtitle = paste(
        "Selection: % of the population used as parents.",
        "Crossover inject: % of free slots filled from unused cells.",
        "Mutation: % chance a turbine swaps to a free cell.",
        "Explore, refine, then a short disturbance pulse if refine lasts."
      )
    ) +
    ga_plot_theme() +
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8, color = "grey30"))

  plots <- list(fitness = p_fit, rates = p_rate)
  show_plot_pages(plots, ask = ask, plotly = interactive)
}

#' @title When the best layout improved
#' @name plot_development
#' @description Change of the generation-best fitness. Green = new record,
#'   orange = flat, red = worse. Blue line is the share of grid cells visited
#'   so far.
#'
#' @export
#'
#' @inheritParams plot_result
#'
#' @family Plotting Functions
#' @return Returns NULL. Used for plotting
#' @examples \donttest{
#' plot_development(resultrect)
#' }
plot_development <- function(result) {
  s <- ga_series(result)
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::par(mar = c(4, 4.2, 2.5, 4.2))

  dmax <- c(0, diff(s$fit_max))
  col <- ifelse(dmax > 1e-8, "#27AE60", ifelse(dmax < -1e-8, "#C0392B", "#F39C12"))
  graphics::plot(
    s$gen, dmax, type = "h", lwd = 2, col = col,
    xlab = "Generation", ylab = "Change in max fitness",
    main = "When the best layout improved"
  )
  graphics::abline(h = 0, col = "grey50")
  if (any(is.finite(s$coverage))) {
    graphics::par(new = TRUE)
    graphics::plot(
      s$gen, s$coverage, type = "l", lwd = 2, col = "#2980B9",
      axes = FALSE, xlab = "", ylab = "", ylim = c(0, 100)
    )
    graphics::axis(4, col = "#2980B9", col.axis = "#2980B9")
    graphics::mtext("Cells visited (%)", side = 4, line = 2.5, col = "#2980B9")
  }
  invisible(NULL)
}

#' @title Energy and efficiency over generations
#' @name plot_evolution
#' @description Max and mean park efficiency and energy yield on one page.
#'
#' @export
#'
#' @inheritParams plot_result
#' @param ask Unused, kept so existing calls do not break.
#' @param spar Unused, kept so existing calls do not break.
#'
#' @family Plotting Functions
#' @return Returns NULL. Used for plotting
#' @examples \donttest{
#' plot_evolution(resultrect)
#' }
plot_evolution <- function(result, ask = FALSE, spar = 0.1) {
  s <- ga_series(result)
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::par(mfrow = c(1, 2), mar = c(4, 4.2, 2.5, 1))

  yr <- range(c(s$eff_mean, s$eff_max), finite = TRUE)
  graphics::plot(
    s$gen, s$eff_max, type = "l", lwd = 2.2, col = "#1B4F72",
    xlab = "Generation", ylab = "Efficiency (%)",
    ylim = yr, main = "Park efficiency"
  )
  graphics::grid(col = "grey85", lty = 1)
  graphics::lines(s$gen, s$eff_mean, lwd = 1.6, col = "#2980B9")
  graphics::legend(
    "bottomright", c("Max", "Mean"),
    col = c("#1B4F72", "#2980B9"), lty = 1, lwd = c(2.2, 1.6),
    bty = "n", cex = 0.85
  )

  yr <- range(c(s$ene_mean, s$ene_max), finite = TRUE)
  graphics::plot(
    s$gen, s$ene_max, type = "l", lwd = 2.2, col = "#1B4F72",
    xlab = "Generation", ylab = "Energy (kW)",
    ylim = yr, main = "Energy yield"
  )
  graphics::grid(col = "grey85", lty = 1)
  graphics::lines(s$gen, s$ene_mean, lwd = 1.6, col = "#2980B9")
  graphics::legend(
    "bottomright", c("Max", "Mean"),
    col = c("#1B4F72", "#2980B9"), lty = 1, lwd = c(2.2, 1.6),
    bty = "n", cex = 0.85
  )
  invisible(NULL)
}

#' @title Per-generation fitness / efficiency / energy
#' @name plot_cloud
#' @description Summarise every evaluated individual. With \code{pl = TRUE}
#'   draw three panels of points (max as a line). The returned table has
#'   min / mean / max / sd per generation.
#'
#' @export
#'
#' @inheritParams plot_result
#' @param pl Draw the three panels? Default is FALSE
#'
#' @family Plotting Functions
#' @return A data.frame with fitness, efficiency and energy summaries
#'
#' @examples \donttest{
#' plcdf <- plot_cloud(resulthex, TRUE)
#' }
plot_cloud <- function(result, pl = FALSE) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  clouddata <- result[, 7]
  efficiency_cloud <- lapply(clouddata, function(x) x[, 1])
  energy_cloud <- lapply(clouddata, function(x) x[, 2])
  fitness_cloud <- lapply(clouddata, function(x) x[, 3])

  efficiency_per_gen <- energy_per_gen <- fitness_per_gen <- list()
  for (i in seq_along(clouddata)) {
    l <- length(clouddata[[i]][, "EfficAllDir"])
    efficiency_per_gen[[i]] <- t(as.matrix(rbind(rep(i, l), efficiency_cloud[[i]])))
    energy_per_gen[[i]] <- t(as.matrix(rbind(rep(i, l), energy_cloud[[i]])))
    fitness_per_gen[[i]] <- t(as.matrix(rbind(rep(i, l), fitness_cloud[[i]])))
  }

  summarise_gen <- function(mat) {
    df <- data.frame(mat)
    cbind(
      X1 = stats::aggregate(df, list(df$X1), max)[, 2],
      max = stats::aggregate(df, list(df$X1), max)[, 3],
      mean = stats::aggregate(df, list(df$X1), mean)[, 3],
      min = stats::aggregate(df, list(df$X1), min)[, 3],
      sd = stats::aggregate(df, list(df$X1), sd)[, 3]
    )
  }

  efficiency_per_gen <- do.call("rbind", efficiency_per_gen)
  energy_per_gen <- do.call("rbind", energy_per_gen)
  fitness_per_gen <- do.call("rbind", fitness_per_gen)
  efficiency_per_genmax <- summarise_gen(efficiency_per_gen)
  energy_per_genmax <- summarise_gen(energy_per_gen)
  fitness_per_genmax <- summarise_gen(fitness_per_gen)

  if (isTRUE(pl)) {
    graphics::par(mfrow = c(1, 3), mar = c(4, 4.2, 2.4, 1))
    fade <- grDevices::adjustcolor("#1B4F72", alpha.f = 0.22)
    panels <- list(
      list(fitness_per_gen, fitness_per_genmax, "Fitness", "Fitness"),
      list(efficiency_per_gen, efficiency_per_genmax, "Efficiency", "Efficiency (%)"),
      list(energy_per_gen, energy_per_genmax, "Energy", "Energy (kW)")
    )
    for (pn in panels) {
      graphics::plot(
        pn[[1]][, 1], pn[[1]][, 2], pch = 16, cex = 0.35, col = fade,
        xlab = "Generation", ylab = pn[[4]], main = pn[[3]]
      )
      graphics::lines(pn[[2]][, "X1"], pn[[2]][, "max"], lwd = 2, col = "#E67E22")
      graphics::lines(pn[[2]][, "X1"], pn[[2]][, "mean"], lwd = 1.4, col = "#2980B9")
    }
    graphics::legend(
      "bottomright", c("Individuals", "Max", "Mean"),
      col = c(fade, "#E67E22", "#2980B9"),
      pch = c(16, NA, NA), lty = c(NA, 1, 1), lwd = c(NA, 2, 1.4),
      bty = "n", cex = 0.8
    )
  }

  clouddatafull <- cbind(
    Fitn = fitness_per_genmax,
    Eff = efficiency_per_genmax,
    Ene = energy_per_genmax
  )
  colnames(clouddatafull) <- c(
    "FitX1", "FitMax", "FitMean", "FitMin", "FitSD",
    "EffX1", "EffMax", "EffMean", "EffMin", "EffSD",
    "EneX1", "EneMax", "EneMean", "EneMin", "EneSD"
  )
  invisible(clouddatafull)
}

#' @title Fitness and operator rates
#' @name plot_fitness_evolution
#' @description Same figure as \code{\link{plot_parkfitness}}.
#' @export
#'
#' @inheritParams plot_evolution
#' @param interactive Use plotly when `ask` is `FALSE` and plotly is installed.
#'
#' @family Plotting Functions
#' @return Returns NULL. Used for plotting
#' @examples \donttest{
#' plot_fitness_evolution(resulthex)
#' }
plot_fitness_evolution <- function(result, spar = 0.1, interactive = NULL,
                                   ask = NULL) {
  plot_parkfitness(result, spar = spar, interactive = interactive, ask = ask)
}


#' @title Plot the result of a randomized output.
#' @name plot_random_search
#' @description Plotting method for the results of
#'   \code{\link{random_search_single}} and \code{\link{random_search}}.
#'
#' @export
#'
#' @inheritParams plot_result
#' @param resultRS The result of the random functions
#'   \code{\link{random_search_single}} and \code{\link{random_search}}.
#' @param best How many best candidates to plot. Default is 1.
#'
#' @family Randomization
#' @return Returns NULL. Used for plotting
#'
#' @examples \donttest{
#' library(sf)
#' area <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)
#'   ))),
#'   crs = 3035
#' ))
#'
#' Res <- random_search(result = resultrect, area = area)
#' plot_random_search(resultRS = Res, result = resultrect, area = area, best = 2)
#' }
plot_random_search <- function(resultRS, result, area, best) {

  ## set Graphic Params
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(1, 2))

  result_inputs <- ga_input_matrix(result)
  resultRS1 <- do.call("rbind", cbind(resultRS))
  a <- resultRS1[, "EnergyOverall"]
  order1 <- order(a, decreasing = TRUE)
  resultRS1 <- resultRS1[order1, ]

  if (missing(best)) best <- 1

  resBest <- resultRS1[!duplicated(resultRS1[, "Run"]), , drop = FALSE]
  if (nrow(resBest) < best) {
    best <- nrow(resBest)
  }

  resBest <- resBest[1:best, , drop = FALSE]

  resultRS2 <- list()
  for (nr in 1:nrow(resBest)) {
    resultRS2[[nr]] <- resultRS1[resultRS1[, "Run"] == resBest[, "Run"][nr] &
      resultRS1[, "bestGARun"] == resBest[, "bestGARun"][nr], ]
  }
  resultRS1 <- resultRS2

  resultRS1 <- rev(resultRS1)
  resBest <- resBest[order(resBest[, "EnergyOverall"]), , drop = FALSE]

  area <- isSpatial(area)
  cellsize <- as.numeric(result_inputs["Resolution", ][[1]])
  if (toupper(result_inputs["Grid Method", ][[1]]) == "RECTANGULAR") {
    Grid <- grid_area(area,
      size = cellsize,
      prop = as.numeric(result_inputs["Percentage of Polygon", ][[1]])
    )[[2]]
  } else {
    Grid <- hexa_area(area, size = cellsize)[[2]]
  }
  rbPal1 <- grDevices::colorRampPalette(c("green", "red"))
  col2res <- "lightblue"

  for (i in 1:length(resultRS1)) {
    ## Original GA-result ################
    bestGAR <- resBest[, "bestGARun", drop = FALSE][i]
    bestrestGA <- result[bestGAR, ]$bestPaEn
    brOrig <- length(levels(factor(bestrestGA[, "AbschGesamt", drop = FALSE])))
    if (brOrig > 1) {
      ColOri <- rbPal1(brOrig)[as.numeric(cut(
        as.numeric(bestrestGA[, "AbschGesamt", drop = FALSE]),
        breaks = brOrig
      ))]
    } else {
      ColOri <- "green"
    }

    bestrestGA[, "EnergyOverall"] <- round(bestrestGA[, "EnergyOverall"], 2)
    bestrestGA[, "EfficAllDir"] <- round(bestrestGA[, "EfficAllDir"], 2)
    plot(sf::st_geometry(area),
      col = col2res, reset = FALSE,
      main = paste(
        "Original - Best Energy:", (best + 1) - i, "\n", "Energy Output",
        bestrestGA[, "EnergyOverall"][[1]], "kW", "\n", "Efficiency:",
        bestrestGA[, "EfficAllDir"][[1]]
      )
    )
    plot(sf::st_geometry(Grid), add = TRUE)
    graphics::mtext("Total Wake Effect in %", side = 2)
    graphics::points(bestrestGA[, "X"], bestrestGA[, "Y"],
      cex = 2, pch = 20, col = ColOri
    )
    graphics::text(bestrestGA[, "X"], bestrestGA[, "Y"],
      round(bestrestGA[, "AbschGesamt"], 0),
      cex = 0.8, pos = 1, col = "black"
    )
    distpo <- stats::dist(
      x = cbind(bestrestGA[, "X"], bestrestGA[, "Y"]),
      method = "euclidian"
    )
    graphics::mtext(paste("minimal Distance", round(min(distpo), 2)),
      side = 1, line = 0
    )
    graphics::mtext(paste("mean Distance", round(mean(distpo), 2)),
      side = 1, line = 1
    )
    ################

    ## Random Search Output  ################
    EnergyBest <- data.frame(resultRS1[[i]])
    ## Assign the colour depending on the individual wind speed
    br <- length(levels(factor(EnergyBest[, "AbschGesamt"])))
    if (br > 1) {
      Col <- rbPal1(br)[as.numeric(cut(as.numeric(
        EnergyBest[, "AbschGesamt"]
      ), breaks = br))]
    } else {
      Col <- "green"
    }

    EnergyBest[, "EnergyOverall"] <- round(EnergyBest[, "EnergyOverall"], 2)
    EnergyBest[, "EfficAllDir"] <- round(EnergyBest[, "EfficAllDir"], 2)
    plot(sf::st_geometry(area),
      col = col2res, reset = FALSE,
      main = paste(
        "Random Search - Best Energy:", (best + 1) - i,
        "\n", "Energy Output",
        EnergyBest[, "EnergyOverall"][[1]], "kW", "\n", "Efficiency:",
        EnergyBest[, "EfficAllDir"][[1]]
      )
    )

    plot(sf::st_geometry(Grid), add = TRUE)
    graphics::mtext("Total Wake Effect in %", side = 2)
    graphics::points(EnergyBest[, "X"], EnergyBest[, "Y"],
      cex = 2, pch = 20, col = Col
    )
    graphics::text(EnergyBest[, "X"], EnergyBest[, "Y"],
      round(EnergyBest[, "AbschGesamt"], 0),
      cex = 0.8, pos = 1, col = "black"
    )
    distpo <- stats::dist(
      x = cbind(EnergyBest[, "X"], EnergyBest[, "Y"]),
      method = "euclidian"
    )
    graphics::mtext(paste("minimal Distance", round(min(distpo), 2)),
      side = 1, line = 0
    )
    graphics::mtext(paste("mean Distance", round(mean(distpo), 2)),
      side = 1, line = 1
    )
    ################
  }

  invisible(NULL)
}
