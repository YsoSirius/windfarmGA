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
#' @param plotit Should the windrose be plotted? Default is TRUE
#'
#' @family Plotting Functions
#' @return NULL
#'
#' @examples
#' ## Exemplary Input Wind speed and direction data frame
#' # Uniform wind speed and single wind direction
#' data.in <- data.frame(ws = 12, wd = 0)
#' windrosePlot <- plot_windrose(data = data.in, spd = data.in$ws,
#'    dir = data.in$wd)
#'
#' # Random wind speeds and random wind directions
#' data.in <- data.frame(ws = sample(1:25, 10), 
#'                       wd = sample(1:260, 10))
#' windrosePlot <- plot_windrose(data = data.in, spd = data.in$ws,
#'    dir = data.in$wd)
#'
plot_windrose <- function(data, spd, dir, spdres = 2, dirres = 10, spdmin = 1,
                          spdmax = 30, palette = "YlGnBu",
                          spdseq = NULL, plotit = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The package 'ggplot2' is required for this function, but it is not installed.\n",
         "Please install it with `install.packages('ggplot2')`")
  }
  
  if (!missing(data) && exists("data")) {
    # Assume that we've been given a data frame. Lets find the correct columns
    if (length(colnames(data))) {
      accep_speed <- c("SPEED", "GESCH", "V", "WS")
      accep_direc <- c("DIR", "RICHT", "WD")
      sum_col_match <- sum(sapply(c(accep_speed, accep_direc), grepl,
                                  toupper(colnames(data)) ))
      if (sum_col_match >= 2) {
        speed_match <- which(sapply(
          lapply(accep_speed, grepl, toupper(colnames(data))),
          any))
        direc_match <- which(sapply(
          lapply(accep_direc, grepl, toupper(colnames(data))),
          any))
        
        speed_index <- which(grepl(accep_speed[speed_match],
                                   toupper(colnames(data))))
        direc_index <- which(grepl(accep_direc[direc_match],
                                   toupper(colnames(data))))
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
  } 
  else if (!missing(spd) && !missing(dir) && is.numeric(spd) && is.numeric(dir)) {
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
    min(max(3, colorpal_n), min(9, colorpal_n)), palette))(colorpal_n)
  
  if (max(data[[spd]], na.rm = TRUE) > spdmax) {
    speed_brks <- c(spdseq, max(data[[spd]], na.rm = TRUE))
    speed_labls <- c(paste(c(spdseq[1:seq_length - 1]), '-', 
                           c(spdseq[2:seq_length])),
                     paste(spdmax, "-", max(data[[spd]], na.rm = TRUE)))
    wind_colorpal <- c(wind_colorpal, "grey50")
  } else{
    speed_brks <- spdseq
    speed_labls <- paste(c(spdseq[1:seq_length - 1]), '-', 
                         c(spdseq[2:seq_length]))
  }
  speed_bins <- cut(x = data[[spd]], breaks = speed_brks, 
                    labels = speed_labls, ordered_result = TRUE)
  
  # figure out the wind direction bins #################
  dir_brks <- c(-dirres / 2, seq(dirres / 2, 360 - dirres/2, 
                                 by = dirres), 360 + dirres / 2)
  dir_labls <- c(paste(360 - dirres / 2, "-", dirres / 2),
                 paste(seq(dirres / 2, 360 - 3 * dirres / 2, 
                           by = dirres), "-", 
                       seq(3 * dirres/2, 360 - dirres / 2, by = dirres)),
                 paste(360 - dirres / 2, "-", dirres / 2))
  # assign each wind direction to a bin
  dir_bins <- cut(data[[dir]], breaks = dir_brks,
                  ordered_result = TRUE)
  levels(dir_bins) <- dir_labls
  data$dir_bins <- dir_bins
  
  # create the plot #################
  plot_windrose <- ggplot2::ggplot(data = data,
                                   ggplot2::aes(x = dir_bins,
                                                fill = speed_bins)) +
    ggplot2::geom_bar() +
    ggplot2::scale_x_discrete(drop = FALSE, labels = ggplot2::waiver()) +
    ggplot2::coord_polar(start = -((dirres / 2) / 360) * 2 * pi) +
    ggplot2::scale_fill_manual(name = "Wind Speed (m/s)",
                               values = wind_colorpal,
                               drop = FALSE) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   legend.background = ggplot2::element_rect(fill = "gray96",
                                                             colour = "gray96"),
                   panel.background = ggplot2::element_rect(fill = "gray96",
                                                            colour = "gray96"),
                   panel.grid.minor.y = ggplot2::element_line(linewidth = 2),
                   panel.grid.major = ggplot2::element_line(colour = "gray86"),
                   panel.border = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(fill = "gray96",
                                                           colour = "gray96"),
                   strip.background = ggplot2::element_rect(fill = "gray96",
                                                            colour = "gray96"),
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), "lines"))
  
  
  if (plotit) {
    # print the plot #################
    print(plot_windrose)
  }
  
  # return the handle to the wind rose #################
  invisible(plot_windrose)
}


#' @title Plot the best results
#' @name plot_result
#' @description  Plot the best solutions of the genetic algorithm.
#'   Depending on \code{plotEn}, either the best energy or efficiency solutions
#'   can be plotted. \code{best} indicates the amount of best solutions to plot.
#'
#' @export
#'
#' @inheritParams genetic_algorithm
#' @param result The output of \code{\link{windfarmGA}} or
#'   \code{\link{genetic_algorithm}}
#' @param best A numeric value indicating how many of the best individuals
#'   should be plotted
#' @param plotEn A numeric value that indicates if the best energy or efficiency
#'   output should be plotted. \code{1} plots the best energy solutions
#'   and \code{2} plots the best efficiency solutions
#' @param topographie A logical value, indicating whether terrain effects should
#'   be considered and plotted or not
#' @param Grid If \code{TRUE} (default) the used grid will be added to the plot.
#'   You can also pass another Simple Feature object
#'
#' @family Plotting Functions
#' @return Returns a data.frame of the best (energy/efficiency) individual
#'   during all iterations
#'
#' @examples \donttest{
#' ## Add some data examples from the package
#' library(sf)
#' Polygon1 <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)))), 
#'   crs = 3035
#' ))
#'
#' ## Plot the results of a hexagonal grid optimization
#' plot_result(resulthex, Polygon1, best = 1, plotEn = 1, topographie = FALSE)
#'
#' ## Plot the results of a rectangular grid optimization
#' plot_result(resultrect, Polygon1, best = 1, plotEn = 1, topographie = FALSE)
#'}
plot_result <- function(result, Polygon1, best = 3, plotEn = 1,
                        topographie = FALSE, Grid = TRUE,
                        sourceCCLRoughness = NULL, sourceCCL = NULL,
                        weibullsrc) {
  ## Check plotEn, set par() and color palette ############## 
  if (!plotEn %in% c(1,2)) {
    stop("plotEn must be either 1 or 2. \n",
         "1 - plots the best energy output. \n",
         "2 - plots the best efficiency output.")
  }
  parpplotRes <- par(no.readonly = TRUE)
  par(mfrow = c(1, 1), mar = c(5, 6, 4, 2) + 0.1, mgp = c(5, 1, 0))
  rbPal1 <- grDevices::colorRampPalette(c('green','red'))
  result_inputs <- result[1,'inputData'][[1]]
  
  ## Check Projections and reference systems ####
  Polygon1 <- isSpatial(Polygon1)
  PROJ6 <- utils::compareVersion(sf::sf_extSoftVersion()[[3]], "6") > 0
  Projection <- result_inputs['Projection',][[1]]
  if (PROJ6) {
    Projection <- tryCatch(as.integer(Projection), 
                           warning = function(e) Projection,
                           error = function(e) Projection)
  }
  if (is.na(st_crs(Polygon1))) {
    message("Polygon is not projected. The spatial reference WGS 84 (EPSG:4326) is assumed.")
    if (PROJ6) {
      st_crs(Polygon1) <- 4326
    } else {
      st_crs(Polygon1) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
    }
  }
  Polygon1 <- sf::st_transform(Polygon1, st_crs(Projection))
  
  ## Check Weibull Rasters #########
  if (missing(weibullsrc)) {
    weibullsrc <- NULL
    col2res <- "lightblue"
  }
  else {
    ## TODO
    browser()
    PolyCrop <- sf::st_transform(Polygon1, sf::st_crs(weibullsrc[[1]]))
    if (inherits(weibullsrc,"list") & length(weibullsrc) == 2) {
      wblcroped <- lapply(weibullsrc, function(x){
        terra::crop(x, Polygon1::ext(PolyCrop))})
      wblcroped <- lapply(wblcroped, function(x){
        terra::mask(x, PolyCrop)})
      Erwartungswert <- wblcroped[[2]] * (gamma(1 + (1 / wblcroped[[1]])))
    } else if (inherits(weibullsrc,"list") & length(weibullsrc) == 1) {
      wblcroped <- terra::crop(weibullsrc[[1]], terra::ext(PolyCrop))
      wblcroped <- terra::mask(weibullsrc[[1]], PolyCrop)
      Erwartungswert <- wblcroped[[1]]
    } else if (inherits(weibullsrc,"RasterLayer")) {
      wblcroped <- terra::crop(weibullsrc, terra::ext(PolyCrop))
      wblcroped <- terra::mask(weibullsrc, PolyCrop)
      Erwartungswert <- wblcroped
    }
    col2res <- "transparent"
    alpha <- 0.9
    Erwartungswert <- terra::project(Erwartungswert, terra::crs(Polygon1))
  }
  
  ## Check & Make Grid #############
  if (isTRUE(Grid)) {
    cellsize <- as.numeric(result_inputs["Resolution",][[1]])
    if (toupper(result_inputs["Grid Method",][[1]]) == "RECTANGULAR") {
      Grid <- grid_area(Polygon1, size = cellsize,
                        prop = as.numeric(result_inputs["Percentage of Polygon",][[1]]))[[2]]
    } else {
      Grid <- hexa_area(Polygon1, size = cellsize)[[2]]
    }
  }
  
  
  ## Check Terrain Modell #########
  if (topographie == TRUE) {
    Polygonwgs84 <-  sf::st_transform(Polygon1, 4326)
    srtm <- tryCatch(elevatr::get_elev_raster(locations = as(Polygonwgs84, "Spatial"), z = 11),
                     error = function(e) {
                       stop("\nDownloading Elevation data failed for the given Polygon.\n",
                            e, call. = FALSE)
                     })
    srtm <- terra::rast(srtm)
    srtm_crop <- terra::crop(srtm, Polygonwgs84)
    srtm_crop <- terra::mask(srtm_crop, Polygonwgs84)
    srtm_crop <- terra::project(srtm_crop, terra::crs(Polygon1))
    
    # Calculates Wind multiplier. Hills will get higher values, valleys will get lower values.
    orogr1 <- srtm_crop / as.numeric(terra::global(srtm_crop, fun="mean", na.rm = TRUE))
    
    if (is.null(sourceCCL)) {
      if (length(list.files(pattern = "g100_06.tif")) == 0) {
        message("\nNo land cover raster ('sourceCCL') was given. It will be downloaded from ",
                "the EEA-website.")
        ## download an zip CCL-tif
        download.file("http://github.com/YsoSirius/windfarm_data/raw/master/clc.zip", 
                      destfile = "clc.zip", 
                      method = "auto")
        unzip("clc.zip")
        unlink("clc.zip")
        ccl <- terra::rast("g100_06.tif")
      } else {
        sourceCCL <- list.files(pattern = "g100_06.tif", full.names = TRUE)
        ccl <- terra::rast(x = sourceCCL)
      }
    }
    # Include Corine Land Cover Raster to get an estimation of Surface Roughness
    cclPoly <- terra::crop(ccl, Polygon1)
    cclPoly1 <- terra::mask(cclPoly, mask = Polygon1)
    if (is.null(sourceCCLRoughness)) {
      path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
      source_ccl <- paste0(path, "clc_legend.csv")
    } else {
      cat("\nYou are using your own Corine Land Cover legend.")
      # readline(prompt = "\nPress <ENTER> if you want to continue")
      source_ccl <- sourceCCLRoughness
    }
    rauhigkeitz <- utils::read.csv(source_ccl, header = TRUE, sep = ";")
    cclRaster <- terra::classify(cclPoly1, matrix(c(rauhigkeitz$GRID_CODE,
                                                    rauhigkeitz$Rauhigkeit_z),
                                                  ncol = 2))
  }
  
  
  ## Set Argments for Best Energy/Efficiency Windfarm ##########
  if (plotEn == 1) {
    filter_col <- "EnergyOverall"
    listind <- 2
    title <- "Energy"
  }
  if (plotEn == 2) {
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
  rectid <- lapply(result, function(x) x[, 'Rect_ID'])
  rectidt <- !duplicated(rectid)
  result <- result[rectidt]
  ndif <- length(result)
  cat(paste("N different optimal configurations:", ndif, "\nAmount duplicates:", 
            (ledup - ndif)))
  
  ## Check for enough results #########
  if (ndif < best) {
    cat(paste("\nNot enough unique Optimas. Show first best Half of different configurations."))
    best <- trunc(ndif / 2)
  }
  if (best == 0) best = 1
  
  ## Pick the `best` results to plot and Loop over #########
  result <- result[(length(result) - best + 1):(length(result))]
  for (i in (1:length(result))) {
    ## Get result ###########
    best_result <- data.frame(result[[i]])
    best_result$EnergyOverall <- round(best_result[, 'EnergyOverall'], 2)
    best_result$EfficAllDir <- round(best_result[, 'EfficAllDir'], 2)
    
    ## Color code locations #########
    br <- length(levels(factor(best_result[, 'AbschGesamt'])))
    if (br > 1) {
      Col <- rbPal1(br)[as.numeric(cut(as.numeric(
        best_result[, 'AbschGesamt']), breaks = br))]
    } else {
      Col <- "green"
    }
    
    ## Plot Best Windfarm  ###########
    cat(paste("\nPlot ", (best + 1) - i, " Best ",title," Solution:\n"))
    par(mfrow = c(1, 1), ask = FALSE)
    plot(st_geometry(Polygon1), col = col2res, 
         main = paste((best + 1) - i, "Best ", title, " Windfarm", 
                      "\nEnergy Output", best_result$EnergyOverall[[1]], "kW",
                      "\nEfficiency:", best_result$EfficAllDir[[1]], "%"),
         cex.main = 0.8)
    if (best > 1) {
      if (i > 1) {
        par(ask = TRUE)
      }
    }
    
    ## Plot Weibull Data ###########      
    if (!is.null(weibullsrc)) {
      terra::plot(Erwartungswert, alpha = alpha, legend = TRUE, axes = FALSE,
                   useRaster = TRUE, add = TRUE,
                   legend.lab = "Mean Wind Speed")
    }
    
    ## Plot Grid  ###########
    if (inherits(Grid, "sf")  || inherits(Grid, "sfc")) {
      plot(Grid, add = TRUE)
    }
    
    ## Plot Turbines and additional Info ###########
    graphics::mtext("Total Wake Effect in %", side = 2, cex = 0.8)
    graphics::points(best_result[, 'X'], best_result[, 'Y'],
                     cex = 2, pch = 20, col = Col)
    graphics::text(best_result[, 'X'], best_result[, 'Y'],
                   round(best_result[, 'AbschGesamt'], 0),
                   cex = 0.8, pos = 1, col = "black")
    
    distpo <- stats::dist(x = cbind(best_result[, 'X'], best_result[, 'Y']),
                          method = "euclidian")
    graphics::mtext(paste("minimal Distance", round(min(distpo), 2)),
                    side = 1, line = 0, cex = 0.8)
    graphics::mtext(paste("mean Distance", round(mean(distpo), 2)),
                    side = 1, line = 1, cex = 0.8)
    
    ## Plot Terrain Model  ###########
    if (topographie == TRUE) {
      par(ask = TRUE)
      sel1 <- best_result[, 1:2]
      plot_terrain(result_inputs, sel1, Polygon1, orogr1, srtm_crop, cclRaster)
    }
  }
  
  ## Reset par() and return best windfarm  ###########
  par(parpplotRes)
  invisible(best_result)
}
plot_terrain <- function(inputs, sel1, polygon1, orogr1, srtm_crop, cclRaster) {
  ## Plot DEM and windspeed multiplier ############
  orogrnum <- terra::extract(x = orogr1, y = as.matrix(sel1))
  windpo <- 1 * orogrnum
  ## Get Elevation of Turbine Locations to estimate the air density at the resulting height
  heightWind <- terra::extract(x = srtm_crop, y = as.matrix(sel1))
  par(mfrow = c(1, 2))
  cexa <- 0.9
  terra::plot(srtm_crop, main = "Elevation Data")
  graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
  calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                    labs = round(heightWind, 0), cex = 0.8)
  plot(polygon1, add = TRUE)
  terra::plot(orogr1, main = "Wind Speed Multipliers")
  points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
  calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                    labs = round(windpo, 3), cex = 0.8)
  plot(polygon1, add = TRUE)
  
  ## Get Air Density and Pressure from Height Values #########
  HeighttoBaro <- matrix(heightWind); colnames(HeighttoBaro) <- "HeighttoBaro"
  air_dt <- barometric_height(matrix(HeighttoBaro), HeighttoBaro)
  terra::plot(srtm_crop, main = "Normal Air Density",
               col = topo.colors(10))
  points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
  calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                    labs = rep(1.225, nrow(sel1)), cex = 0.8)
  plot(polygon1, add = TRUE)
  terra::plot(srtm_crop, main = "Corrected Air Density",
               col = topo.colors(10))
  points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
  calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                    labs = round(air_dt[, 'rh'], 2), cex = 0.8)
  plot(polygon1, add = TRUE)
  
  ## CorineLandCover Roughness values ##################
  SurfaceRoughness0 <- terra::extract(x = cclRaster, y = as.matrix(sel1))
  SurfaceRoughness1 <- terra::extract(x = terra::terrain(srtm_crop, "roughness"),
                                       y = as.matrix(sel1))
  SurfaceRoughness <- SurfaceRoughness0 * (1 + (SurfaceRoughness1 / max(terra::res(srtm_crop))))
  elrouind <- terra::terrain(srtm_crop, "roughness")
  elrouindn <- terra::resample(elrouind, cclRaster, method = "near")
  modSurf <- terra::lapp(x = cclRaster, y = elrouindn,
                             fun = function(x, y) {
                               return(x * (1 + (y / max(terra::res(srtm_crop)))))
                             })
  
  par(mfrow = c(1, 2))
  terra::plot(cclRaster, main = "Corine Land Cover Roughness")
  points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
  calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                    labs = round(SurfaceRoughness0, 2), cex = cexa)
  plot(polygon1, add = TRUE)
  terra::plot(x = elrouindn,
               main = "Elevation Roughness Indicator")
  points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
  calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                    labs = round((SurfaceRoughness1), 2), cex = cexa)
  plot(polygon1, add = TRUE)
  terra::plot(modSurf, main = "Modified Surface Roughness")
  points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
  calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                    labs = round((SurfaceRoughness), 2), cex = cexa)
  plot(polygon1, add = TRUE)
  
  ## Wake Decay Constant #############
  RotorHeight <- as.integer(inputs['Rotor Height',])
  k_raster <- terra::app(modSurf, function(x) {x <- 0.5 / (log(RotorHeight / x))})
  # New Wake Decay Constant calculated with new surface roughness values, according to CLC
  k <- 0.5 / (log(RotorHeight / SurfaceRoughness))
  terra::plot(k_raster, main = "Adapted Wake Decay Constant - K")
  points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
  calibrate::textxy(sel1[, 'X'], sel1[, 'Y'], labs = round(k, 3), cex = cexa)
  plot(polygon1, add = TRUE)
}

#' @title Plot the results of an optimization run
#' @name plot_windfarmGA
#' @description  Plot the results of a genetic algorithm run with given inputs.
#'   Several plots try to show all relevant effects and outcomes of the
#'   algorithm. 6 plot methods are available that can be selected individually.
#'
#' @export
#'
#' @inheritParams plot_result
#' @param whichPl Which plots should be shown: 1-6 are possible. The default is
#'   "all" which shows all available plots
#'
#' @family Plotting Functions
#' @return NULL
#' @examples \donttest{
#' library(sf)
#' Polygon1 <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)))), 
#'   crs = 3035
#' ))
#'
#' ## Plot the results of a hexagonal grid optimization
#' plot_windfarmGA(resulthex, Polygon1, whichPl = "all", best = 1, plotEn = 1)
#'
#' ## Plot the results of a rectangular grid optimization
#' plot_windfarmGA(resultrect, Polygon1, whichPl = "all", best = 1, plotEn = 1)
#' }
plot_windfarmGA <- function(result, Polygon1, whichPl = "all", 
                            best = 1, plotEn = 1,
                            weibullsrc) {
  
  parpplotWindGa <- par(ask = FALSE, no.readonly = TRUE)
  on.exit(par(parpplotWindGa))
  
  ## DATA #################
  if (any(whichPl == "all")) {
    whichPl <- 1:6
  }
  resol <- as.numeric(result[, 'inputData'][[1]][,1]['Resolution'][[1]])
  Polygon1 <- isSpatial(Polygon1)
  if (nrow(result) < 4) {
    if (any(2:5 %in% whichPl)) {
      cat("Cannot plot option 2,3,4,5. \n Only option 1,6 are available.")
      whichPl <- c(1, 6)
    }
  }
  #################
  
  ## PLOTTING OUTPUTS ####################
  if (any(whichPl == 1)) {
    print("plot_result: Plot the 'best' Individuals of the GA:")
    plot_result(result = result, Polygon1 = Polygon1, best = best , plotEn = plotEn,
                topographie = FALSE, Grid = TRUE, weibullsrc = weibullsrc)
    readline(prompt = "Press [enter] to continue")
  }
  if (any(whichPl == 2)) {
    print("plot_evolution: Plot the Evolution of the Efficiency and Energy Values:")
    plot_evolution(result, TRUE, 0.3)
  }
  if (any(whichPl == 3)) {
    print("plot_parkfitness: Plot the Influence of Population Size, Selection, Crossover, Mutation:")
    plot_parkfitness(result, 0.1)
    readline(prompt = "Press [enter] to continue")
  }
  if (any(whichPl == 4)) {
    print("plot_fitness_evolution: Plot the Changes in Fitness Values:")
    plot_fitness_evolution(result)
    readline(prompt = "Press [enter] to continue")
  }
  if (any(whichPl == 5)) {
    print("plot_cloud: Plot all individual Values of the whole Evolution:")
    plot_cloud(result, TRUE)
    readline(prompt = "Press [enter] to continue")
  }
  if (any(whichPl == 6)) {
    print("plot_heatmap: Plot a Heatmap of all Grid Cells:")
    plot_heatmap(result = result, si = 2)
    # readline(prompt = "Press [enter] to continue")
  }
  # if (any(whichPl==7)){
  #   print("GooglePlot: Plot the 'best' Individual with static Google Map:")
  #   GooglePlot(result,Polygon1,best,plotEn,Projection)
  #   readline(prompt="Press [enter] to continue")
  # }
  # if (any(whichPl==8)){
  #   print("GoogleChromePlot: Plot the 'best' Individual with Leaflet with Satelitte Imagery:")
  #   GoogleChromePlot(result,Polygon1,best,plotEn,Projection)
  # }
  return()
}

#' @title Plot a wind warm with leaflet
#' @name plot_leaflet
#' @description  Plot a resulting wind farm on a leaflet map.
#'
#' @export
#'
#' @inheritParams plot_result
#' @param which A numeric value, indicating which individual to plot. The
#'   default is 1. Combined with \code{orderitems = TRUE} this will show the 
#'   best performing wind farm.
#' @param orderitems A logical value indicating whether the results should be
#'   ordered by energy values \code{TRUE} or chronologically \code{FALSE}
#' @param GridPol By default, the grid will be calculated based on the inputs 
#'  of \code{result} and the \code{Polygon1}. But another spatial object or the 
#'  output of the  \code{\link{grid_area}} or \code{\link{hexa_area}} functions 
#'  can also be
#'
#' @return Returns a leaflet map.
#'
#' @examples \donttest{
#' ## Plot the best wind farm on a leaflet map (ordered by energy values)
#' plot_leaflet(result = resulthex, Polygon1 = sp_polygon, which = 1)
#'
#' ## Plot the last wind farm (ordered by chronology).
#' plot_leaflet(result = resulthex, Polygon1 = sp_polygon, orderitems = FALSE,
#'          which = 1)
#'          
#' ## Plot the best wind farm on a leaflet map with the rectangular Grid
#' Grid <- grid_area(sp_polygon, size = 150, prop = 0.4)
#' plot_leaflet(result = resultrect, Polygon1 = sp_polygon, which = 1, 
#'              GridPol = Grid[[2]])
#'
#' ## Plot the last wind farm with hexagonal Grid
#' Grid <- hexa_area(sp_polygon, size = 75)
#' plot_leaflet(result = resulthex, Polygon1 = sp_polygon, which = 1, 
#'              GridPol = Grid[[2]])
#' }
plot_leaflet <- function(result, Polygon1, which = 1, orderitems = TRUE, GridPol) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("The package 'leaflet' is required for this function, but it is not installed.\n",
         "Please install it with `install.packages('leaflet')`")
  }
  
  ## Check Polygon and CRS ##############
  poly1 <- isSpatial(shape = Polygon1)
  if (is.na(st_crs(poly1))) {
    projection <- result[, "inputData"][[1]]["Projection", ][[1]]
    projection <- tryCatch(as.numeric(projection),
                           warning = function(e) projection,
                           error = function(e) projection)
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
      subset.matrix(i, subset = c(TRUE, rep(FALSE, nrow(i) - 1)),
                    select = "EnergyOverall")
    })
    order1 <- order(a, decreasing = TRUE)
    result <- result[order1, ]
    beste <- which
  } else {
    beste <- ""
  }
  
  ## WGS84 Projection for old and new GDAL ##############
  PROJ6 <- utils::compareVersion(sf::sf_extSoftVersion()[[3]], "6") > 0
  if (PROJ6) {
    proj_longlat <- 4326
  } else {
    proj_longlat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  
  ## Grid-Function ##############
  if (!missing(GridPol)) {
    if (is.na(st_crs(GridPol))) {
      st_crs(GridPol) <- st_crs(proj_pol)
    }
  } else {
    cellsize <- as.numeric(result[, "inputData"][[1]]["Resolution",][[1]])
    if (result[, "inputData"][[1]]["Grid Method",][[1]] != "h") {
      GridPol <- grid_area(poly1, cellsize, 
                           prop = as.numeric(result[, "inputData"][[1]]["Percentage of Polygon",][[1]]),
                           plotGrid = FALSE)[[2]]
    } else {
      GridPol <- hexa_area(poly1, cellsize,  plotGrid = FALSE)[[2]]
    }
  }
  GridPol <- st_transform(GridPol, st_crs(proj_longlat))
  
  ## Pick a Windfarm and Project to WGS84 ##############
  result <- result[, "bestPaEn"][[which]]
  xysp <- st_as_sf(data.frame(result), coords=c("X","Y"))
  st_crs(xysp) <- proj_pol
  resultxy <- st_coordinates(st_transform(xysp, proj_longlat))
  result <- data.frame(result, stringsAsFactors = FALSE)
  result$X <- resultxy[, 1]
  result$Y <- resultxy[, 2]
  result$wake_radius <- round(result$AbschGesamt, 2) / 10
  
  poly1 <- st_transform(poly1, proj_longlat)
  
  ## Get Coordinates for Title ##############
  bbx <- st_bbox(poly1)
  title_locat <- c(mean(bbx[c(1,3)]), max(bbx[c(2,4)]))
  
  ## Color Coding ##############
  col_cir <- grDevices::colorRampPalette(c("green", "yellow",
                                           "red", "darkred"))
  br <- length(levels(factor(result$AbschGesamt)))
  if (br > 1) {
    color_pal <- col_cir(br)
  } else {
    color_pal <- "green"
  }
  pal <- leaflet::colorFactor(color_pal, domain = sort(result$AbschGesamt), 
                              ordered = TRUE,
                              reverse = FALSE)
  result$farbe <- pal(result$AbschGesamt)
  
  
  ## Turbine Icons ###########
  turbine_icon <- leaflet::iconList(
    turbine_icon = leaflet::makeIcon(
      iconUrl = paste0(system.file(package = "windfarmGA"),
                       "/extdata/windturdk.png"),
      iconWidth = 30, iconHeight = 50))
  list_popup <- paste("Total Wake Effect: ", as.character(result$AbschGesamt),
                      "% </dd>")
  
  
  ## Plot a Leaflet Map ###################
  overlay_group <- c("Wake Circles", "Title", "Polygon", "Turbines", "Grid")
  opaycity <- 0.4
  map <- leaflet::leaflet() %>%
    leaflet::addTiles(group = "OSM") %>%
    leaflet::addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    leaflet::addProviderTiles("Stamen.Toner", group = "Toner") %>%
    ## Write a Popup with the energy output
    leaflet::addPopups(title_locat[1], (title_locat[2] + 0.0002),
                       group = "Title",
                       popup = paste(beste, "<b>Best Wind Farm with: ",
                                     round(as.numeric(
                                       result[, "EnergyOverall"][[1]]), 2),
                                     "kWh</b>"),
                       options = leaflet::popupOptions(
                         closeButton = TRUE, closeOnClick = FALSE)) %>%
    ## Add the Polygon
    leaflet::addPolygons(data = poly1, group = "Polygon",
                fill = TRUE, fillOpacity = 0.4) %>%
    ## Add the Genetic Algorithm Space
    leaflet::addPolygons(data = GridPol, group = "Grid", weight = 1,
                opacity = opaycity,
                fill = TRUE, fillOpacity = 0) %>%
    ## Create Circles in Map
    leaflet::addCircleMarkers(lng = result$X, lat = result$Y,
                     radius = result$wake_radius,
                     color = result$farbe,
                     stroke = TRUE, fillOpacity = 0.8,
                     group = "Wake Circles") %>%
    ## Add the turbine symbols
    leaflet::addMarkers(lng = result[, 1], lat = result[, 2],
               icon = turbine_icon[1], popup = list_popup,
               group = "Turbines") %>%
    leaflet::addLegend(position = "topleft",
              pal = pal,
              values = result$AbschGesamt,
              labFormat = leaflet::labelFormat(suffix = "%"),
              opacity = 1, title = "Total Wake Effect",
              layerId = "Legend") %>%
    ## Layers control
    leaflet::addLayersControl(baseGroups = c(
      "OSM",
      "Terrain",
      "Satellite",
      "Toner"),
      overlayGroups = overlay_group,
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
  
  # Plot the map
  map
}

#' @title Plot the genetic algorithm results
#' @name plot_parkfitness
#' @description Plot the evolution of fitness values with the influences of
#'   selection, crossover and mutation.
#' @export
#'
#' @inheritParams plot_result
#' @param spar A numeric value determining how exact a spline should be drawn.
#'   Default is 0.1
#'
#' @family Plotting Functions
#' @return NULL
#' @examples \donttest{
#' ## Plot the results of a hexagonal grid optimization
#' plot_parkfitness(resulthex)
#'}
plot_parkfitness <- function(result, spar = 0.1) {
  ## Data #####################
  rslt <- as.data.frame(do.call("rbind", result[, "allparkcoeff"]))
  mutres <- as.data.frame(do.call("rbind", result[, "mut_rate"]))
  nindiv1 <- as.data.frame(do.call("cbind", result[, "nindiv"]))
  nindiv1 <- nindiv1[-seq(4, length(nindiv1), 4)]
  
  selcross <- unlist(result[, "selcross"])
  selteil <- selcross[seq(2, length(selcross), 2)]
  crossteil <- selcross[seq(1, length(selcross), 2)]
  #######################
  
  ## Set graphics param #####################
  parparfit <- par(ask = FALSE, no.readonly = TRUE)
  on.exit(par(parparfit))
  graphics::layout(matrix(c(1, 1, 1, 1, 2, 3, 4, 5), 2, 4, byrow = TRUE))
  rbPal <- grDevices::colorRampPalette(c("red", "green"))
  Col <- rbPal(4)[as.numeric(cut(as.numeric(rslt$maxparkfitness), breaks = 4))]
  #######################
  
  ## Plot All together (5 Plots) #####################
  plot(rslt$minparkfitness, xaxt = "n", main = "Parkfitness per Generation", pch = 20,
       ylab = "Parkfitness", xlab = "Generation", cex = 1, cex.main = 1, 
       col = "red", ylim = c(min(rslt$minparkfitness), max(rslt$maxparkfitness)))
  graphics::axis(1, at = 1:nrow(rslt), tick = TRUE)
  graphics::points(rslt$meanparkfitness, ylab = "MeanParkF", cex = 1.2, col = "blue", pch = 20)
  graphics::points(rslt$maxparkfitness, ylab = "maxParkF", cex = 1.2, col = "green", pch = 20)
  x <- 1:length(rslt$maxparkfitness)
  
  if (nrow(result) >= 4) {
    lmin <- stats::smooth.spline(x, rslt$minparkfitness, spar = spar)
    graphics::lines(lmin, col = "red", lwd = 1.2)
    lmea <- stats::smooth.spline(x, rslt$meanparkfitness, spar = spar)
    graphics::lines(lmea, col = "blue", lwd = 1.2)
    lmax <- stats::smooth.spline(x, rslt$maxparkfitness, spar = spar)
    graphics::lines(lmax, col = "green", lwd = 1.2)
    graphics::grid(col = "gray")
  }
  
  par(ask = TRUE)
  par(mar = c(5,5,3,2))
  farbe <- rep(seq(1,3,1), length(nindiv1)/3)
  ndindiplot <- as.integer(nindiv1)
  plot(ndindiplot, type = "b", col = farbe, cex = 1.5, cex.main = 1, pch = 20, 
       main = "Population Size", axes = FALSE, xlab = "Generation",
       ylab = "Amount of Individuals", ylim = c(0, max(ndindiplot) + 100))
  axis(side = 2, tick = TRUE) 
  axis(side = 1, tick = TRUE, at = seq(1, length(ndindiplot),3),
       labels = (1:(length(ndindiplot)/3)))
  legend("topleft", title = "Amount of Individuals in: ", lty = c(1,1,1),
         cex = 0.5, inset = c(0.01, 0.01),
         box.lty = 0, box.lwd = 0, c("Fitness","Selection","Crossover"), 
         col = farbe[1:3], xjust = 0)
  
  plot(1*100/selteil, ylim = c(20, 110), type = "b", cex = 2, cex.main = 1,
       col = "green", pch = 20, main = "Selection",
       ylab = "Percentage", xlab = "Generation")
  graphics::grid(col = "gray")
  selrpl <- 1*100/selteil
  timeticksel <- which(selrpl > 75)
  selrplval <- selrpl[selrpl > 75]
  calibrate::textxy(timeticksel, selrplval, labs = timeticksel, cex = 0.7)
  
  
  plot(crossteil, col = crossteil, main = "Crossover",
       xlab = "Generation", ylab = "Crossover Points",
       ylim = c(1, 8), cex = 1, cex.main = 1, pch = 15)
  graphics::grid(col = "gray")
  timetickcro <- which(crossteil > median(crossteil))
  crorplval <- crossteil[crossteil > median(crossteil)]
  calibrate::textxy(timetickcro, crorplval, labs = timetickcro, cex = 0.5)
  
  plot(as.numeric(t(mutres)), type = "b", main = "Mutation", xlab = "Generation",
       ylab = "Mutation Percentage", cex = 1, cex.main = 1, pch = 15)
  mutrpl <- as.numeric(t(mutres))
  timetick <- which(mutrpl > median(mutrpl))
  mutrplval <- mutrpl[mutrpl > median(mutrpl)]
  calibrate::textxy(timetick, mutrplval, labs = timetick, cex = 0.7)
  grid(col = "gray")
  #######################
  
  ## Plot Count Individuals #####################
  par(mfrow = c(1,1))
  plot(ndindiplot, type = "b", col = farbe, cex = 1.5, cex.main = 1, pch = 20, 
       main = "Population Size", axes = FALSE, xlab = "Generation",
       ylab = "Amount of Individuals", ylim = c(0, max(ndindiplot) + 100))
  axis(side = 2, tick = TRUE) 
  axis(side = 1, tick = TRUE, at = seq(1, length(ndindiplot), 3),
       labels = (1:(length(ndindiplot)/3)))
  
  graphics::legend("topleft", title = "Amount of Individuals in: ", pch = c(20,20,20),
                   cex = 1, inset = c(0.01,0.01),
                   box.lty = 0, box.lwd = 0, c("Fitness","Selection","Crossover"),
                   text.col = farbe[1:3], col = farbe[1:3], xjust = 0)
  #######################
  
  ## Plot Selection / Crossover Params #####################
  graphics::par(mfrow = c(2,1))
  plot(1*100/selteil, ylim = c(20,110), type = "b", cex = 2, col = "green",
       pch = 20, main = "Selection",
       ylab = "Percentage", xlab = "Generation")
  graphics::grid(col = "gray")
  selrpl <- 1*100/selteil
  timeticksel <- which(selrpl > 75)
  selrplval <- selrpl[selrpl > 75]
  calibrate::textxy(timeticksel, selrplval, labs = timeticksel, cex = 0.5)
  plot(crossteil, col = crossteil, main = "Crossover", xlab = "Generation",
       ylab = "Crossover Points", ylim = c(1,8), cex = 1, pch = 15)
  grid(col = "gray")
  timetickcro <- which(crossteil > median(crossteil))
  crorplval <- crossteil[crossteil > median(crossteil)]
  calibrate::textxy(timetickcro, crorplval, labs = timetickcro, cex = 0.5)
  #######################
  ## Add Special Events #######################
  if (length(timetick) != 0) {
    graphics::par(mfrow = c(1, 1))
    rbPal <- grDevices::colorRampPalette(c("red","green"))
    Col <- rbPal(4)[as.numeric(cut(as.numeric(rslt$maxparkfitness), breaks = 4))]
    plot(rslt$minParkwirkungsg, xaxt = "n", main = "Mutation Influence", 
         ylab = " in %", cex = 1.2, cex.main = 1, 
         col = "red", pch = 20, xlab = "Generation",
         ylim = c(min(rslt$minParkwirkungsg),max(rslt$maxParkwirkungsg)))
    graphics::axis(1, at = 1:nrow(rslt), tick = TRUE)
    graphics::points(rslt$meanParkwirkungsg, ylab = "MeanParkEff", cex = 1.2, 
                     col = "blue", pch = 20)
    graphics::points(rslt$maxParkwirkungsg, ylab = "maxParkEff", cex = 1.2, 
                     col = "green", pch = 20)
    x <- 1:length(rslt$maxparkfitness)
    if (nrow(result) >= 4) {
      lmin <- stats::smooth.spline(x, rslt$minParkwirkungsg, spar = spar) 
      graphics::lines(lmin, col = "red", lwd = 1.2)
      lmea <- stats::smooth.spline(x, rslt$meanParkwirkungsg, spar = spar) 
      graphics::lines(lmea, col = "blue", lwd = 1.2)
      lmax <- stats::smooth.spline(x, rslt$maxParkwirkungsg, spar = spar) 
      graphics::lines(lmax, col = "green", lwd = 1.2)
      graphics::grid(col = "gray")
    }
    graphics::abline(v = timetick, col = "black")
    graphics::mtext(mutrplval, side = 3, at = timetick, cex = 0.8)
  }
  if (length(timeticksel) != 0) {
    graphics::par(mfrow = c(1,1))
    rbPal <- grDevices::colorRampPalette(c("red","green"))
    Col <- rbPal(4)[as.numeric(cut(as.numeric(rslt$maxparkfitness), breaks = 4))]
    graphics::plot(rslt$minParkwirkungsg, xaxt = "n", 
                   main = "Selection Influence", ylab = " in %", cex = 1, 
                   cex.main = 1, col = "red",xlab = "Generation",
                   pch = 20, ylim = c(min(rslt$minParkwirkungsg),max(rslt$maxParkwirkungsg)))
    graphics::axis(1, at = 1:nrow(rslt), tick = TRUE)
    graphics::points(rslt$meanParkwirkungsg, ylab = "MeanParkEff", 
                     cex = 1.2, col = "blue", pch = 20)
    graphics::points(rslt$maxParkwirkungsg, ylab = "maxParkEff", 
                     cex = 1.2, col = "green", pch = 20)
    x <- 1:length(rslt$maxparkfitness)
    if (nrow(result) >= 4) {
      lmin <- stats::smooth.spline(x, rslt$minParkwirkungsg, spar = spar)
      graphics::lines(lmin, col = "red", lwd = 1.2)
      lmea <- stats::smooth.spline(x, rslt$meanParkwirkungsg, spar = spar)
      graphics::lines(lmea, col = "blue", lwd = 1.2)
      lmax <- stats::smooth.spline(x, rslt$maxParkwirkungsg, spar = spar)
      graphics::lines(lmax, col = "green", lwd = 1.2)
      graphics::grid(col = "gray")
    }
    abline(v = timeticksel,col = "green")
    mtext(selrplval, side = 3,at = timeticksel, col = "green", cex = 0.8)
  }
  if (length(timetickcro) != 0 ) {
    par(mfrow = c(1,1))
    rbPal <- colorRampPalette(c("red","green"))
    Col <- rbPal(4)[as.numeric(cut(as.numeric(rslt$maxparkfitness),
                                   breaks = 4))]
    plot(rslt$minParkwirkungsg, xaxt = "n", main = "Crossover Influence",
         ylab = " in %", cex = 1, cex.main = 1, col = "red", xlab = "Generation",
         pch = 20, ylim = c(min(rslt$minParkwirkungsg),max(rslt$maxParkwirkungsg)))
    axis(1, at = 1:nrow(rslt), tick = TRUE)
    points(rslt$meanParkwirkungsg, ylab = "MeanParkEff", cex = 1.2,
           col = "blue", pch = 20)
    points(rslt$maxParkwirkungsg, ylab = "maxParkEff", cex = 1.2,
           col = "green", pch = 20)
    x <- 1:length(rslt$maxparkfitness)
    if (nrow(result) >= 4) {
      lmin <- stats::smooth.spline(x, rslt$minParkwirkungsg, spar = spar)
      graphics::lines(lmin, col = "red", lwd = 1.2)
      lmea <- stats::smooth.spline(x, rslt$meanParkwirkungsg, spar = spar)
      graphics::lines(lmea, col = "blue", lwd = 1.2)
      lmax <- stats::smooth.spline(x, rslt$maxParkwirkungsg, spar = spar)
      graphics::lines(lmax, col = "green", lwd = 1.2)
      graphics::grid(col = "gray")
    }
    graphics::abline(v = timetickcro, col = "red")
    graphics::mtext(crorplval, side = 3,at = timetickcro, col = "red", cex = 0.8)
  }
  #######################
  
  ## Plot Fitness, Selection, Crossover and Fitness Deviation #####################
  sddata <- plot_cloud(result)
  fitsd <- sddata[, grep(pattern = "Fit", colnames(sddata)), drop=FALSE]
  effsd <- sddata[, grep(pattern = "Eff", colnames(sddata)), drop=FALSE]
  enesd <- sddata[, grep(pattern = "Ene", colnames(sddata)), drop=FALSE]
  graphics::par(mfrow = c(4, 1))
  plot(rslt$minparkfitness, xaxt = "n", main = "Parkfitness per Generation", 
       ylab = "Parkfitness", xlab = "Generation",
       cex = 1, cex.main = 1, col = "red", pch = 20,
       ylim = c(min(rslt$minparkfitness), max(rslt$maxparkfitness)))
  graphics::axis(1, at = 1:nrow(rslt), tick = TRUE)
  graphics::grid(col = "black")
  graphics::points(rslt$meanparkfitness, ylab = "MeanParkF",
                   cex = 1.2, col = "blue", pch = 20)
  graphics::points(rslt$maxparkfitness, ylab = "maxParkF",
                   cex = 1.2, col = "green", pch = 20)
  x <- 1:length(rslt$maxparkfitness)
  if (nrow(result) >= 4) {
    lmin <- smooth.spline(x, rslt$minparkfitness, spar = spar) 
    graphics::lines(lmin, col = "red", lwd = 1.2)
    lmea <- smooth.spline(x, rslt$meanparkfitness, spar = spar)
    graphics::lines(lmea, col = "blue", lwd = 1.2)
    lmax <- smooth.spline(x, rslt$maxparkfitness, spar = spar) 
    graphics::lines(lmax, col = "green", lwd = 1.2)
    graphics::grid(col = "gray")
  }
  
  plot(100 / selteil, ylim = c(20, 110), type = "b", lwd = 2, col = "green",
       pch = 20, main = "Selection",
       ylab = "Percentage", xlab = "Generation", cex.main = 1)
  graphics::grid(lty = 2)
  
  plot(crossteil, col = crossteil, pch = 20, cex.main = 1, cex = 2,
       main = "Crossover", xlab = "Generation", ylab = "Crossover Points",
       ylim = c(1, 6))
  graphics::grid(lty = 2)
  
  plot(enesd[,"EneSD"], type = "b", col = "blue", pch = 20, lwd = 2,
       ylab = "Energy/Efficiency/Fitness Deviation", xlab = "Generation",
       cex.main = 1, main = "Standard Deviation")
  graphics::grid(lty = 2)
  graphics::par(new = TRUE)
  plot(effsd[, "EffSD"], type = "b", col = "orange", lwd = 2, axes = FALSE, 
       bty = "n", xlab = "", ylab = "", pch = 20)
  graphics::par(new = TRUE)
  plot(fitsd[,"FitSD"], type = "b", col = "red", lwd = 2, axes = FALSE, 
       bty = "n", xlab = "", ylab = "",pch = 20)
  
  timeticksd <- which(mutrpl > median(mutrpl))
  sdrplval <- fitsd[,'FitSD'][timeticksd]
  if (length(timeticksd) != 0) {
    calibrate::textxy(timeticksd, sdrplval, labs = timeticksd, cex = 0.5)
    graphics::abline(v = timeticksd)
    graphics::mtext(mutrplval, side = 3, at = timetick, cex = 0.8)
  }
  #######################
  
  ## Plot Mutation influence #####################
  graphics::par(mfrow = c(1, 1))
  plot(fitsd[,'FitSD'], type = "b", col = "red", lwd = 2, cex.main = 1, axes = TRUE,
       bty = "n", xlab = "Generation", ylab = "",
       pch = 20, main = "Mutation influence on Standard Deviation")
  if (length(timeticksd) != 0) {
    calibrate::textxy(timeticksd, sdrplval, labs = timeticksd, cex = 0.7)
    graphics::abline(v = timeticksd)
    graphics::mtext(mutrplval, side = 3,at = timetick,cex = 0.8)
  }
  
  return()
}

#' @title Plot the progress of populations
#' @name plot_development
#' @description Plot the changes in mean and max fitness values to previous
#' generation.
#'
#' @export
#'
#' @inheritParams plot_result
#' 
#' @family Plotting Functions
#' @return NULL
#' @examples \donttest{
#' plot_development(resultrect)
#' }
plot_development <- function(result) {
  parbeorwo <- par(ask = FALSE, no.readonly = TRUE)
  on.exit(par(parbeorwo))
  par(mfrow = c(2, 1))
  
  beorworse <- do.call("rbind", result[, 9])
  maxdif <- data.frame(
    diff = beorworse[, 1],
    farbe = 0)
  
  maxdif$farbe[maxdif$diff < 0]  <- "red"
  maxdif$farbe[maxdif$diff > 0] <- "green"
  maxdif$farbe[maxdif$diff == 0] <- "orange"
  plot(maxdif$diff, type = "b", col = maxdif$farbe, pch = 20, cex = 2)
  abline(0, 0)
  title("Max Difference to previous generation")
  
  
  meandif <- data.frame(
    diff = beorworse[, 2],
    farbe = 0)
  meandif$farbe[meandif$diff < 0]  <- "red"
  meandif$farbe[meandif$diff > 0] <- "green"
  meandif$farbe[meandif$diff == 0] <- "orange"
  plot(meandif$diff, type = "b", col = meandif$farbe, pch = 20, cex = 2)
  abline(0, 0)
  title("Mean Difference to previous generation")
  
  return()
}

#' @title Plot the evolution of fitness values
#' @name plot_evolution
#' @description  Plot the evolution of energy outputs and efficiency rates over
#'   the whole generations. Plots min, mean and max values.
#' @export
#'
#' @inheritParams plot_result
#' @param ask Should R wait for interaction for subsequent plotting. Default is
#'   TRUE
#' @param spar A numeric value determining how exact a spline should be drawn.
#'   Default is 0.1
#'
#' @family Plotting Functions
#' @return NULL
#' @examples \donttest{
#' ## Plot the results of a rectangular grid optimization
#' plot_evolution(resultrect, ask = TRUE, spar = 0.1)
#'}
plot_evolution <- function(result, ask = TRUE, spar = 0.1) {
  ## Set the graphical parameters
  parevol <- par(no.readonly = TRUE)
  on.exit(par(parevol))
  par(mfrow = c(1, 1))
  
  result1 <- as.data.frame(do.call("rbind", result[, 1]))
  
  plot(result1$minParkwirkungsg, xaxt = "n",
       main = "Park Efficiency per Generation",
       xlab = "Generation",
       ylab = "Park Efficiency in %", cex = 0.8, cex.main = 0.8,
       col = "red", pch = 20,
       ylim = c(min(result1$minParkwirkungsg), max(result1$maxParkwirkungsg)))
  axis(1, at = 1:nrow(result1), tick = TRUE)
  grid(col = "black")
  points(result1$meanParkwirkungsg, cex = 1.2, col = "blue", pch = 20)
  points(result1$maxParkwirkungsg, cex = 1.2, col = "green", pch = 20)
  x <- 1:length(result1$MaxEnergyRedu)
  
  if (nrow(result) >= 4) {
    lmin <- smooth.spline(x, result1$minParkwirkungsg, spar = spar)
    lines(lmin, col = "red", lwd = 1.2)
    lmea <- smooth.spline(x, result1$meanParkwirkungsg, spar = spar)
    lines(lmea, col = "blue", lwd = 1.2)
    lmax <- smooth.spline(x, result1$maxParkwirkungsg, spar = spar)
    lines(lmax, col = "green", lwd = 1.2)
  }
  
  par(ask = ask)
  
  plot(result1$MeanEnergyRedu, xaxt = "n",
       main = "Energy Yield per Generation",
       xlab = "Generation",
       ylab = "Energy in kW", cex = 0.8, cex.main = 0.8,
       col = "blue", pch = 20,
       ylim = c(min(result1$MinEnergyRedu), max(result1$MaxEnergyRedu)))
  axis(1, at = 1:nrow(result1), tick = TRUE)
  grid(col = "black")
  points(result1$MaxEnergyRedu, cex = 1.2, col = "green", pch = 20)
  points(result1$MinEnergyRedu, cex = 1.2, col = "red", pch = 20)
  
  if (nrow(result) >= 4) {
    emean <- smooth.spline(x, result1$MeanEnergyRedu, spar = spar)
    lines(emean, col = "blue", lwd = 1.2)
    emax <- smooth.spline(x, result1$MaxEnergyRedu, spar = spar)
    lines(emax, col = "green", lwd = 1.2)
    emin <- smooth.spline(x, result1$MinEnergyRedu, spar = spar)
    lines(emin, col = "red", lwd = 1.2)
  }
  
  return()
}

#' @title Plot outputs of all generations with standard deviations
#' @name plot_cloud
#' @description  Plot the fitness, efficiency and energy outputs of all
#'   generations and the corresponding standard deviations.
#'
#' @export
#'
#' @inheritParams plot_result
#' @param pl Should the results be plotted? Default is FALSE
#'
#' @family Plotting Functions
#' @return Returns a data.frame with the values for fitness, efficiency and
#'   energy for all evaluated individuals
#'
#' @examples \donttest{
#' ## Plot the results of a hexagonal grid optimization
#' plcdf <- plot_cloud(resulthex, TRUE)
#'}
plot_cloud <- function(result, pl = FALSE) {
  parcloud <- par(ask = FALSE, no.readonly = TRUE)
  on.exit(par(parcloud))
  ## Data Aggregation ##########
  clouddata <- result[, 7]
  efficiency_cloud <- lapply(clouddata, function(x) x = x[, 1])
  energy_cloud <- lapply(clouddata, function(x) x = x[, 2])
  fitness_cloud <- lapply(clouddata, function(x) x = x[, 3])

  efficiency_per_gen <- energy_per_gen <- fitness_per_gen <- list()
  for (i in 1:length(clouddata)) {
    l <- length(clouddata[[i]][, "EfficAllDir"])
    efficiency_per_gen[[i]] <- t(as.matrix(rbind(rep(i, l),
                                                 efficiency_cloud[[i]])))
    energy_per_gen[[i]] <- t(as.matrix(rbind(rep(i, l),
                                             energy_cloud[[i]])))
    fitness_per_gen[[i]] <- t(as.matrix(rbind(rep(i, l),
                                              fitness_cloud[[i]])))
  }
  
  
  efficiency_per_gen <- do.call("rbind", efficiency_per_gen)
  efficiency_per_genmax <- data.frame(efficiency_per_gen)
  max_effic_per_gen <- aggregate(efficiency_per_genmax,
                                 list(efficiency_per_genmax$X1), max)
  mean_effic_per_gen <- aggregate(efficiency_per_genmax,
                                  list(efficiency_per_genmax$X1), mean)
  min_effic_per_gen <- aggregate(efficiency_per_genmax,
                                 list(efficiency_per_genmax$X1), min)
  sd_effic_per_gen <- aggregate(efficiency_per_genmax,
                                list(efficiency_per_genmax$X1), sd)
  efficiency_per_genmax <- cbind(
    "X1" = max_effic_per_gen[, 2],
    "max" = max_effic_per_gen[, 3],
    "mean" = mean_effic_per_gen[, 3],
    "min" = min_effic_per_gen[, 3],
    "sd" = sd_effic_per_gen[, 3])

  energy_per_gen <- do.call("rbind", energy_per_gen)
  energy_per_genmax <- data.frame(energy_per_gen)
  max_energy_per_gen <- aggregate(energy_per_genmax,
                                 list(energy_per_genmax$X1), max)
  mean_energy_per_gen <- aggregate(energy_per_genmax,
                                  list(energy_per_genmax$X1), mean)
  min_energy_per_gen <- aggregate(energy_per_genmax,
                                 list(energy_per_genmax$X1), min)
  sd_energy_per_gen <- aggregate(energy_per_genmax,
                                list(energy_per_genmax$X1), sd)
  energy_per_genmax <- cbind(
    "X1" = max_energy_per_gen[, 2],
    "max" = max_energy_per_gen[, 3],
    "mean" = mean_energy_per_gen[, 3],
    "min" = min_energy_per_gen[, 3],
    "sd" = sd_energy_per_gen[, 3])

  fitness_per_gen <- do.call("rbind", fitness_per_gen)
  fitness_per_genmax <- data.frame(fitness_per_gen)
  max_fit_per_gen <- aggregate(fitness_per_genmax,
                              list(fitness_per_genmax$X1), max)
  mean_fit_per_gen <- aggregate(fitness_per_genmax,
                               list(fitness_per_genmax$X1), mean)
  min_fit_per_gen <- aggregate(fitness_per_genmax,
                              list(fitness_per_genmax$X1), min)
  sd_fit_per_gen <- aggregate(fitness_per_genmax,
                             list(fitness_per_genmax$X1), sd)
  fitness_per_genmax <- cbind(
    "X1" = max_fit_per_gen[, 2],
    "max" = max_fit_per_gen[, 3],
    "mean" = mean_fit_per_gen[, 3],
    "min" = min_fit_per_gen[, 3],
    "sd" = sd_fit_per_gen[, 3])

  ## Plots ##########
  if (pl) {
    par(mfrow = c(2, 3))
    ## Fitness #######
    df <- data.frame(fitness_per_gen)
    colnames(df) <- c("generation", "values")
    boxplot(values~generation, data=df, main = "Fitness",
            xlab = "Generation",
            ylab = "Fitnessvalue", 
            pch = 20, col = "red", cex = 1.3)
    
    if (length(clouddata) >= 4) {
      lf <- stats::smooth.spline(x = fitness_per_gen[, 1],
                                 y = fitness_per_gen[, 2],
                                 spar = 0.1)
      graphics::lines(lf, col = "red", lwd = 1.2)
    }
    graphics::points(x = fitness_per_genmax[, "X1"],
                     y = fitness_per_genmax[, "max"],
                     type = "l", col = "red")
    graphics::points(x = fitness_per_genmax[, "X1"],
                     y = fitness_per_genmax[, "min"],
                     type = "l", col = "red")
    
    ## Efficiency #######
    df <- data.frame(efficiency_per_gen)
    colnames(df) <- c("generation", "values")
    boxplot(values~generation, data=df, main = "Efficiency",
            xlab = "Generation",
            ylab = "Efficiency in %",
            pch = 20, col = "orange", cex = 1.3)
    if (length(clouddata) >= 4) {
      le <- stats::smooth.spline(x = efficiency_per_gen[, 1],
                                 y = efficiency_per_gen[, 2],
                                 spar = 0.1)
      graphics::lines(le, col = "orange", lwd = 1.2)
    }
    graphics::points(x = efficiency_per_genmax[, "X1"],
                     y = efficiency_per_genmax[, "max"],
                     type = "l", col = "orange")
    graphics::points(x = efficiency_per_genmax[, "X1"],
                     y = efficiency_per_genmax[, "min"],
                     type = "l", col = "orange")
    
    ## Energy #######
    df <- data.frame(energy_per_gen)
    colnames(df) <- c("generation", "values")
    boxplot(values~generation, data=df, main = "Energy",
            xlab = "Generation",
            ylab = "Energy in kW",
            pch = 20, col = "blue", cex = 1.3)
    if (length(clouddata) >= 4) {
      len <- stats::smooth.spline(x = energy_per_gen[, 1],
                                  y = energy_per_gen[, 2],
                                  spar = 0.1)
      graphics::lines(len, col = "blue", lwd = 1.2)
    }
    graphics::points(x = energy_per_genmax[, "X1"],
                     y = energy_per_genmax[, "max"],
                     type = "l", col = "blue")
    graphics::points(x = energy_per_genmax[, "X1"],
                     y = energy_per_genmax[, "min"],
                     type = "l", col = "blue")

    graphics::plot(x = fitness_per_genmax[, "X1"],
                   y = fitness_per_genmax[, "sd"],
                   main = "Standard Deviation Fitness",
                   xlab = "Generation",
                   ylab = "Standard Deviation of Population", col = "red",
                   type = "b")
    graphics::plot(x = efficiency_per_genmax[, "X1"],
                   y = efficiency_per_genmax[, "sd"],
                   main = "Standard Deviation Efficiency",
                   xlab = "Generation",
                   ylab = "Standard Deviation of Population", col = "orange",
                   type = "b")
    graphics::plot(x = energy_per_genmax[, "X1"],
                   y = energy_per_genmax[, "sd"],
                   main = "Standard Deviation Energy",
                   xlab = "Generation",
                   ylab = "Standard Deviation of Population", col = "blue",
                   type = "b")
  }

  ## Output ##########
  clouddatafull <- cbind(Fitn = fitness_per_genmax,
                         Eff = efficiency_per_genmax,
                         Ene = energy_per_genmax)
  colnames(clouddatafull) <- c("FitX1", "FitMax", "FitMean", "FitMin", "FitSD",
                               "EffX1", "EffMax", "EffMean", "EffMin", "EffSD",
                               "EneX1", "EneMax", "EneMean", "EneMin", "EneSD")
  invisible(clouddatafull)
}

#' @title Plot the changes of min/mean/max fitness values
#' @name plot_fitness_evolution
#' @description  Plot the evolution of fitness values and the change in the min,
#'   mean and max fitness values to the former generations.
#' @export
#'
#' @inheritParams plot_evolution
#'
#' @family Plotting Functions
#' @return NULL
#' @examples \donttest{
#' ## Plot the results of a hexagonal grid optimization
#' plot_fitness_evolution(resulthex, 0.1)
#' }
plot_fitness_evolution <- function(result, spar = 0.1) {
  ## Setting par() and get Result #######
  oparplotfitness <- par(no.readonly = TRUE)
  on.exit(par(oparplotfitness))
  par(mfrow = c(1, 1), ask = FALSE, mar = c(4, 5, 4, 2))
  layout(mat = matrix(c(1, 2, 3, 4, 4, 4), nrow = 2, ncol = 3, byrow = TRUE))
  
  x <- result[, 4]
  x <- x[-c(1)]
  x1 <- do.call("rbind", x)
  result <- as.data.frame(do.call("rbind", result[, 1]))
  
  ## Minimal Fitness #########
  minge <- x1[seq(1, length(x1[,1]), 2), 1]
  minge2 <- x1[seq(2, length(x1[,2]), 2), 1]
  ming3 <- minge - minge2
  ming3 <- c(0, ming3)
  ming3 <- as.data.frame(ming3)
  ming3$farbe <- 0
  ming3$farbe[ming3$ming3 < 0]  <- "red"
  ming3$farbe[ming3$ming3 > 0] <- "green"
  ming3$farbe[ming3$ming3 == 0] <- "orange"
  plot(ming3$ming3, type = "b", col = ming3$farbe, pch = 20, cex = 2,
       xlab = "Generation", ylab = "Change")
  title(main = "Minimal Fitness Values",sub = "compared to previous generation",
        col.main = "red")
  abline(0, 0)
  grid(col = "black")
  
  ## Mean Fitness #########
  meange <- x1[seq(1, length(x1[, 1]), 2), 3]
  meange2 <- x1[seq(2, length(x1[, 2]), 2), 3]
  meag3 <- meange - meange2
  meag3 <- c(0, meag3)
  meag3 <- as.data.frame(meag3)
  meag3$farbe <- 0
  meag3$farbe[meag3$meag3 < 0]  <- "red"
  meag3$farbe[meag3$meag3 > 0] <- "green"
  meag3$farbe[meag3$meag3 == 0] <- "orange"
  plot(meag3$meag3, type = "b", col = meag3$farbe, pch = 20, cex = 2,
       xlab = "Generation", ylab = "Change")
  title(main = "Mean Fitness Values", sub = "compared to previous generation",
        col.main = "orange")
  abline(0, 0)
  grid(col = "black")
  
  ## Maximum Fitness #########
  maxge <- x1[seq(1, length(x1[, 1]), 2), 2]
  maxge2 <- x1[seq(2, length(x1[, 2]), 2), 2]
  mg3 <- maxge - maxge2
  mg3 <- c(0, mg3)
  mg3 <- as.data.frame(mg3)
  mg3$farbe <- 0
  mg3$farbe[mg3$mg3 < 0]  <- "red"
  mg3$farbe[mg3$mg3 > 0] <- "green"
  mg3$farbe[mg3$mg3 == 0] <- "orange"
  plot(mg3$mg3, type = "b", col = mg3$farbe, pch = 20, cex = 2,
       xlab = "Generation", ylab = "Change")
  title(main = "Maximal Fitness Values", sub = "compared to previous generation",
        col.main = "darkgreen")
  abline(0, 0)
  grid(col = "black")
  
  ## Fitness Values per Generation #########
  plot(result$minparkfitness, xaxt = "n", main = "Parkfitness per Generation",
       ylab = "Parkfitness in %", xlab = "Generation", cex = 2, col = "red",
       pch = 20, ylim = c(min(result$minparkfitness), max(result$maxparkfitness)))
  axis(1, at = 1:nrow(result),tick = TRUE)
  grid(col = "black")
  points(result$meanparkfitness, ylab = "MeanParkF", cex = 2, 
         col = "blue", pch = 20)
  points(result$maxparkfitness, ylab = "maxParkF", cex = 2, 
         col = "green", pch = 20)
  x <- 1:length(result$maxparkfitness)
  
  if (nrow(result) >= 4) {
    lmin <- smooth.spline(x,result$minparkfitness, spar = spar)
    lines(lmin, col = "red", lwd = 1.6)
    lmea <- smooth.spline(x,result$meanparkfitness, spar = spar)
    lines(lmea, col = "blue", lwd = 1.6)
    lmax <- smooth.spline(x,result$maxparkfitness, spar = spar)
    lines(lmax, col = "green", lwd = 1.6)
  }
  
  return()
}


#' @title Plot a heatmap of selected grid cells
#' @name plot_heatmap
#' @description  Plot a heatmap of selected grid cells. Green grid cells
#'   have been selected more often than red grid cells.
#'
#' @export
#'
#' @family Plotting Functions
#' @inheritParams plot_result
#' @param si A numeric value that is used for the sizing of the resolution of
#'   the heatmap. Default is 2
#' @param idistw The inverse distance weighting power. Default is the rotor
#'   radius from the 'result' values
#'
#' @return Invisibly returns a list with the result of the inverse distance
#'   weighting and an aggregated dataframe of all grid cells
#' @examples \donttest{
#' ## Plot the results of a hexagonal grid optimization
#' plot_heatmap(resulthex)
#'
#' ## Plot the heatmap with different settings
#' plot_heatmap(resulthex, si = 4, idistw = 2)
#' }
plot_heatmap <- function(result, si = 2, idistw) {
  if (!requireNamespace("gstat", quietly = TRUE)) {
    stop("The package 'gstat' is required for this function, but it is not installed.\n",
         "Please install it with `install.packages('gstat')`")
  }  
  parheat <- par(ask = FALSE, no.readonly = TRUE)
  on.exit(par(parheat))
  par(mfrow = c(1, 1))
  
  bpe <- do.call("rbind", result[, "allCoords"])
  bpe <- data.frame(bpe[, 1:2])
  row.names(bpe) <- NULL
  
  sizingidw <- as.integer(result[, "inputData"][[1]][, 1]["Rotorradius"])
  sizing <- as.integer(result[, "inputData"][[1]][, 1]["Resolution"]) / si
  
  # dupco <- geoR::dup.coords(bpe, simplify = TRUE)
  dupco <- dup_coords(bpe, simplify = TRUE)
    
  bpe$Ids <- seq.int(nrow(bpe))
  dupco <- lapply(dupco, function(x) as.integer(x))
  dupcosum <- lapply(dupco, function(x) length(x))
  bpenew <- vector("list", length(dupco))
  for (i in 1:length(dupco)) {
    bpenew[[i]] <- bpe[bpe$Ids == dupco[[i]][1], ]
    bpenew[[i]]$Sum <- dupcosum[[i]][1]
  }
  bpenew <- do.call("rbind", bpenew)
  bpenew <- bpenew[-3]
  
  polo <- st_as_sf(bpenew, coords=c("X","Y"))
  
  extra_margin <- 50
  x_range <-   range(bpenew$X)
  y_range <-   range(bpenew$Y)
  grd <- expand.grid(X = seq(from = x_range[1] - extra_margin,
                             to = x_range[2] + extra_margin, by = sizing),
                     Y = seq(from = y_range[1] - extra_margin,
                             to = y_range[2] + extra_margin, by = sizing))
  
  ## convert grid to SpatialPixel class
  grd <- st_as_sf(grd, coords=c("X","Y"))
  grd <- st_make_grid(grd, offset = st_bbox(grd)[c("xmin", "ymin")],
                      cellsize = st_distance(grd[1,], grd[2,])[[1]]) 
  
  if (missing(idistw)) {
    idistw <- sizingidw
  } else {
    idistw <- idistw
  }
  
  ## Calculate IDW
  idwout <- data.frame(gstat::idw(formula = bpenew$Sum ~ 1,
                                  locations = polo, newdata = grd,
                                  idp = idistw))
  
  ## Plot heatmap
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("The package 'ggplot2' is required to plot the result, but it is not installed.\n",
            "Please install it with `install.packages('ggplot2')`")
  } else {
    var1.pred=X=Y=x=y=NULL
    
    plot1 <- ggplot2::ggplot(data = idwout,
                             mapping = ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_tile(data = idwout, ggplot2::aes(fill = var1.pred),
                         show.legend = TRUE) +
      ggplot2::labs(
        title = "Inverse Distance Weighting for Grid Cell Selection") +
      ggplot2::geom_point(data = bpenew, mapping = ggplot2::aes(x = X, y = Y),
                          show.legend = TRUE, size = 5*bpenew$Sum/max(bpenew$Sum),
                          alpha = 0.6) +
      ggplot2::scale_fill_gradient(low = "red", high = "green") +
      ggplot2::coord_equal()
    
    print(plot1)
  }
  
  invisible(list("idw" = idwout,
                 "GA_grids" = bpenew))
}


#' @title Splits duplicated coords (copy of geoR::dup.coords)
#' @name dup_coords
#' @description This function takes an object with 2-D coordinates and returns
#' the positions of the duplicated coordinates. Also sets a method for
#' duplicated. Helper function for \code{\link{plot_heatmap}}
#'
#' @export
#'
#' @param x Two column numeric matrix or data frame
#' @param ... passed to sapply. If simplify = TRUE (default) results are
#' returned as an array if possible (when the number of replicates are the
#' same at each replicated location)
#' 
#' @family Helper Functions
#' @return Function and methods returns NULL if there are no duplicates 
#' locations. Otherwise, the default method returns a list where each component
#' is a vector with the positions or the rownames, if available, of the
#' duplicates coordinates. The method for geodata returns a data-frame with
#' rownames equals to the positions of the duplicated coordinates, the first
#' column is a factor indicating duplicates and the remaining are output of
#' as.data.frame.geodata.
#' 
#' @author Paulo Justiniano Ribeiro Jr. \email{paulojus@@leg.ufpr.br}
#' Peter J. Diggle \email{p.diggle@@lancaster.ac.uk}
dup_coords <- function(x, ...) {
  ap1 <- unclass(factor(paste("x", x[, 1], "y", x[, 2], sep = "")))
  ap2 <- table(ap1)
  ap2 <- ap2[ap2 > 1]
  takecoords <- function(n){
    if (!is.null(rownames(x))) rownames(x[ap1 == n, ])
  }
  res <- sapply(as.numeric(names(ap2)), takecoords, ...)
  if (!is.null(res)) class(res) <- "duplicated.coords"
  return(res)
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
#' @return NULL
#' 
#' @examples \donttest{
#' library(sf)
#' Polygon1 <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)))), 
#'   crs = 3035
#' ))
#'
#' Res = random_search(result = resultrect, Polygon1 = Polygon1)
#' plot_random_search(resultRS = Res, result = resultrect, Polygon1 = Polygon1, best=2)
#' }
plot_random_search <- function(resultRS, result, Polygon1, best) {
  
  op <- par(no.readonly = TRUE) 
  par(mfrow = c(1, 2))
  
  result_inputs <- result[1,'inputData'][[1]]
  resultRS1 <- do.call("rbind", cbind(resultRS))
  a <- resultRS1[,"EnergyOverall"]
  order1 <- order(a, decreasing = TRUE)
  resultRS1 <- resultRS1[order1, ]
  
  if (missing(best)) best <- 1
  
  # resBest <- resultRS1[!duplicated(resultRS1$Run) & !duplicated(resultRS1$bestGARun),]
  resBest <- resultRS1[!duplicated(resultRS1[, "Run"]),,drop = FALSE]
  if (nrow(resBest) < best) {best <- nrow(resBest)}
  
  resBest <- resBest[1:best,,drop = FALSE]
  # resBest <- resBest[nrow(resBest):(nrow(resBest)-best),]
  
  resultRS2 <- list()
  for (nr in 1:nrow(resBest)) {
    resultRS2[[nr]] <- resultRS1[resultRS1[, "Run"] == resBest[, "Run"][nr] & 
                                   resultRS1[, "bestGARun"] == resBest[, "bestGARun"][nr],]
  }
  resultRS1 <- resultRS2
  
  resultRS1 <- rev(resultRS1)
  resBest <- resBest[order(resBest[,"EnergyOverall"]),,drop=FALSE]
  
  Polygon1 <- isSpatial(Polygon1)
  cellsize <- as.numeric(result_inputs["Resolution",][[1]])
  if (toupper(result_inputs["Grid Method",][[1]]) == "RECTANGULAR") {
    Grid <- grid_area(Polygon1, size = cellsize,
                      prop = as.numeric(result_inputs["Percentage of Polygon",][[1]]))[[2]]
  } else {
    Grid <- hexa_area(Polygon1, size = cellsize)[[2]]
  }
  rbPal1 <- grDevices::colorRampPalette(c("green","red"))
  col2res <- "lightblue"
  
  for (i in 1:length(resultRS1) ) {
    ## Original GA-result ################
    bestGAR <- resBest[,"bestGARun",drop=F][i]
    bestrestGA <- result[bestGAR,]$bestPaEn
    brOrig <- length(levels(factor(bestrestGA[,"AbschGesamt",drop=F])))
    if (brOrig > 1) {
      ColOri <- rbPal1(brOrig)[as.numeric(cut(as.numeric(bestrestGA[,"AbschGesamt",drop=F]),
                                              breaks = brOrig))]
    } else {
      ColOri <- "green"
    }
    
    bestrestGA[,"EnergyOverall"] <- round(bestrestGA[,"EnergyOverall"], 2)
    bestrestGA[,"EfficAllDir"] <- round(bestrestGA[,"EfficAllDir"], 2)
    plot(Polygon1, col = col2res, 
                 main = paste("Original - Best Energy:", (best + 1) - i, "\n","Energy Output",
                              bestrestGA[,"EnergyOverall"][[1]],"kW", "\n", "Efficiency:",
                              bestrestGA[,"EfficAllDir"][[1]]))
    plot(Grid, add = TRUE)
    graphics::mtext("Total Wake Effect in %", side = 2)
    graphics::points(bestrestGA[,"X"],bestrestGA[,"Y"],
                     cex = 2, pch = 20, col = ColOri)
    graphics::text(bestrestGA[,"X"], bestrestGA[,"Y"],
                   round(bestrestGA[,"AbschGesamt"],0),
                   cex = 0.8, pos = 1, col = "black")
    distpo <- stats::dist(x = cbind(bestrestGA[,"X"], bestrestGA[,"Y"]),
                          method = "euclidian")
    graphics::mtext(paste("minimal Distance", round(min(distpo), 2)),
                    side = 1, line = 0)
    graphics::mtext(paste("mean Distance", round(mean(distpo), 2)),
                    side = 1, line = 1)
    ################
    
    ## Random Search Output  ################
    EnergyBest <- data.frame(resultRS1[[i]])
    ## Assign the colour depending on the individual wind speed 
    br <- length(levels(factor(EnergyBest[,'AbschGesamt'])))
    if (br > 1) {
      Col <- rbPal1(br)[as.numeric(cut(as.numeric(
        EnergyBest[, 'AbschGesamt']), breaks = br))]
    } else {
      Col <- "green"
    }
    
    EnergyBest[,'EnergyOverall'] <- round(EnergyBest[,'EnergyOverall'], 2)
    EnergyBest[,'EfficAllDir'] <- round(EnergyBest[,'EfficAllDir'], 2)
    plot(Polygon1, col = col2res,
                 main = paste("Random Search - Best Energy:", (best + 1) - i, 
                              "\n","Energy Output",
                              EnergyBest[,'EnergyOverall'][[1]],"kW", "\n", "Efficiency:",
                              EnergyBest[,'EfficAllDir'][[1]]))
    
    plot(Grid, add = TRUE)
    graphics::mtext("Total Wake Effect in %", side = 2)
    graphics::points(EnergyBest[,'X'],EnergyBest[,'Y'],
                     cex = 2, pch = 20, col = Col)
    graphics::text(EnergyBest[,'X'], EnergyBest[,'Y'],
                   round(EnergyBest[,'AbschGesamt'], 0),
                   cex = 0.8, pos = 1, col = "black")
    distpo <- stats::dist(x = cbind(EnergyBest[,'X'],EnergyBest[,'Y']),
                          method = "euclidian")
    graphics::mtext(paste("minimal Distance", round(min(distpo), 2)),
                    side = 1, line = 0)
    graphics::mtext(paste("mean Distance", round(mean(distpo), 2)),
                    side = 1, line = 1)
    ################
  }
  
  par(op)
  invisible(NULL)
}


