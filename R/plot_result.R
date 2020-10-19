#' @title Plot the best results
#' @name plot_result
#' @description  Plot the best solutions of the genetic algorithm.
#'   Depending on \code{plotEn}, either the best energy or efficiency solutions
#'   can be plotted. \code{best} indicates the amount of best solutions to plot.
#'
#' @export
#'
#' @param result The output of \code{\link{windfarmGA}} or
#'   \code{\link{genetic_algorithm}}
#' @param Polygon1 The considered area as shapefile
#' @param best A numeric value indicating how many of the best individuals
#'   should be plotted
#' @param plotEn A numeric value that indicates if the best energy or efficiency
#'   output should be plotted. If (plotEn==1) plots the best energy solutions
#'   and (plotEn==2) plots the best efficiency solutions
#' @param topographie A logical value, indicating whether terrain effects should
#'   be considered and plotted or not
#' @param Grid The grid as SpatialPolygons, which is obtained from
#'   \code{\link{grid_area}} and used for plotting
#' @param Projection A desired Projection can be used instead of the default
#'   Lambert Azimuthal Equal Area Projection
#' @param sourceCCL The source to the Corine Land Cover raster (.tif). Only
#'   required, when the terrain effect model is activated
#' @param sourceCCLRoughness The source to the adapted Corine Land Cover legend
#'   as .csv file. Only required when terrain effect model is activated. As
#'   default a .csv file within this package (\file{~/extdata/clc_legend.csv})
#'   is taken that was already adapted manually
#' @param weibullsrc A list of Weibull parameter rasters, where the first list
#'   item must be the shape parameter raster k and the second item must be the
#'   scale parameter raster a of the Weibull distribution. If no list is given,
#'   then rasters included in the package are used instead, which currently only
#'   cover Austria.
#'
#' @family Plotting Functions
#' @return Returns a data.frame of the best (energy/efficiency) individual
#'   during all iterations
#'
#' @examples \donttest{
#' ## Add some data examples from the package
#' load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
#'
#' ## Plot the results of a hexagonal grid optimization
#' Grid <- hexa_area(polygon, size = 75, FALSE)
#' plot_result(resulthex, polygon, best = 1, plotEn = 1, topographie = FALSE,
#'            Grid = Grid[[2]])
#'
#' ## Plot the results of a rectangular grid optimization
#' Grid <- grid_area(polygon, resol = 150, 1, FALSE)
#' plot_result(resultrect, polygon, best = 1, plotEn = 1, topographie = FALSE,
#'            Grid = Grid[[2]])
#'}
plot_result <- function(result, Polygon1, best = 3, plotEn = 1,
                       topographie = FALSE, Grid, Projection,
                       sourceCCLRoughness, sourceCCL,
                       weibullsrc){
  parpplotRes <- par(no.readonly = TRUE)
  par(mfrow = c(1, 1))
  par(mar = c(5, 6, 4, 2) + 0.1, mgp = c(5, 1, 0))


  ## Check Projections and reference systems
  Polygon1 <- windfarmGA::isSpatial(Polygon1)

  PROJ6 <- utils::compareVersion(sf::sf_extSoftVersion()[[3]], "6") > 0
  if (missing(Projection)) {
    if (PROJ6) {
      ProjLAEA <- "+init=epsg:3035"
    } else {
      ProjLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
      +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
    }
  } else {
    ProjLAEA <- Projection
  }
  
  if (PROJ6) {
    if (is.na(slot(Polygon1, "proj4string"))) {
      ProjLAEA <- result[1,'inputData'][[1]]['Projection',][[1]]
      slot(Polygon1, "proj4string") <- CRS(ProjLAEA)
      # stop("Polygon is not projected.", call. = FALSE )
      message("Polygon is not projected. Same projection from result will be assumed.")
    }
    if (suppressWarnings(!isTRUE( all.equal(wkt(Polygon1), wkt(CRS(ProjLAEA))) ))) {
      Polygon1 <- sp::spTransform(Polygon1, CRSobj = ProjLAEA)
    }
  } else {
    if (is.na(sp::proj4string(Polygon1))) {
      ProjLAEA <- result[1,'inputData'][[1]]['Projection',][[1]]
      sp::proj4string(Polygon1) <- ProjLAEA
      # stop("Polygon is not projected.", call. = FALSE )
      message("Polygon is not projected. Same projection from result will be assumed.")
    }
    if (as.character(raster::crs(Polygon1)) != ProjLAEA) {
      Polygon1 <- sp::spTransform(Polygon1, CRSobj = ProjLAEA)
    }
  }
  if (missing(sourceCCL)) {
    sourceCCL <- NULL
  }
  if (missing(sourceCCLRoughness)) {
    sourceCCLRoughness <- NULL
  }
  

  ## Check Weibull Rasters
  if (missing(weibullsrc)) {
    weibullsrc <- NULL
    col2res <- "lightblue"
  } else {
    PolyCrop <- sp::spTransform(Polygon1,
                                CRSobj = sp::proj4string(weibullsrc[[1]]))
    if (class(weibullsrc) == "list" & length(weibullsrc) == 2) {
      wblcroped <- lapply(weibullsrc, function(x){
        raster::crop(x, raster::extent(PolyCrop))})
      wblcroped <- lapply(wblcroped, function(x){
        raster::mask(x, PolyCrop)})
      Erwartungswert <- wblcroped[[2]] * (gamma(1 + (1 / wblcroped[[1]])))
    } else if (class(weibullsrc) == "list" & length(weibullsrc) == 1) {
      wblcroped <- raster::crop(weibullsrc[[1]], raster::extent(PolyCrop))
      wblcroped <- raster::mask(weibullsrc[[1]], PolyCrop)
      Erwartungswert <- wblcroped[[1]]
    } else if (class(weibullsrc) == "RasterLayer") {
      wblcroped <- raster::crop(weibullsrc, raster::extent(PolyCrop))
      wblcroped <- raster::mask(weibullsrc, PolyCrop)
      Erwartungswert <- wblcroped
    }
    col2res <- "transparent"
    alpha <- 0.9
    Erwartungswert <- raster::projectRaster(Erwartungswert, 
                                            crs = CRS(ProjLAEA))
    # plot(Erwartungswert)
  }


  ## Creat a color ramp
  rbPal1 <- grDevices::colorRampPalette(c('green','red'))

  resultSafe <- result

  if (!plotEn %in% c(1,2)) {
    stop("plotEn must be either 1 or 2. \n",
         "1 - plots the best energy output. \n",
         "2 - plots the best efficiency output.")
  }
  ## Plot Best Energy
  if (plotEn == 1) {
    a <- sapply(result[, 2], function(i) subset.matrix(i, 
                                                       select = "EnergyOverall"))
    b <- a[1, ]
    order1 <- order(b, decreasing = FALSE)
    result <- result[, 2][order1]
    ledup <- length(result)

    rectid <- lapply(result, function(x) x[, 'Rect_ID'])
    rectidt <- !duplicated(rectid)
    result <- result[rectidt]
    ndif <- length(result)

    cat(paste("N different optimal configurations:", ndif, "\nAmount duplicates:", 
              (ledup - ndif)))
    
    if (ndif < best) {
      cat(paste("\nNot enough unique Optimas. Show first best Half of different configurations."))
      best <- trunc(ndif / 2)
    }
    if (best == 0) best = 1
    result <- result[(length(result) - best + 1):(length(result))]

    for (i in (1:length(result))){
      EnergyBest <- data.frame(result[[i]])
      ## Assign the colour depending on the individual wind speed
      br <- length(levels(factor(EnergyBest[, 'AbschGesamt'])))
      if (br > 1) {
        Col <- rbPal1(br)[as.numeric(cut(as.numeric(
          EnergyBest[, 'AbschGesamt']), breaks = br))]
      } else {
        Col <- "green"
      }

      EnergyBest$EnergyOverall <- round(EnergyBest[, 'EnergyOverall'], 2)
      EnergyBest$EfficAllDir <- round(EnergyBest[, 'EfficAllDir'], 2)

      cat(paste("\nPlot ", (best + 1) - i, " Best Energy Solution:\n"))
      par(mfrow = c(1, 1), ask = FALSE)
      plot(Polygon1, col = col2res, 
           main = paste((best + 1) - i, "Best Energy Windfarm", "\n","Energy Output",
                        EnergyBest$EnergyOverall[[1]], "kW", "\n", "Efficiency:",
                        EnergyBest$EfficAllDir[[1]], "%"), cex.main = 0.8)

      if (best > 1){
        if (i > 1){
          par(ask = TRUE)
        }
      }

      if (!is.null(weibullsrc)) {
        raster::plot(Erwartungswert, alpha = alpha, legend = TRUE, axes = FALSE,
                     useRaster = TRUE, add = TRUE,
                     legend.lab = "Mean Wind Speed")
      }
      if (!missing(Grid)) {
        plot(Grid, add = TRUE)
      }

      graphics::mtext("Total Wake Effect in %", side = 2, cex = 0.8)
      graphics::points(EnergyBest[, 'X'], EnergyBest[, 'Y'],
                       cex = 2, pch = 20, col = Col)
      graphics::text(EnergyBest[, 'X'], EnergyBest[, 'Y'],
                     round(EnergyBest[, 'AbschGesamt'], 0),
                     cex = 0.8, pos = 1, col = "black")

      distpo <- stats::dist(x = cbind(EnergyBest[, 'X'], EnergyBest[, 'Y']),
                            method = "euclidian")
      graphics::mtext(paste("minimal Distance", round(min(distpo), 2)),
                      side = 1, line = 0, cex = 0.8)
      graphics::mtext(paste("mean Distance", round(mean(distpo), 2)),
                      side = 1, line = 1, cex = 0.8)
      
      if (topographie == TRUE && plotEn == 1){
        par(ask = TRUE)
        resol <- as.integer(resultSafe[1, ]$inputData['Resolution',])
        polygon1 <- Polygon1
        sel1 <- EnergyBest[, 1:2]
        windpo <- 1

        if (is.null(sourceCCL)) {
          if (length(list.files(pattern = "g100_06.tif")) == 0) {
            # warning("\nNo raster given or found in directory for the land coverage.",
                 # "\nInternal CLC-raster is used.'\n")
            ## ccl is loaded from /data
            message("\nNo land cover raster ('sourceCCL') was given. It will be downloaded from ",
                    "the EEA-website.")
            ## download an zip CCL-tif
            # ccl_raster_url <-
            #   "https://www.eea.europa.eu/data-and-maps/data/clc-2006-raster-3/clc-2006-100m/g100_06.zip/at_download/file"
            # temp <- tempfile()
            # download.file(ccl_raster_url, temp, method = "libcurl", mode = "wb")
            # unzip(temp, "g100_06.tif")
            # unlink(temp)
            download.file("http://github.com/YsoSirius/windfarm_data/raw/master/clc.zip", 
                          destfile = "clc.zip", 
                          method = "auto")
            unzip("clc.zip")
            unlink("clc.zip")
            ccl <- raster::raster("g100_06.tif")
          } else {
            sourceCCL <- list.files(pattern = "g100_06.tif", full.names = TRUE)
            ccl <- raster::raster(x = sourceCCL)
          }
        }

        if (1 == 1) {
          Polygon1 <-  sp::spTransform(
            Polygon1, 
            CRSobj = raster::crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
          extpol <- round(Polygon1@bbox, 0)[, 2]
          srtm <- raster::getData('SRTM', lon = extpol[1], lat = extpol[2])
          srtm_crop <- raster::crop(srtm, Polygon1)
          srtm_crop <- raster::mask(srtm_crop, Polygon1)

          Polygon1 <-  sp::spTransform(Polygon1, CRSobj = raster::crs(ProjLAEA))
          srtm_crop <- raster::projectRaster(srtm_crop, crs = raster::crs(ProjLAEA))

          # Include Corine Land Cover Raster to get an estimation of Surface Roughness
          cclPoly <- raster::crop(ccl, Polygon1)
          cclPoly1 <- raster::mask(cclPoly, Polygon1)

          if (is.null(sourceCCLRoughness)) {
            path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
            source_ccl <- paste0(path, "clc_legend.csv")
          } else {
            cat("\nYou are using your own Corine Land Cover legend.")
            # readline(prompt = "\nPress <ENTER> if you want to continue")
            source_ccl <- sourceCCLRoughness
          }
          rauhigkeitz <- utils::read.csv(source_ccl, header = TRUE, sep = ";")

          cclRaster <- raster::reclassify(cclPoly1,
                                          matrix(c(rauhigkeitz[,'GRID_CODE'],
                                                   rauhigkeitz[,'Rauhigkeit_z']),
                                                 ncol = 2))

          # Calculates Wind multiplier. Hills will get higher values, valleys will get lower values.
          orogr1 <- raster::calc(srtm_crop, function(x) {
            x / (raster::cellStats(srtm_crop, mean, na.rm = TRUE))
          })
          orogrnum <- raster::extract(x = orogr1, y = as.matrix((sel1)), #buffer = resol * 2, 
                                      small = TRUE, fun = mean, na.rm = TRUE)
          windpo <- windpo * orogrnum
          ## Get Elevation of Turbine Locations to estimate the air density at the resulting height
          heightWind <- raster::extract(x = srtm_crop, y = as.matrix((sel1)), 
                                        small = TRUE, fun = max, na.rm = TRUE)

          par(mfrow = c(1, 2))
          plot(srtm_crop, main = "SRTM Elevation Data")
          graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                            labs = round(heightWind, 0), cex = 0.8)
          plot(polygon1, add = TRUE)
          plot(orogr1, main = "Wind Speed Multipliers")
          points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                            labs = round(windpo, 3), cex = 0.8)
          plot(polygon1, add = TRUE)

          # Get Air Density and Pressure from Height Values
          HeighttoBaro <- matrix(heightWind); colnames(HeighttoBaro) <- "HeighttoBaro"
          air_dt <- barometric_height(matrix(HeighttoBaro), HeighttoBaro)

          plot(srtm_crop, main = "Normal Air Density",
               col = topo.colors(10))
          points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                            labs = rep(1.225, nrow(sel1)), cex = 0.8)
          plot(polygon1, add = TRUE)
          plot(srtm_crop, main = "Corrected Air Density",
               col = topo.colors(10))
          points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                            labs = round(air_dt[,'rh'], 2), cex = 0.8)
          plot(polygon1, add = TRUE)

          #CorineLandCover Roughness values
          SurfaceRoughness0 <- raster::extract(x = cclRaster, y = as.matrix((sel1)),
                                               # buffer = resol * 2,
                                               small = TRUE, fun = mean, na.rm = TRUE)
          SurfaceRoughness1 <- raster::extract(x = raster::terrain(srtm_crop, "roughness"),
                                               y = as.matrix((sel1)),
                                               # buffer = resol * 2,
                                               small = TRUE, fun = mean, na.rm = TRUE)
          SurfaceRoughness <- SurfaceRoughness0 * (1 + (SurfaceRoughness1 / max(raster::res(srtm_crop))))
          elrouind <- raster::terrain(srtm_crop, "roughness")
          elrouindn <- raster::resample(elrouind, cclRaster, method = "ngb")
          modSurf <- raster::overlay(x = cclRaster, y = elrouindn,
                                     fun = function(x,y){
                                       return(x * (1 + (y / max(raster::res(srtm_crop)))))
                                      })

          graphics::par(mfrow = c(1, 2))
          cexa <- 0.9
          raster::plot(cclRaster, main = "Corine Land Cover Roughness")
          points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                            labs = round(SurfaceRoughness0, 2), cex = cexa)
          plot(polygon1, add = TRUE)
          raster::plot(x = raster::terrain(srtm_crop, "roughness", neighbors = 4),
                       main = "Elevation Roughness Indicator")
          graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                            labs = round((SurfaceRoughness1), 2), cex = cexa)
          raster::plot(polygon1, add = TRUE)
          raster::plot(modSurf, main = "Modified Surface Roughness")
          graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                            labs = round((SurfaceRoughness), 2), cex = cexa)
          raster::plot(polygon1, add = TRUE)

          # RotorHeight <- as.integer(resultSafe[1, 'inputData']$inputData['Rotor Height',][[1]])
          RotorHeight <- as.integer(resultSafe[1, 'inputData'][[1]]['Rotor Height',])
          k_raster <- raster::calc(modSurf, function(x) {x <- 0.5 / (log(RotorHeight / x))})
          # New Wake Decay Constant calculated with new surface roughness values, according to CLC
          k <- 0.5 / (log(RotorHeight / SurfaceRoughness))
          # graphics::par(mfrow=c(1,1)); cexa=0.9
          raster::plot(k_raster, main = "Adapted Wake Decay Constant - K")
          graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'], labs = round(k, 3), cex = cexa)
          raster::plot(polygon1, add = TRUE)
        }
      }
    }
    ResPlotResult <- EnergyBest
  }
  ## Plot Best Efficiency
  if (plotEn == 2){
    a <- sapply(result[,2], function(i) subset.matrix(i, select = "EfficAllDir"))
    b <- a[1, ]
    order2 <- order(b, decreasing = FALSE)
    result <- result[, 3][order2]
    ledup <- length(result)
    rectid <- lapply(result, function(x) x[,'Rect_ID'])
    rectidt <- !duplicated(rectid)
    result <- result[rectidt]
    ndif <- length(result)

    cat(paste("N different optimal configurations:", ndif, "\nAmount duplicates:", (ledup - ndif)))
    if (ndif < best) {
      cat(paste("\nNot enough unique Optimas. Show first best Half of different configurations."))
      best <- trunc(ndif / 2)
    }
    if (best == 0) best = 1
    result <- result[(length(result) - best + 1):(length(result))]

    for (i in (1:length(result))) {
      EfficiencyBest <- data.frame(result[[i]])
      ## Assign the colour depending on the individual wind speed (from windraster and influence)
      br <- length(levels(factor(EfficiencyBest[,'AbschGesamt'])))
      if (br > 1) {
        Col1 <- rbPal1(br)[as.numeric(cut(EfficiencyBest[,'AbschGesamt'], breaks = br))]
      } else {
        Col1 <- "green"
      }

      EfficiencyBest[,'EnergyOverall'] <- round(EfficiencyBest[,'EnergyOverall'], 2)
      EfficiencyBest[,'EfficAllDir'] <- round(EfficiencyBest[,'EfficAllDir'], 2)

      cat(paste("\nPlot ", (best + 1) - i, " Best Efficiency Solution:\n"))

      par(mfrow = c(1, 1), ask = FALSE)
      raster::plot(Polygon1, col = col2res, 
                   main = paste((best + 1) - i, "Best Efficiency Windfarm", "\n","Energy Output",
                                EfficiencyBest[,'EnergyOverall'][[1]],"kW", "\n", "Efficiency:",
                                EfficiencyBest[,'EfficAllDir'][[1]], "%"), cex.main = 0.8)
      if (best > 1){
        if (i > 1){
          par(ask = TRUE)
        }
      }
      if (!is.null(weibullsrc)) {
        raster::plot(Erwartungswert, alpha = alpha, legend = TRUE, axes = FALSE,
                     useRaster = TRUE, add = TRUE, legend.lab = "Mean Wind Speed")
      }
      if (!missing(Grid)) {
        plot(Grid, add = TRUE)
      }

      graphics::mtext("Total Wake Effect in %", side = 2, cex = 0.8)
      graphics::points(EfficiencyBest[, 'X'], EfficiencyBest[, 'Y'], col = Col1, cex = 2, pch = 20)
      graphics::text(EfficiencyBest[, 'X'], EfficiencyBest[, 'Y'], round(EfficiencyBest$AbschGesamt, 0),
                     cex = 0.8, pos = 1)

      distpo <- stats::dist(x = cbind(EfficiencyBest[, 'X'], EfficiencyBest[, 'Y']), method = "euclidian")
      graphics::mtext(paste("minimal Distance", round(min(distpo), 2)), side = 1, line = 0, cex = 0.8)
      graphics::mtext(paste("mean Distance", round(mean(distpo), 2)), side = 1, line = 1, cex = 0.8)

      
      if (topographie == TRUE && plotEn == 2){
        par(ask = TRUE)
        resol <- as.integer(resultSafe[1,]$inputData['Resolution',])
        polygon1 <- Polygon1
        sel1 <- EfficiencyBest[,1:2]
        windpo <- 1

        if (is.null(sourceCCL)){
          if (length(list.files(pattern = "g100_06.tif")) == 0) {
            message("\nNo land cover raster ('sourceCCL') was given. It will be downloaded from ",
                    "the EEA-website.")
            ## download an zip CCL-tif
            # ccl_raster_url <-
            #   "https://www.eea.europa.eu/data-and-maps/data/clc-2006-raster-3/clc-2006-100m/g100_06.zip/at_download/file"
            # temp <- tempfile()
            # download.file(ccl_raster_url, temp, method = "libcurl", mode = "wb")
            # unzip(temp, "g100_06.tif")
            # unlink(temp)
            download.file("http://github.com/YsoSirius/windfarm_data/raw/master/clc.zip", 
                          destfile = "clc.zip", 
                          method = "auto")
            unzip("clc.zip")
            unlink("clc.zip")
            ccl <- raster::raster("g100_06.tif")
          } else {
            sourceCCL <- list.files(pattern = "g100_06.tif", full.names = TRUE)
            ccl <- raster::raster(x = sourceCCL)
          }
        }

        if (1 == 1) {
          Polygon1 <-  sp::spTransform(Polygon1, CRSobj = raster::crs("+proj=longlat +datum=WGS84 +ellps=WGS84
                                                                      +towgs84=0,0,0"))
          extpol <- round(Polygon1@bbox, 0)[, 2]
          srtm <- raster::getData('SRTM', lon = extpol[1], lat = extpol[2])
          srtm_crop <- raster::crop(srtm, Polygon1)
          srtm_crop <- raster::mask(srtm_crop, Polygon1)

          Polygon1 <-  sp::spTransform(Polygon1, CRSobj = raster::crs(ProjLAEA))
          srtm_crop <- raster::projectRaster(srtm_crop, crs = raster::crs(ProjLAEA))

          # Include Corine Land Cover Raster to get an estimation of Surface Roughness
          cclPoly <- raster::crop(ccl, Polygon1)
          cclPoly1 <- raster::mask(cclPoly, Polygon1)
          if (is.null(sourceCCLRoughness)) {
            path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
            source_ccl <- paste0(path, "clc_legend.csv")
          } else {
            cat("\nYou are using your own Corine Land Cover legend.")
            source_ccl <- sourceCCLRoughness
            # readline(prompt = "\nPress <ENTER> if you want to continue")
          }
          rauhigkeitz <- utils::read.csv(source_ccl, header = TRUE, sep = ";")
          cclRaster <- raster::reclassify(cclPoly1,
                                          matrix(c(rauhigkeitz[,'GRID_CODE'],
                                                   rauhigkeitz[,'Rauhigkeit_z']),
                                                 ncol = 2))

          # Calculates Wind multiplier. Hills will get higher values, valleys will get lower values.
          orogr1 <- raster::calc(srtm_crop, function(x) {
            x / (raster::cellStats(srtm_crop, mean, na.rm = TRUE))
          })
          orogrnum <- raster::extract(x = orogr1, y = as.matrix((sel1)), 
                                      # buffer = resol * 2, 
                                      small = TRUE, fun = mean, na.rm = TRUE)
          windpo <- windpo * orogrnum
          ## Get Elevation of Turbine Locations to estimate the air density at the resulting height
          heightWind <- raster::extract(x = srtm_crop, y = as.matrix((sel1)), 
                                        small = TRUE, fun = max, na.rm = TRUE)

          graphics::par(mfrow = c(1, 2))
          cexa <- 0.9
          raster::plot(srtm_crop, main = "SRTM Elevation Data")
          graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                            labs = round(heightWind, 0), cex = 0.8)
          raster::plot(polygon1, add = TRUE)
          raster::plot(orogr1, main = "Wind Speed Multipliers")
          graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'],
                            labs = round(windpo, 3), cex = 0.8)
          raster::plot(polygon1, add = TRUE)

          # Get Air Density and Pressure from Height Values
          HeighttoBaro <- matrix(heightWind); colnames(HeighttoBaro) <- "HeighttoBaro"
          air_dt <- barometric_height(matrix(HeighttoBaro), HeighttoBaro)
          raster::plot(srtm_crop, main = "Normal Air Density", col = topo.colors(10))
          graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'], labs = rep(1.225, nrow(sel1)), cex = 0.8)
          raster::plot(polygon1, add = TRUE)
          raster::plot(srtm_crop, main = "Corrected Air Density", col = topo.colors(10))
          graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'], 
                            labs = round(air_dt[, 'rh'], 2), cex = 0.8)
          raster::plot(polygon1, add = TRUE)

          #CorineLandCover Roughness values
          SurfaceRoughness0 <- raster::extract(x = cclRaster, y = as.matrix((sel1)),
                                               # buffer = resol * 2,
                                               small = TRUE, fun = mean, na.rm = TRUE)
          SurfaceRoughness1 <- raster::extract(x = raster::terrain(srtm_crop, "roughness"),
                                               y = as.matrix((sel1)),
                                               # buffer = resol * 2,
                                               small = TRUE, fun = mean, na.rm = TRUE)
          SurfaceRoughness <-SurfaceRoughness0 * (1 + (SurfaceRoughness1 / max(raster::res(srtm_crop))))
          elrouind <- raster::terrain(srtm_crop, "roughness")
          elrouindn <- raster::resample(elrouind, cclRaster, method = "ngb")
          modSurf <- raster::overlay(x = cclRaster, y = elrouindn, fun = function(x, y) {
            return(x * (1 + (y / max(raster::res(srtm_crop)))))
          })

          raster::plot(cclRaster, main = "Corine Land Cover Roughness")
          graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'], labs = round(SurfaceRoughness0, 2),
                            cex = cexa)
          plot(polygon1, add = TRUE)
          raster::plot(x = raster::terrain(srtm_crop, "roughness", neighbors = 4), 
                       main = "Elevation Roughness Indicator")
          graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'], labs = round((SurfaceRoughness1), 2),
                            cex = cexa)
          plot(polygon1, add = TRUE)
          raster::plot(modSurf, main = "Modified Surface Roughness")
          graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'], 
                            labs = round((SurfaceRoughness), 2), cex = cexa)
          plot(polygon1, add = TRUE)

          # browser()
          # RotorHeight <- as.integer(resultSafe[1, 'inputData']$inputData['Rotor Height',][[1]])
          RotorHeight <- as.integer(resultSafe[1, 'inputData'][[1]]['Rotor Height',])
          k_raster <- raster::calc(modSurf, function(x) {x <- 0.5 / (log(RotorHeight / x))})
          # New Wake Decay Constant calculated with new surface roughness values, according to CLC
          k <- 0.5 / (log(RotorHeight / SurfaceRoughness))
          raster::plot(k_raster, main = "Adapted Wake Decay Constant - K")
          graphics::points(sel1[, 'X'], sel1[, 'Y'], pch = 20)
          calibrate::textxy(sel1[, 'X'], sel1[, 'Y'], labs = round(k, 3), cex = cexa)
          raster::plot(polygon1, add = TRUE)
        }
      }
    }
    ResPlotResult <- EfficiencyBest
  }
  ############################
  # if(topographie==TRUE && plotEn == 2){
  # 
  #   par(ask=F)
  #   
  #   resol <- as.integer(resultSafe[1,]$inputData['Resolution',])
  #   polygon1 <- Polygon1
  #   sel1 <- EfficiencyBest[,1:2]
  #   windpo <- 1
  # 
  #   if (is.null(sourceCCL)){
  #     stop("\nNo raster given for the surface roughness. \nAssign the path to the Corine Land Cover raster (.tif) to 'sourceCCL'\n",call. = F)
  #   }
  # 
  #   if (1==1){
  #     Polygon1 <-  sp::spTransform(Polygon1, CRSobj = raster::crs("+proj=longlat +datum=WGS84 +ellps=WGS84
  #                                                                 +towgs84=0,0,0"));
  #     extpol <- round(Polygon1@bbox,0)[,2]
  #     srtm <- raster::getData('SRTM', lon=extpol[1], lat=extpol[2]);
  #     srtm_crop <- raster::crop(srtm, Polygon1);
  #     srtm_crop <- raster::mask(srtm_crop, Polygon1)
  # 
  #     Polygon1 <-  sp::spTransform(Polygon1, CRSobj = raster::crs(ProjLAEA));
  #     srtm_crop <- raster::projectRaster(srtm_crop, crs = raster::crs(ProjLAEA));
  # 
  # 
  #     # Include Corine Land Cover Raster to get an estimation of Surface Roughness
  #     ccl <- raster::raster(x = sourceCCL)
  #     cclPoly <- raster::crop(ccl,Polygon1)
  #     cclPoly1 <- raster::mask(cclPoly,Polygon1)
  #     if (is.null(sourceCCLRoughness)) {
  #       path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
  #       sourceCCLRoughness <- paste0(path, "clc_legend.csv")
  #     } else {
  #       cat("\nYou are using your own Corine Land Cover legend.")
  #       # readline(prompt = "\nPress <ENTER> if you want to continue")
  #       sourceCCLRoughness <- sourceCCLRoughness
  #     }
  #     rauhigkeitz <- utils::read.csv(sourceCCLRoughness,header = TRUE,sep = ";");
  #     cclRaster <- raster::reclassify(cclPoly1,
  #                                     matrix(c(rauhigkeitz$GRID_CODE,rauhigkeitz$Rauhigkeit_z),ncol = 2))
  # 
  # 
  #     # Calculates Wind multiplier. Hills will get higher values, valleys will get lower values.
  #     orogr1 <- raster::calc(srtm_crop, function(x) {x/(raster::cellStats(srtm_crop,mean,na.rm=T))})
  #     orogrnum <- raster::extract(x= orogr1, y = as.matrix((sel1)), buffer=resol*2, small=TRUE,fun= mean,na.rm=T);
  #     windpo <- windpo * orogrnum
  #     ## Get Elevation of Turbine Locations to estimate the air density at the resulting height
  #     heightWind <- raster::extract(x= srtm_crop, y = as.matrix((sel1)), small=TRUE,fun= max,na.rm=T);
  #     
  #     graphics::par(mfrow=c(1,2)); cexa <- 0.9
  #     raster::plot(srtm_crop, main="SRTM Elevation Data");graphics::points(sel1$X,sel1$Y,pch=20);
  #     calibrate::textxy(sel1$X,sel1$Y,labs = round(heightWind,0),cex=0.8);raster::plot(polygon1,add=T)
  #     raster::plot(orogr1, main="Wind Speed Multipliers");graphics::points(sel1$X,sel1$Y,pch=20);
  #     calibrate::textxy(sel1$X,sel1$Y,labs = round(windpo,3),cex=0.8);raster::plot(polygon1,add=T)
  # 
  #     # Get Air Density and Pressure from Height Values
  #     HeighttoBaro <- matrix(heightWind); colnames(HeighttoBaro) <- "HeighttoBaro"
  #     air_dt <- barometric_height(matrix(HeighttoBaro),HeighttoBaro)
  #     # graphics::par(mfrow=c(1,1))
  #     raster::plot(srtm_crop, main="Normal Air Density",col=topo.colors(10));
  #     graphics::points(sel1$X,sel1$Y,pch=20);
  #     calibrate::textxy(sel1$X,sel1$Y,labs = rep(1.225,nrow(sel1)),cex=0.8);
  #     raster::plot(polygon1,add=T)
  #     raster::plot(srtm_crop, main="Corrected Air Density",col=topo.colors(10));
  #     graphics::points(sel1$X,sel1$Y,pch=20);
  #     calibrate::textxy(sel1$X,sel1$Y,labs = round(air_dt$rh,2),cex=0.8);
  #     raster::plot(polygon1,add=T)
  # 
  # 
  #     #CorineLandCover Roughness values
  #     SurfaceRoughness0 <- raster::extract(x= cclRaster, y = as.matrix((sel1)),buffer=resol*2,
  #                                          small=TRUE,fun= mean,na.rm=T);
  #     SurfaceRoughness1 <- raster::extract(x=raster::terrain(srtm_crop,"roughness"),
  #                                          y = as.matrix((sel1)),buffer=resol*2, small=TRUE,fun= mean,na.rm=TRUE);
  #     SurfaceRoughness <-SurfaceRoughness0*(1+(SurfaceRoughness1/max(raster::res(srtm_crop))));
  #     elrouind <- raster::terrain(srtm_crop,"roughness")
  #     elrouindn <- raster::resample(elrouind,cclRaster,method="ngb")
  #     modSurf <- raster::overlay(x = cclRaster,y = elrouindn, fun=function(x,y)
  #                               {return(x*(1+(y/max(raster::res(srtm_crop)))))})
  # 
  #     # graphics::par(mfrow=c(1,1)); cexa <- 0.9
  #     raster::plot(cclRaster, main="Corine Land Cover Roughness");graphics::points(sel1$X,sel1$Y,pch=20);
  #     calibrate::textxy(sel1$X,sel1$Y,labs = round(SurfaceRoughness0,2),cex=cexa);plot(polygon1,add=T)
  #     raster::plot(x=raster::terrain(srtm_crop,"roughness",neighbors = 4), main="Elevation Roughness Indicator");
  #     graphics::points(sel1$X,sel1$Y,pch=20);
  #     calibrate::textxy(sel1$X,sel1$Y,labs = round((SurfaceRoughness1),2),cex=cexa);plot(polygon1,add=T)
  #     raster::plot(modSurf, main="Modified Surface Roughness");graphics::points(sel1$X,sel1$Y,pch=20);
  #     calibrate::textxy(sel1$X,sel1$Y,labs = round((SurfaceRoughness),2),cex=cexa);plot(polygon1,add=T)
  # 
  # 
  #     RotorHeight <- as.integer(resultSafe[1,'inputData']$inputData['Rotor Height',][[1]])
  #     k_raster <- raster::calc(modSurf, function(x) {x <- 0.5/(log(RotorHeight/x))})
  #     # New Wake Decay Constant calculated with new surface roughness values, according to CLC
  #     k <- 0.5/(log(RotorHeight/SurfaceRoughness))
  #     # graphics::par(mfrow=c(1,1)); cexa <- 0.9
  #     raster::plot(k_raster, main="Adapted Wake Decay Constant - K");
  #     graphics::points(sel1$X,sel1$Y,pch=20); calibrate::textxy(sel1$X,sel1$Y,labs = round((k),3),cex=cexa);
  #     raster::plot(polygon1,add=T)
  #   }
  # }
  par(parpplotRes)
  invisible(ResPlotResult)
}
