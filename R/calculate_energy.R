#' @title Calculate Energy Outputs of Individuals
#' @name calculate_energy
#' @description Calculate the energy output and efficiency rates of an
#'   individual in the current population under all given wind directions and
#'   speeds. If the terrain effect model is activated, the main calculations to
#'   model those effects will be done in this function.
#'
#' @export
#'
#' @inheritParams genetic_algorithm
#' @inheritParams fitness
#' @param sel A matrix of an individual of the current population
#' @param srtm_crop The first element of the \code{\link{terrain_model}} resulting list
#' @param cclRaster The second element of the \code{\link{terrain_model}} resulting list
#' @param wnkl The angle from which wake influences are considered to be 
#'   negligible
#' @param distanz The distance after which wake effects are considered
#'   to be eliminated
#' @param polygon1 The considered area as Simple Feature Polygon
#' @param RotorR The desired rotor radius in meter
#' @param dirSpeed The wind speed and direction data.frame
#' @param plotit If \code{TRUE}, the process will be plotted. 
#'   Default is \code{FALSE}
#'
#' @family Wind Energy Calculation Functions
#' @return Returns a list of an individual of the current generation with
#'   resulting wake effects, energy outputs, efficiency rates for every wind
#'   direction. The length of the list corresponds to the number of different
#'   wind directions.
#'
#' @examples \donttest{
#' ## Create a random Polygon
#' library(sf)
#' Polygon1 <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)))), 
#'   crs = 3035
#' ))
#'
#' ## Create a uniform and unidirectional wind data.frame and plot the
#' ## resulting wind rose
#' data.in <- data.frame(ws = 12, wd = 0)
#' windrosePlot <- plot_windrose(data = data.in, spd = data.in$ws,
#'                 dir = data.in$wd, dirres=10, spdmax=20)
#'
#' ## Assign the rotor radius and a factor of the radius for grid spacing.
#' Rotor= 50; fcrR= 3
#' resGrid <- grid_area(shape = Polygon1, size = Rotor*fcrR, prop=1,
#'                       plotGrid = TRUE)
#' ## Assign the indexed data frame to new variable. Element 2 of the list
#' ## is the grid, saved as Simple Feature Polygons.
#' resGrid1 <- resGrid[[1]]
#'
#' ## Create an initial population with the indexed Grid, 15 turbines and
#' ## 100 individuals.
#' initpop <- init_population(Grid = resGrid1, n = 15, nStart = 100)
#'
#' ## Calculate the expected energy output of the first individual of the
#' ## population.
#' par(mfrow = c(1,2))
#' plot(Polygon1); points(initpop[[1]][,'X'],initpop[[1]][,'Y'], pch=20,cex=2)
#' plot(resGrid[[2]], add = TRUE)
#' resCalcEn <- calculate_energy(sel=initpop[[1]],referenceHeight= 50,
#'                    RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
#'                    distanz = 100000, dirSpeed = data.in,
#'                    RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
#'                    weibull = FALSE)
#' resCalcEn <- as.data.frame(resCalcEn)
#' plot(Polygon1, main = resCalcEn[,'Energy_Output_Red'][[1]])
#' points(x = resCalcEn[,'Bx'], y = resCalcEn[,'By'], pch = 20)
#'
#'
#' ## Create a variable and multidirectional wind data.frame and plot the
#' ## resulting wind rose
#' data.in10 <- data.frame(ws = runif(10,1,25), wd = runif(10,0,360))
#' windrosePlot <- plot_windrose(data = data.in10, spd = data.in10$ws,
#'                 dir = data.in10$wd, dirres=10, spdmax=20)
#'
#' ## Calculate the energy outputs for the first individual with more than one
#' ## wind direction.
#' resCalcEn <- calculate_energy(sel=initpop[[1]],referenceHeight= 50,
#'                    RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
#'                    distanz = 100000, dirSpeed = data.in10,
#'                    RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
#'                    weibull = FALSE)
#' }
#'
calculate_energy <- function(sel, referenceHeight, RotorHeight,
                             SurfaceRoughness, wnkl, distanz,
                             polygon1, RotorR, dirSpeed,
                             srtm_crop, topograp, cclRaster, weibull,
                             plotit = FALSE) {

  ## Get default values ###################
  cT <- getOption("windfarmGA.cT")
  air_rh <- getOption("windfarmGA.air_rh")
  k <- getOption("windfarmGA.k")

  ## Get the Coordinates of the current individual / windfarm ###################
  xy_individual <- sel[, 2:3]

  ## Get Center of Polygon for rotating
  ## TODO - this can go in some upper level
  pcent <- apply(matrix(sf::st_bbox(polygon1), ncol = 2, byrow = FALSE), 1, mean)

  ## Create a dummy vector for the wind speeds for every turbine with value 1
  n_turbines <- length(xy_individual[, 1])
  windpo <- rep(1, n_turbines)

  ## set Graphic Params ###############
  if (plotit) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(par(oldpar))
  }
  
  ## Terrain Effect Model ###################
  cexa <- 0.7
  if (topograp) {
    ## Calculate Wind multiplier - Hills get higher values, valleys get lower values.
    wind_multiplier <- srtm_crop[[2]]
    wind_multiplier_val <- terra::extract(x = wind_multiplier, y = xy_individual)
    wind_multiplier_val[is.na(wind_multiplier_val),] <- mean(wind_multiplier_val[,1], na.rm = TRUE)
    windpo <- windpo * wind_multiplier_val
    windpo <- windpo[,1]

    ## Get Elevation of Turbine Locations to estimate the air density
    turb_elev <- terra::extract(x = srtm_crop[[1]], y = xy_individual)
    turb_elev[is.na(turb_elev),] <- mean(turb_elev[,1], na.rm = TRUE)
    turb_elev <- turb_elev[,1]
    ## Plot the elevation and the wind speed multiplier rasters
    if (plotit) {
      
      par(mfrow = c(2, 1))
      plot(srtm_crop[[1]], main = "SRTM Elevation Data")
      points(xy_individual[, "X"], xy_individual[, "Y"], pch = 20)
      calibrate::textxy(xy_individual[, "X"], xy_individual[, "Y"],
                        labs = round(turb_elev, 0),
                        cex = cexa)
      plot(st_geometry(polygon1), add = TRUE)
      plot(wind_multiplier, main = "Wind Speed Multipliers")
      points(xy_individual[, "X"], xy_individual[, "Y"], pch = 20)
      calibrate::textxy(xy_individual[, "X"], xy_individual[, "Y"],
                        labs = round(windpo, 3),
                        cex = cexa)
      plot(st_geometry(polygon1), add = TRUE)
    }

    ## Get Air Density and Pressure from Height Values
    air_dt <- barometric_height(matrix(turb_elev), turb_elev)
    air_rh <- as.numeric(air_dt[, "rh"])
    ## Plot the normal and corrected Air Density Values
    if (plotit) {
      par(mfrow = c(1, 1))
      plot(srtm_crop[[1]], main = "Normal Air Density", col = topo.colors(10))
      points(xy_individual[, "X"], xy_individual[, "Y"], pch = 20)
      calibrate::textxy(xy_individual[, "X"], xy_individual[, "Y"],
                        labs = rep(round(air_rh, 4), nrow(xy_individual)), cex = cexa)
      plot(st_geometry(polygon1), add = TRUE)
      terra::plot(srtm_crop[[1]], main = "Corrected Air Density",
                   col = topo.colors(10))
      points(xy_individual[, "X"], xy_individual[, "Y"], pch = 20)
      calibrate::textxy(xy_individual[, "X"], xy_individual[, "Y"],
                        labs = round(air_dt[, "rh"], 4), cex = cexa)
      plot(st_geometry(polygon1), add = TRUE)
    }

    ## Corine Land Cover Surface Roughness values
    land_rough <- terra::extract(x = cclRaster, y = xy_individual)
    land_rough <- land_rough[,1]
    land_rough[is.na(land_rough)] <- mean(land_rough, na.rm = TRUE)

    ## Elevation Roughness values
    terrain_rough_ras <- srtm_crop[[3]]
    terrain_rough_vals <- terra::extract(x = terrain_rough_ras, y = xy_individual)
    terrain_rough_vals <- terrain_rough_vals[,1]
    terrain_rough_vals[is.na(terrain_rough_vals)] <- mean(terrain_rough_vals, na.rm = TRUE)
    maxrasres <- max(terra::res(terrain_rough_ras))
    
    ## Calculate modified surface Roughness
    SurfaceRoughness <- land_rough * (1 + (terrain_rough_vals / maxrasres))
    
    ## Plot the different Surface Roughness Values
    if (plotit) {
      terrain_rough_resample <- terra::resample(terrain_rough_ras, cclRaster, method = "near")
      modified_rough <- terra::lapp(x = c(cclRaster, terrain_rough_resample),
                                 fun = function(x, y) {
                                   return(x * (1 + y / maxrasres))
                                 })
      
      graphics::par(mfrow = c(1, 1))
      
      plot(cclRaster, main = "Corine Land Cover Roughness")
      graphics::points(xy_individual[, "X"], xy_individual[, "Y"], pch = 20)
      calibrate::textxy(xy_individual[, "X"], xy_individual[, "Y"],
                        labs = round(land_rough, 2), cex = cexa)
      plot(st_geometry(polygon1), add = TRUE)
      plot(x = terrain_rough_ras, main = "Elevation Roughness Indicator")
      graphics::points(xy_individual[, "X"], xy_individual[, "Y"], pch = 20)
      calibrate::textxy(xy_individual[, "X"], xy_individual[, "Y"],
                        labs = round(1 + (terrain_rough_vals / maxrasres), 2), cex = cexa)
      plot(st_geometry(polygon1), add = TRUE)
      plot(modified_rough, main = "Modified Surface Roughness")
      graphics::points(xy_individual[, "X"], xy_individual[, "Y"], pch = 20)
      calibrate::textxy(xy_individual[, "X"], xy_individual[, "Y"],
                        labs = round(SurfaceRoughness, 2), cex = cexa)
      plot(st_geometry(polygon1), add = TRUE)
    }

    ## New Wake Decay Constant calculated with new surface roughness values
    k <- 0.5 / (log(RotorHeight / SurfaceRoughness))

    ## Plot resulting Wake Decay Values
    if (plotit) {
      graphics::par(mfrow = c(1, 1))
      plot(x = terrain_rough_ras, main = "Adapted Wake Decay Values - K")
      graphics::points(xy_individual[, "X"], xy_individual[, "Y"], pch = 20)
      calibrate::textxy(xy_individual[, "X"], xy_individual[, "Y"],
                        labs = round(k, 3), cex = cexa)
      plot(st_geometry(polygon1), add = TRUE)
    }
  }

  ## Weibull Wind Speed Estimator ###################
  if (inherits(weibull, "RasterLayer") || inherits(weibull, "character") || inherits(weibull, "SpatRaster")) {
    weibull_bool <- TRUE
    if (!inherits(weibull, "SpatRaster")) {
      weibull <- terra::rast(weibull)
    }
    if (plotit) {
      par(mfrow = c(1, 1), ask = FALSE)
      plot(weibull, main = "Weibull Raster")
      plot(polygon1, add = TRUE)
    }
    ## Extract Weibul values for turbine locations
    estim_speed <- terra::extract(weibull, xy_individual)[[1]]
    ## Check and replace NA Values..
    if (anyNA(estim_speed)) {
      estim_speed[which(is.na(estim_speed))] <- mean(estim_speed, na.rm = TRUE)
    }
    ## Multiply dummy vector `windpo` with expected wind speeds
    point_wind <- windpo * estim_speed
  } else {
    weibull_bool <- FALSE
  }

  ## Calculate Energy for all incoming wind directions ###################
  ## Rotate Polygon for all angles and analyze which turbine is affected by 
  ## another one to calculate total energy output.
  alllist <- vector("list", length(dirSpeed[, 1]))
  for (index in 1:length(dirSpeed[, 2])) {
    ## Get mean windspeed for every turbine location from windraster
    point_wind <- windpo * dirSpeed[index, "ws"]

    ## If Weibull is active/raster, multiply wind speeds with dummy vector
    if (weibull_bool) {
      point_wind <- windpo * estim_speed
    }

    ## Calculate Windspeed according to Rotor Height using wind profile law
    ## TODO MISSING: Include other laws: -log
    point_wind <- point_wind * ((RotorHeight / referenceHeight) ^ SurfaceRoughness)
    point_wind[is.na(point_wind)] <- 0

    ## Get the current incoming wind direction and assign to "angle"
    angle <- -dirSpeed[index, "wd"]
    if (plotit) {
      ## Plot turbine locations with angle 0 and open a
      ## second frame for rotated turbine locations
      par(mfrow = c(1, 2))
      plot(st_geometry(polygon1), main = "Shape at angle 0")
      points(xy_individual[, 1], xy_individual[, 2], pch = 20)
      textxy(xy_individual[, 1], xy_individual[, 2],
             labs = dimnames(xy_individual)[[1]], cex = cexa)
      
      ## Rotate and Plot the Polygon
      cordslist <- list(st_coordinates(polygon1))
      cordslist <- lapply(cordslist, function(x) {
        rotate_CPP(x[,1], x[,2], pcent[1], pcent[2], angle)
      })
      poly3 <- sf::st_as_sf(sf::st_sfc(
        sf::st_polygon(cordslist), crs = st_crs(polygon1)))
      plot(st_geometry(poly3), main = c("Shape at angle:", round(-1 * angle, 2)))
      mtext(paste("Direction: ", index, "\nfrom total: ",
                  nrow(dirSpeed)), side = 1)
    }

    ## Rotate Coordinates by the incoming wind direction
    xy_individual_rot <- rotate_CPP(xy_individual[, 1], xy_individual[, 2],
                                pcent[1], pcent[2], angle)
    if (plotit) {
      ## Plot the rotated turbines in red
      points(xy_individual_rot[, 1], xy_individual_rot[, 2], col = "red", pch = 20)
    }

    ## Bind Wind Data and X/Y Coords together
    dat_xyspeed <- cbind(point_wind, xy_individual_rot)
    colnames(dat_xyspeed) <- c("Windmittel", "X", "Y")

    ## Get the influecing points given with incoming wind direction angle
    ## and reduce then to data frame
    tmp <- turbine_influences(t = xy_individual_rot, wnkl = wnkl, dist = distanz,
                              polYgon = polygon1, dirct = angle)
    df_all <- do.call("rbind", tmp)

    ## Sometimes betha / gamma are NA - Set to 0.. 
    ## TODO Why/When is that hapenning?
    if (any(is.na(df_all))) {
      df_all[which(is.na(df_all))] <- 0
    }

    ## Create a list for every turbine
    ## Assign Windspeed to a filtered list with all turbines and add
    ## the rotor radius
    tmp <- lapply(seq_len(max(df_all[, "Punkt_id"])), function(i) {
      cbind(subset.matrix(df_all, df_all[, "Punkt_id"] == i),
            "Windmean" = dat_xyspeed[, 1L][i])
    })
    windlist <- do.call("rbind", tmp)
    windlist <- windlist[, c("Punkt_id", "Ax", "Ay", "Bx", "By",
                             "Laenge_B", "Laenge_A", "alpha",
                             "Windrichtung", "Windmean"), drop = FALSE]
    row.names(windlist) <- NULL
    windlist <- cbind(windlist,
                      "RotorR" = as.numeric(RotorR))

    ## Change k to lenght of windlist. Repeat or Inflate vector k
    if (!topograp) {
      ## Repeat the vector k
      k1 <- rep(k, length(windlist[, 1]))
    } else {
      ## Inflate the vector k
      k1 <- rep(k, table(windlist[, "Punkt_id"]))
    }

    ## Calculate the wake Radius and the rotor area for every turbine
    lnro <- length(windlist[, 1])
    windlist <- cbind(windlist,
                      "WakeR" = as.numeric(windlist[, "Laenge_B"] > 0) *
                        (windlist[, "RotorR"] * 2 + 2 * k1 *
                           windlist[, "Laenge_B"]) / 2,
                      "Rotorflaeche" = (windlist[, "RotorR"] ^ 2) * pi )

    ## Calculate the overlapping area and the overlapping percentage.
    ## TODO make this in 3D (especially if terrain is included)
    tmp <- sapply(1:lnro, function(o) {
      Rotorf <- windlist[o, "RotorR"]
      leA <- windlist[o, "Laenge_A"]
      wakr <- windlist[o, "WakeR"]
      if (windlist[o, "Laenge_B"] == 0) {
        aov <- 0
      } else {
        ## TODO - why not if-else ?
        if ( (wakr - Rotorf) >= leA && leA >= 0) {
          aov <- (windlist[o, "RotorR"] ^ 2) * pi
        }
        if ( (wakr + Rotorf) <= leA) {
          aov <- 0
        }
        if ( (wakr - Rotorf) <= leA && leA <= (wakr + Rotorf))  {
          aov <- wake_CPP(Rotorf = Rotorf,  wakr = wakr, leA = leA)
        }
      }
      if (aov != 0) {
        absch <- ((aov / windlist[o, "Rotorflaeche"]) * 100)
      } else {
        absch <- 0
      }
      c(aov, absch)
    })
    windlist <- cbind(windlist,
                      "A_ov" = round(tmp[1, ], 4),
                      "AbschatInProz" = round(tmp[2, ], 4))

    ## Calculate the wind velocity reduction.
    a <- 1 - sqrt(1 - cT)
    s <- windlist[, "Laenge_B"] / windlist[, "RotorR"]
    b <- (1 + (k1 * s))^2
    aov <- windlist[, "A_ov"] / windlist[, "Rotorflaeche"]
    vredu <- windlist[, "Windmean"] * (aov * (a / b))
    windlist <- cbind(windlist,
                      "V_red" = vredu)

    ## Calculate multiple wake effects, total wake influence,
    ## the new resulting wind velocity and add the Grid IDs.
    whichh <- windlist[, "Punkt_id"]
    windlist <- cbind(windlist,
                      "V_i" = 0,
                      "TotAbschProz" = 0,
                      "V_New" = 0,
                      "Rect_ID" = 0)

    ## Sum up the wind speed reduction from all possible influental turbines
    windlist[, "V_i"] <- unlist(lapply(unique(whichh), function(i) {
      sums <- sqrt(sum(windlist[whichh == i, "V_red"] ^ 2))
      rep(sums, length(windlist[whichh == i, "V_red"]))
    }))
    ## Sum up the wake effects from all possible influental turbines
    windlist[, "TotAbschProz"] <- unlist(lapply(unique(whichh), function(i) {
      absch <- windlist[whichh == i, "AbschatInProz"]
      rep(sum(absch), length(absch))
    }))
    ## Caluclate new wins speed, after reduction
    windlist[, "V_New"] <- unlist(lapply(unique(whichh), function(i) {
      windlist[whichh == i, "Windmean"] - windlist[whichh == i, "V_i"]
    }))
    ## Assign the Grid-ID to all influential turbines
    windlist[, "Rect_ID"] <-  unlist(lapply(unique(whichh), function(i) {
      rep(sel[i, "ID"], length(windlist[whichh == i, 1]))
    }))

    ## Get a reduced dataframe and split duplicated Point_id, since a
    ## turbine with fixed Point_id, can have several influencing turbines
    ## and therefore several matrix rows
    windlist2 <- subset.matrix(
      windlist,
      select = c("Punkt_id", "Ax", "Ay", "Bx", "By",
                 "Laenge_B", "Laenge_A", "Windrichtung",
                 "Windmean", "RotorR", "WakeR", "A_ov",
                 "TotAbschProz", "V_New", "Rect_ID"))

    ## Get unique turbine locations, to calculate correct energy outputs
    windlist1 <- subset.matrix(windlist2,
                               subset = !duplicated(windlist2[, "Punkt_id"]))

    ## Change air-density to length of windlist1. Repeat or inflate
    if (!topograp) {
      airrh <- rep(air_rh, length(windlist1[, 1]))
    } else {
      airrh <- air_rh
    }

    ## Calculate Full and Reduced Energy Outputs in kW and
    ## Park Efficienca in %.
    energy_reduced <- energy_calc_CPP(windlist1[, "V_New"],
                                 windlist1[, "RotorR"], airrh)
    energy_full <- energy_calc_CPP(windlist1[, "Windmean"],
                                 windlist1[, "RotorR"], airrh)
    efficiency <- (energy_reduced * 100) / energy_full


    ## Assign values back to complete matrix
    windlist2 <- cbind(windlist2,
                       "Energy_Output_Red" = energy_reduced,
                       "Energy_Output_Voll" = energy_full,
                       "Parkwirkungsgrad" = efficiency)

    windlist2[, "Windrichtung"] <- windlist2[, "Windrichtung"] * (-1)

    alllist[[index]] <- windlist2
  }
  invisible(alllist)
}
