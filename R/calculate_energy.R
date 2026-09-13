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
#' @param layout One individual: matrix with X/Y (and typically cell IDs).
#' @param wake_angle Angle (degrees) beyond which wake influence is ignored.
#' @param wake_distance Distance (metres) beyond which wake effects are ignored.
#' @param elevation Terrain list from [terrain_model()]. Unused when
#'   `terrain` is `FALSE`.
#' @param ccl_raster Land-cover roughness raster from [terrain_model()].
#' @param park_center Optional numeric of length 2 (`x`, `y`) used as rotation
#'   origin. Computed from the polygon bounding box when missing.
#' @param plot If `TRUE`, the process will be plotted.
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
#' area <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)
#'   ))),
#'   crs = 3035
#' ))
#'
#' ## Create a uniform and unidirectional wind data.frame and plot the
#' ## resulting wind rose
#' data.in <- data.frame(ws = 12, wd = 0)
#' windrosePlot <- plot_windrose(
#'   data = data.in, spd = data.in$ws,
#'   dir = data.in$wd, dirres = 10, spdmax = 20
#' )
#'
#' ## Assign the rotor radius and a factor of the radius for grid spacing.
#' rotor <- 50
#' fcr <- 3
#' resGrid <- grid_area(
#'   area = area, size = rotor * fcr, prop = 1,
#'   plot_grid = TRUE
#' )
#' ## Assign the indexed data frame to new variable. Element 2 of the list
#' ## is the grid, saved as Simple Feature Polygons.
#' resGrid1 <- resGrid[[1]]
#'
#' ## Create an initial population with the indexed Grid, 15 turbines and
#' ## 100 individuals.
#' initpop <- init_population(grid = resGrid1, n = 15, n_start = 100)
#'
#' ## Calculate the expected energy output of the first individual of the
#' ## population.
#' par(mfrow = c(1, 2))
#' plot(area)
#' points(initpop[[1]][, "X"], initpop[[1]][, "Y"], pch = 20, cex = 2)
#' plot(resGrid[[2]], add = TRUE)
#' resCalcEn <- calculate_energy(
#'   layout = initpop[[1]], reference_height = 50,
#'   rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
#'   wake_distance = 100000, wind = data.in,
#'   rotor = 50, area = area, terrain = FALSE,
#'   weibull = FALSE
#' )
#' resCalcEn <- as.data.frame(resCalcEn)
#' plot(area, main = resCalcEn[, "Energy_Output_Red"][[1]])
#' points(x = resCalcEn[, "Bx"], y = resCalcEn[, "By"], pch = 20)
#'
#'
#' ## Create a variable and multidirectional wind data.frame and plot the
#' ## resulting wind rose
#' data.in10 <- data.frame(ws = runif(10, 1, 25), wd = runif(10, 0, 360))
#' windrosePlot <- plot_windrose(
#'   data = data.in10, spd = data.in10$ws,
#'   dir = data.in10$wd, dirres = 10, spdmax = 20
#' )
#'
#' ## Calculate the energy outputs for the first individual with more than one
#' ## wind direction.
#' resCalcEn <- calculate_energy(
#'   layout = initpop[[1]], reference_height = 50,
#'   rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
#'   wake_distance = 100000, wind = data.in10,
#'   rotor = 50, area = area, terrain = FALSE,
#'   weibull = FALSE
#' )
#' }
#'
calculate_energy <- function(layout, reference_height, rotor_height,
                             surface_roughness, wake_angle, wake_distance,
                             area, rotor, wind,
                             elevation = NULL, terrain = FALSE,
                             ccl_raster = NULL, weibull = FALSE,
                             park_center = NULL, plot = FALSE) {
  ## Get default values ###################
  cT <- getOption("windfarmGA.cT", 0.88)
  air_rh <- getOption("windfarmGA.air_rh", 1.225)
  k <- getOption("windfarmGA.k", 0.075)
  cp <- getOption("windfarmGA.Cp", 0.45)
  cut_in <- getOption("windfarmGA.cut_in", 0)
  rated_ws <- getOption("windfarmGA.rated_ws", Inf)
  cut_out <- getOption("windfarmGA.cut_out", Inf)
  pcurve <- getOption("windfarmGA.power_curve", NULL)

  ## Get the Coordinates of the current individual / windfarm ###################
  xy_individual <- layout_xy(layout)

  ## Get Center of Polygon for rotating
  if (is.null(park_center)) {
    park_center <- apply(
      matrix(sf::st_bbox(area), ncol = 2, byrow = FALSE), 1, mean
    )
  }
  pcent <- park_center

  ## Create a dummy vector for the wind speeds for every turbine with value 1
  n_turbines <- length(xy_individual[, 1])
  windpo <- rep(1, n_turbines)

  ## set Graphic Params ###############
  if (plot) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(par(oldpar))
  }

  ## Terrain Effect Model ###################
  cexa <- 0.7
  turb_elev <- rep(1, nrow(xy_individual))
  if (terrain) {
    terr <- terrain_at_layout(
      xy_individual, layout_ids(layout), elevation, ccl_raster, rotor_height
    )
    windpo <- windpo * terr$wind_mult
    turb_elev <- terr$elevation
    surface_roughness <- terr$z0
    k <- terr$k
    air_rh <- terr$air_rh

    if (plot && terrain_has_rasters(elevation)) {
      plot_terrain_energy(
        xy_individual, area, elevation, ccl_raster, terr, cexa
      )
    }
  }

  ## Weibull Wind Speed Estimator ###################
  if (inherits(weibull, "RasterLayer") || inherits(weibull, "character") || inherits(weibull, "SpatRaster")) {
    weibull_bool <- TRUE
    if (!inherits(weibull, "SpatRaster")) {
      weibull <- terra::rast(weibull)
    }
    if (plot) {
      par(mfrow = c(1, 1), ask = FALSE)
      plot(weibull, main = "Weibull Raster")
      plot(area, add = TRUE)
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
  alllist <- vector("list", length(wind[, 1]))
  for (index in 1:length(wind[, 2])) {
    ## Get mean windspeed for every turbine location from windraster ##################
    point_wind <- windpo * wind[index, "ws"]

    ## If Weibull is active/raster, multiply wind speeds with dummy vector ##################
    if (weibull_bool) {
      point_wind <- windpo * estim_speed
    }

    ## Calculate Windspeed according to Rotor Height using the log profile
    ## (or the legacy power law if options(windfarmGA.wind_profile = "power"))
    point_wind <- point_wind * wind_shear_factor(
      rotor_height, reference_height, surface_roughness
    )
    point_wind[is.na(point_wind)] <- 0

    ## Get the current incoming wind direction and assign to "angle"
    angle <- -wind[index, "wd"]
    if (plot) {
      ## Plot turbine locations with angle 0 and open a
      ## second frame for rotated turbine locations
      par(mfrow = c(1, 2))
      plot(st_geometry(area), main = "Shape at angle 0")
      points(xy_individual[, 1], xy_individual[, 2], pch = 20)
      textxy(xy_individual[, 1], xy_individual[, 2],
        labs = dimnames(xy_individual)[[1]], cex = cexa
      )

      ## Rotate and Plot the Polygon
      cordslist <- list(st_coordinates(area))
      cordslist <- lapply(cordslist, function(x) {
        rotate_CPP(x[, 1], x[, 2], pcent[1], pcent[2], angle)
      })
      poly3 <- sf::st_as_sf(sf::st_sfc(
        sf::st_polygon(cordslist),
        crs = st_crs(area)
      ))
      plot(st_geometry(poly3), main = c("Shape at angle:", round(-1 * angle, 2)))
      mtext(paste(
        "Direction: ", index, "\nfrom total: ",
        nrow(wind)
      ), side = 1)
    }

    ## Rotate Coordinates by the incoming wind direction ##################
    xy_individual_rot <- rotate_CPP(
      xy_individual[, 1], xy_individual[, 2],
      pcent[1], pcent[2], angle
    )
    if (plot) {
      ## Plot the rotated turbines in red
      points(xy_individual_rot[, 1], xy_individual_rot[, 2], col = "red", pch = 20)
    }

    ## Bind Wind Data and X/Y Coords together ##################
    xy_individual_rot <- cbind(xy_individual_rot, "Z" = turb_elev)
    dat_xyspeed <- cbind(point_wind, xy_individual_rot)
    colnames(dat_xyspeed) <- c("Windmittel", "X", "Y", "Z")

    ## Get the influecing points given with incoming wind direction angle ##################
    ## and reduce then to data frame
    tmp <- turbine_influences(
      t = xy_individual_rot, wnkl = wake_angle, dist = wake_distance,
      area = area, dirct = angle
    )
    df_all <- do.call("rbind", tmp)

    ## Create a list for every turbine ##################
    ## Assign Windspeed to a filtered list with all turbines and add
    ## the rotor radius
    tmp <- lapply(seq_len(max(df_all[, "Punkt_id"])), function(i) {
      cbind(subset.matrix(df_all, df_all[, "Punkt_id"] == i),
        "Windmean" = dat_xyspeed[, 1L][i]
      )
    })
    windlist <- do.call("rbind", tmp)
    windlist <- windlist[, c(
      "Punkt_id", "Ax", "Ay", "Bx", "By",
      "Laenge_B", "Laenge_A", "alpha",
      "Windrichtung", "Windmean",
      "height1", "height2"
    ), drop = FALSE]
    row.names(windlist) <- NULL
    windlist <- cbind(windlist,
      "RotorR" = as.numeric(rotor)
    )

    ## Change k to lenght of windlist. Repeat or Inflate vector k ##################
    if (!terrain) {
      ## Repeat the vector k
      k1 <- rep(k, length(windlist[, 1]))
    } else {
      ## Inflate the vector k
      k1 <- rep(k, times = table(windlist[, "Punkt_id"]))
    }

    ## Calculate the wake Radius and the rotor area for every turbine ##################
    windlist <- cbind(windlist,
      "WakeR" = as.numeric(windlist[, "Laenge_B"] > 0) *
        (windlist[, "RotorR"] * 2 + 2 * k1 * windlist[, "Laenge_B"]) / 2,
      "Rotorflaeche" = (windlist[, "RotorR"]^2) * pi
    )

    ## Calculate the overlapping area and the overlapping percentage. ##################
    aov <- circle_intersection(
      windlist[, "RotorR"], windlist[, "WakeR"],
      windlist[, "height1"], windlist[, "height2"],
      windlist[, "Laenge_A"]
    )
    aov[windlist[, "Laenge_B"] == 0] <- 0
    absch <- (aov / windlist[, "Rotorflaeche"]) * 100
    absch[aov == 0] <- 0
    windlist <- cbind(windlist,
      "A_ov" = round(aov, 4),
      "AbschatInProz" = round(absch, 4)
    )

    ## Calculate the wind velocity reduction. ##################
    a <- 1 - sqrt(1 - cT)
    s <- windlist[, "Laenge_B"] / windlist[, "RotorR"]
    b <- (1 + (k1 * s))^2
    aov <- windlist[, "A_ov"] / windlist[, "Rotorflaeche"]
    vredu <- windlist[, "Windmean"] * (aov * (a / b))
    windlist <- cbind(windlist,
      "V_red" = vredu
    )

    ## Per turbine: RMS of V_red, sum of wake %, reduced speed, grid ID.
    ## ave() writes back in row order (safer than unlist(unique())).
    id <- windlist[, "Punkt_id"]
    v_i <- ave(windlist[, "V_red"], id, FUN = function(x) sqrt(sum(x^2)))
    tot_ab <- ave(windlist[, "AbschatInProz"], id, FUN = sum)
    windlist <- cbind(
      windlist,
      "V_i" = v_i,
      "TotAbschProz" = tot_ab,
      "V_New" = windlist[, "Windmean"] - v_i,
      "Rect_ID" = layout[id, "ID"]
    )

    ## Get a reduced dataframe and split duplicated Point_id, since a ##################
    ## turbine with fixed Point_id, can have several influencing turbines
    ## and therefore several matrix rows
    windlist2 <- subset.matrix(
      windlist,
      select = c(
        "Punkt_id", "Ax", "Ay", "Bx", "By",
        "Laenge_B", "Laenge_A", "Windrichtung",
        "Windmean", "RotorR", "WakeR", "A_ov",
        "TotAbschProz", "V_New", "Rect_ID"
      )
    )

    ## Get unique turbine locations, to calculate correct energy outputs ##################
    windlist1 <- subset.matrix(windlist2,
      subset = !duplicated(windlist2[, "Punkt_id"])
    )

    ## Change air-density to length of windlist1. Repeat or inflate ##################
    if (!terrain) {
      airrh <- rep(air_rh, length(windlist1[, 1]))
    } else {
      airrh <- air_rh
    }

    ## Park energy (kW) and efficiency. energy_calc_CPP already sums
    ## turbines; a power-curve table must be summed the same way.
    if (!is.null(pcurve)) {
      energy_reduced <- sum(lookup_power_curve(windlist1[, "V_New"], pcurve))
      energy_full <- sum(lookup_power_curve(windlist1[, "Windmean"], pcurve))
    } else {
      v_red <- apply_power_curve(windlist1[, "V_New"], cut_in, rated_ws, cut_out)
      v_full <- apply_power_curve(windlist1[, "Windmean"], cut_in, rated_ws, cut_out)
      energy_reduced <- energy_calc_CPP(
        v_red,
        windlist1[, "RotorR"], airrh
      ) * (cp / 0.593)
      energy_full <- energy_calc_CPP(
        v_full,
        windlist1[, "RotorR"], airrh
      ) * (cp / 0.593)
    }
    efficiency <- if (energy_full > 0) {
      (energy_reduced * 100) / energy_full
    } else {
      0
    }


    ## Assign values back to complete matrix ##################
    windlist2 <- cbind(windlist2,
      "Energy_Output_Red" = energy_reduced,
      "Energy_Output_Voll" = energy_full,
      "Parkwirkungsgrad" = efficiency
    )

    windlist2[, "Windrichtung"] <- windlist2[, "Windrichtung"] * (-1)

    alllist[[index]] <- windlist2
  }
  invisible(alllist)
}

#' @title Get area of intersecting circles
#' @name circle_intersection
#' @description Calculate the intersection area of two circles with different
#'   radii and different heights
#'
#' @export
#'
#' @param r1 The radius of circle 1
#' @param r2 The radius of circle 2
#' @param h1 The height of the circle center 1
#' @param h2 The height of the circle center 2
#' @param dx The distance on the x-axis between both centers
#'
#' @family Wind Energy Calculation Functions
#' @return A numeric vector; one intersection area per pair. Scalars
#'   stay length 1.
#'
circle_intersection <- function(r1, r2, h1, h2, dx) {
  circle_intersection_CPP(r1, r2, h1, h2, dx)
}

wind_shear_factor <- function(hub, ref, z0) {
  if (abs(hub - ref) < 1e-9) {
    return(1)
  }
  method <- getOption("windfarmGA.wind_profile", "log")
  if (identical(method, "power")) {
    return((hub / ref)^z0)
  }
  z0_cap <- pmin(z0, 0.99 * min(hub, ref))
  z0_cap <- pmax(z0_cap, 1e-6)
  log(hub / z0_cap) / log(ref / z0_cap)
}

apply_power_curve <- function(v, cut_in, rated_ws, cut_out) {
  v_out <- pmin(v, rated_ws)
  v_out[v < cut_in | v >= cut_out] <- 0
  v_out
}

power_curve_xy <- function(curve) {
  if (is.null(curve)) {
    return(NULL)
  }
  curve <- as.data.frame(curve)
  ws <- if ("ws" %in% names(curve)) curve$ws else curve[[1]]
  pw <- if ("power" %in% names(curve)) curve$power else curve[[2]]
  data.frame(ws = as.numeric(ws), power = as.numeric(pw))
}

lookup_power_curve <- function(v, curve) {
  xy <- power_curve_xy(curve)
  as.numeric(stats::approx(xy$ws, xy$power, xout = as.numeric(v), rule = 2)$y)
}

#' @title Manufacturer power curve
#' @name plot_power_curve
#' @description Linear interpolation of a two-column table (`ws`, `power`
#'   in kW). Set it with `ga_options(power_curve = curve)` or
#'   `options(windfarmGA.power_curve = curve)`. While a curve is set,
#'   park energy is the sum of those kW values instead of Cp * v^3.
#'   Cut-in / rated / cut-out still apply only when no table is set.
#'   If hub wind stays on the rated plateau after wakes, every layout
#'   looks the same - use wind in the rising part of the curve.
#'   Supply your own table (manufacturer data); the package does not
#'   ship copyrighted curves.
#' @export
#'
#' @param curve A data.frame with wind speed and power. Default is the
#'   current `windfarmGA.power_curve` option.
#' @param plot If `TRUE`, draw the curve. Default is `TRUE`.
#' @return The interpolated table, invisibly.
#'
#' @examples
#' curve <- data.frame(
#'   ws = c(0, 3, 4, 8, 12, 25, 26),
#'   power = c(0, 0, 80, 1200, 2000, 2000, 0)
#' )
#' plot_power_curve(curve)
plot_power_curve <- function(curve = NULL, plot = TRUE) {
  if (is.null(curve)) {
    curve <- getOption("windfarmGA.power_curve", NULL)
  }
  if (is.null(curve)) {
    stop("No power curve. Pass a data.frame(ws, power) or ga_options(power_curve = ...).")
  }
  xy <- power_curve_xy(curve)
  if (isTRUE(plot)) {
    graphics::plot(
      xy$ws, xy$power, type = "l", lwd = 2,
      xlab = "Wind speed (m/s)", ylab = "Power (kW)",
      main = "Power curve"
    )
    graphics::points(xy$ws, xy$power, pch = 16)
  }
  invisible(xy)
}
