#' @title Randomize the output of the Genetic Algorithm
#' @name random_search
#' @description Jitter the best GA layouts inside their grid cells and
#'   re-evaluate energy. Use this as a short post-search after
#'   [genetic_algorithm()]. Terrain and Weibull follow the GA flags
#'   when `terrain` / `weibull` are `NULL`. Terrain rasters from the GA
#'   are reused when stored in `result`; pass a DEM to rebuild. Weibull
#'   rasters are not stored; pass `weibull_src` again if needed.
#'
#' @export
#' @inheritParams genetic_algorithm
#' @param result The resulting matrix of the function \code{\link{genetic_algorithm}}
#' @param best How many distinct best layouts to refine. Default is 1.
#' @param runs How many jittered layouts to try per `best` start.
#'   Default is 20.
#' @param plot Draw the random-search layouts
#' @param max_dist A numeric value multiplied by the rotor radius to perform
#'   collision checks. Default is \code{2.2}
#' @param terrain `NULL` (default) follows the GA and reuses
#'   `result$terrainModel`. `TRUE` downloads only if nothing is stored.
#'   A DEM rebuilds the model. `FALSE` skips terrain.
#' @param weibull `NULL` follows the GA flag. Weibull rasters are not
#'   stored; pass `weibull_src` (or a speed raster as `weibull`) again.
#'   Giving `weibull_src` is enough; you do not also need `weibull = TRUE`.
#'
#' @family Randomization
#' @return Returns a list.
#'
#' @examples \donttest{
#' new <- random_search(resultrect, sp_polygon, runs = 20, best = 4)
#' plot_random_search(resultRS = new, result = resultrect, area = sp_polygon, best = 2)
#' }
random_search <- function(result, area, runs = 20, best = 1, plot = FALSE,
                          max_dist = 2.2, terrain = NULL, weibull = NULL,
                          weibull_src = NULL, ccl = NULL,
                          ccl_roughness = NULL) {
  ## Data Config ############################
  # Order the resulting layouts with highest Energy output
  resldat <- do.call("rbind", result[, "bestPaEn"])
  inp0 <- ga_input_matrix(result)
  maxDist <- as.numeric(inp0["Rotorradius", 1]) * max_dist

  ## Remove duplicated layouts based on x, y energy / efficiency
  resldat <- resldat[!duplicated(resldat[, 1:4]), ]

  if (plot) {
    plot.new()
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar), add = TRUE)
    par(
      mfrow = c(1, 1),
      mar = c(8, 4, 4, 2)
    )
  }

  ## Process Data ########
  ## Remove duplicated "Runs", assign do resldat and sort by Energy
  resldat <- as.data.frame(resldat[!duplicated(resldat[, "Run"]), , drop = FALSE])
  resldat$GARun <- 1:nrow(resldat)
  ## Sort by EnergyOverall
  resldat <- resldat[order(resldat[, 4], decreasing = TRUE), ]

  ## Get the GA-runs of the best layouts
  if (best > nrow(resldat)) {
    message(paste0(
      "Only ", nrow(resldat), " unique layouts found. Set 'best' to ",
      nrow(resldat)
    ))
    best <- nrow(resldat)
  }
  bestGARunIn <- resldat$GARun[1:best]

  inp <- ga_input_matrix(result, bestGARunIn[1])
  resolu <- as.numeric(inp["Resolution", 1])
  rotRad <- as.numeric(inp["Rotorradius", 1])
  winddata <- result[bestGARunIn[1], ]$inputWind
  ## Get max factor for alteration of coordination
  maxFac <- rotRad * (resolu / (rotRad * 2))

  ## Grid the Polygon ############
  area <- isSpatial(area = area)
  grid_method <- toupper(as.character(inp["Grid Method", 1]))
  if (grid_method != "HEXAGON" && grid_method != "H") {
    # Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
    propu <- as.numeric(inp["Percentage of Polygon", 1])
    Grid <- grid_area(area = area, size = resolu, prop = propu)
  } else {
    # Calculate a Grid with hexagonal grid cells
    Grid <- hexa_area(area, resolu)
  }

  ## Windata Formatting ###################
  winddata <- windata_format(winddata)
  probabDir <- winddata[[2]]
  winddata <- winddata[[1]]

  ## Heights by name: 5.0.0 dropped Trim/Crossover rows, so [12]/[13] are wrong.
  phys <- random_search_physics(
    result, area,
    run = bestGARunIn[1],
    terrain = terrain, weibull = weibull, weibull_src = weibull_src,
    ccl = ccl, ccl_roughness = ccl_roughness
  )
  if (isTRUE(phys$terrain)) {
    phys$elevation <- terrain_ensure_cells(
      phys$elevation, phys$ccl_raster, Grid[[1]], phys$rotor_height
    )
  }
  ref_height <- phys$ref_height
  rotor_height <- phys$rotor_height
  rotor_radius <- phys$rotor_radius

  max_angle <- getOption("windfarmGA.max_angle")
  max_dist <- getOption("windfarmGA.max_distance")

  ## Run Random Search  ################
  RandResultAll <- vector(mode = "list", length = best)
  for (o in 1:best) {
    bestGARun <- bestGARunIn[o]

    ## Get the starting layout of windfarm[o]
    layout_start <- result[bestGARun, ]$bestPaEn
    coordLay <- cbind(
      X = as.numeric(layout_start[, "X"]),
      Y = as.numeric(layout_start[, "Y"])
    )
    cell_id <- as.integer(layout_start[, "Rect_ID"])

    if (plot) {
      plot(sf::st_geometry(Grid[[2]]), reset = FALSE)
      points(coordLay, pch = 15, col = "black")

      legend(
        x = "bottom",
        inset = c(0, -0.45),
        xpd = NA,
        legend = c(
          "Starting Location", "Randomly generated Location",
          "Suitable Location", "Relocated due to Turbine Collision"
        ),
        col = c("black", "blue", "green", "red"),
        lwd = 1, lty = c(0, 0),
        pch = c(15, 3, 1, 20)
      )
    }

    ## Run n random searches on windfarm[o]
    RandResult <- vector(mode = "list", length = runs)
    for (i in 1:runs) {
      coordLayTmp <- vector(mode = "list", length = length(coordLay[, 1]))
      ## For every turbine, alter x/y coordinates randomly
      for (j in 1:length(coordLay[, 1])) {
        maxAlterX <- runif(1, min = -maxFac, max = maxFac)
        maxAlterY <- runif(1, min = -maxFac, max = maxFac)
        cordNew <- coordLay[j, ]
        cordNew[1] <- cordNew[1] + maxAlterX
        cordNew[2] <- cordNew[2] + maxAlterY
        if (plot) {
          points(cordNew[1], cordNew[2], col = "blue", pch = 3)
        }
        coordLayTmp[[j]] <- cordNew
      }
      coordsj <- do.call("rbind", coordLayTmp)

      ## Check if turbines are not colliding #####################
      pointsDistBl <- st_as_sf(data.frame(coordsj), coords = c("X", "Y"))
      pointsDist <- st_distance(pointsDistBl)
      distMin <- pointsDist[which(pointsDist < maxDist & pointsDist != 0)]

      while (length(distMin) > 0) {
        pointsDistBl <- st_as_sf(data.frame(coordsj), coords = c("X", "Y"))
        pointsDist <- st_distance(pointsDistBl)
        distMin <- pointsDist[which(pointsDist < maxDist & pointsDist != 0)]
        if (length(distMin) != 0) {
          pointsDist <- data.frame(pointsDist)
          colnames(pointsDist) <- 1:length(pointsDist)

          pointsDist <- round(pointsDist, 2)
          distMin <- round(distMin, 2)
          distMin <- distMin[duplicated(distMin)]

          ColRowMin <- which(pointsDist == distMin,
            useNames = TRUE, arr.ind = TRUE
          )

          CoordsWrongOrigin <- coordLay[ColRowMin[1, 2], ]

          maxAlterX <- runif(1, min = -maxFac, max = maxFac)
          maxAlterY <- runif(1, min = -maxFac, max = maxFac)
          cordNew <- CoordsWrongOrigin
          cordNew[1] <- cordNew[1] + maxAlterX
          cordNew[2] <- cordNew[2] + maxAlterY

          if (plot) {
            points(
              x = coordsj[ColRowMin[1, 2], 1],
              y = coordsj[ColRowMin[1, 2], 2],
              col = "red", cex = 1.2, pch = 20
            )
          }
          coordsj[ColRowMin[1, 2], ] <- cordNew
        }
      }
      if (plot) {
        points(coordsj, col = "green", cex = 1.5)
      }
      #####################

      ## Arrange random points to input for calculate_energy
      coordsj <- cbind(coordsj,
        "ID" = cell_id,
        "bin" = 1
      )
      coordsj <- coordsj[, c("ID", "X", "Y", "bin")]

      # Calculate energy and save in list with length `runs` ################
      resCalcen <- calculate_energy(
        layout = coordsj,
        reference_height = ref_height,
        rotor_height = rotor_height,
        surface_roughness = 0.3,
        wake_angle = max_angle, wake_distance = max_dist,
        wind = winddata,
        rotor = rotor_radius,
        area = area, terrain = phys$terrain,
        elevation = phys$elevation, ccl_raster = phys$ccl_raster,
        weibull = phys$weibull
      )

      ## Process Result ###################
      ## TODO - optimize all next lines (calculate_energy has already beeter method)
      ee <- lapply(resCalcen, function(x) {
        subset.matrix(x,
          subset = !duplicated(x[, "Punkt_id"]),
          select = c(
            "Bx", "By", "Windrichtung", "RotorR", "TotAbschProz", "V_New",
            "Rect_ID", "Energy_Output_Red", "Energy_Output_Voll",
            "Parkwirkungsgrad"
          )
        )
      })

      # get Energy Output and Efficiency rate for every wind direction
      enOut <- lapply(ee, function(x) {
        subset.matrix(x,
          subset = c(TRUE, rep(FALSE, length(ee[[1]][, 1]) - 1)),
          select = c(
            "Windrichtung", "Energy_Output_Red",
            "Parkwirkungsgrad"
          )
        )
      })
      enOut <- do.call("rbind", enOut)

      # Add the Probability of every direction
      # Calculate the relative Energy outputs relative to wind direction probabilities
      enOut <- cbind(enOut, "probabDir" = probabDir)
      enOut <- cbind(enOut,
        "Eneralldire" =
          enOut[, "Energy_Output_Red"] * (enOut[, "probabDir"] / 100)
      )

      # Calculate the sum of the relative Energy outputs
      enOut <- cbind(enOut, "EnergyOverall" = sum(enOut[, "Eneralldire"]))

      # Calculate the sum of the relative Efficiency rates respective to
      # the probability of the wind direction
      enOut <- cbind(enOut,
        "Efficalldire" =
          sum(enOut[, "Parkwirkungsgrad"] * (enOut[, "probabDir"] / 100))
      )

      # Get the total Wake Effect of every Turbine for all Wind directions
      total_wake <- lapply(ee, function(x) {
        x[, "TotAbschProz"]
      })
      total_wake <- do.call("cbind", total_wake)
      total_wake <- rowSums(total_wake)

      # Get the original X / Y - Coordinates of the selected individual
      xundyOrig <- coordsj[, 2:3]

      # Add the Efficieny and the Energy Output of all wind directions and add the total
      # Wake Effect of every Point Location
      # Include the Run of the genertion to the data frame
      xundyOrig <- cbind(xundyOrig,
        "EfficAllDir" = enOut[1, "Efficalldire"],
        "EnergyOverall" = enOut[1, "EnergyOverall"],
        "AbschGesamt" = total_wake,
        "Run" = i
      )

      # Get the Rotor Radius and the Rect_IDs of the park configuration
      ## TODO - rotor radius is already saved outisde the loop and Rect_ID is just dummy
      dt <- ee[[1]]
      dt <- subset.matrix(dt, select = c("RotorR", "Rect_ID"))

      # Bind the Efficiency,Energy,WakeEffect,Run to the Radius and Rect_IDs
      dt <- cbind(xundyOrig,
        dt,
        "bestGARun" = bestGARun
      )

      RandResult[[i]] <- dt
    }
    ## Group list together
    RandResult <- do.call("rbind", RandResult)
    RandResultAll[[o]] <- RandResult
  }
  return(RandResultAll)
}

#' @title Randomize the location of a single turbine
#' @name random_search_single
#' @description Perform a random search for a single turbine, to further
#'   optimize the output of the wind farm layout.
#'
#' @export
#' @inheritParams random_search
#' @param max_dist A numeric value multiplied by the rotor radius to perform
#'   collision checks. Default is 2.2
#' @param turbine Grid cell ID of the turbine to move. If `NULL`, the
#'   function asks interactively.
#'
#' @family Randomization
#' @family Plotting Functions
#' @return Returns a list
random_search_single <- function(result, area, runs = 20, plot = FALSE,
                                 max_dist = 2.2, terrain = NULL,
                                 weibull = NULL, weibull_src = NULL,
                                 ccl = NULL, ccl_roughness = NULL,
                                 turbine = NULL) {
  ## Data Config ############################
  # Order the resulting layouts with highest Energy output
  resldat <- do.call("rbind", result[, "bestPaEn"])
  inp0 <- ga_input_matrix(result)
  maxDist <- as.numeric(inp0["Rotorradius", 1]) * max_dist

  if (plot) {
    plot.new()
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
    par(mfrow = c(1, 1))
  }

  ## Process Data ########
  ## Remove duplicated "Runs", assign do resldat and sort by Energy
  resldat <- as.data.frame(resldat[!duplicated(resldat[, "Run"]), , drop = FALSE])
  resldat$GARun <- 1:nrow(resldat)
  ## Sort by EnergyOverall
  resldat <- resldat[order(resldat[, 4], decreasing = TRUE), ]

  ## Get the GA-run of the best layout
  bestGARun <- resldat$GARun[1]

  inp <- ga_input_matrix(result, bestGARun)
  resolu <- as.numeric(inp["Resolution", 1])
  rotRad <- as.numeric(inp["Rotorradius", 1])
  winddata <- result[bestGARun, ]$inputWind
  ## Get max factor for alteration of coordination
  maxFac <- rotRad * (resolu / (rotRad * 2))

  ## Grid the Polygon ############
  area <- isSpatial(area = area)
  grid_method <- toupper(as.character(inp["Grid Method", 1]))
  if (grid_method != "HEXAGON" && grid_method != "H") {
    # Calculate a Grid and indexed coordinates of all grid cell centers
    propu <- as.numeric(inp["Percentage of Polygon", 1])
    Grid <- grid_area(area = area, size = resolu, prop = propu)
  } else {
    # Calculate a Grid with hexagonal grid cells
    Grid <- hexa_area(area, resolu)
  }

  ## Windata Formatting ###################
  winddata <- windata_format(winddata)
  probabDir <- winddata[[2]]
  winddata <- winddata[[1]]

  phys <- random_search_physics(
    result, area,
    run = bestGARun,
    terrain = terrain, weibull = weibull, weibull_src = weibull_src,
    ccl = ccl, ccl_roughness = ccl_roughness
  )
  if (isTRUE(phys$terrain)) {
    phys$elevation <- terrain_ensure_cells(
      phys$elevation, phys$ccl_raster, Grid[[1]], phys$rotor_height
    )
  }
  ref_height <- phys$ref_height
  rotor_height <- phys$rotor_height
  rotor_radius <- phys$rotor_radius

  max_angle <- getOption("windfarmGA.max_angle")
  max_dist <- getOption("windfarmGA.max_distance")

  ## Turbine Indexing by user input (Must be plotted) ################
  ## Get the starting layout of windfarm[o]
  layout_start <- result[bestGARun, ]$bestPaEn
  layout_ids <- as.character(layout_start[, "Rect_ID"])
  if (is.null(turbine)) {
    plot(sf::st_geometry(Grid[[2]]), reset = FALSE)
    points(x = layout_start[, "X"], y = layout_start[, "Y"], pch = 15)
    calibrate::textxy(
      X = layout_start[, "X"], Y = layout_start[, "Y"],
      labs = layout_start[, "Rect_ID"], cex = 1.5, offset = 0.75
    )
    turbInx <- ""
    while (!as.character(turbInx) %in% layout_ids) {
      message("Enter the turbine number that you want to optimize.")
      message("Please enter the corresponding number:\n")
      turbInx <- readLines(n = 1, con = getOption("windfarmGA.connection"))
    }
  } else if (!as.character(turbine) %in% layout_ids) {
    stop("`turbine` is not a cell ID in the best layout.")
  } else {
    turbInx <- as.character(turbine)
  }
  turbInx <- which(layout_start[, "Rect_ID"] == as.numeric(turbInx))
  coordLay <- cbind(
    X = as.numeric(layout_start[, "X"]),
    Y = as.numeric(layout_start[, "Y"])
  )
  if (plot) {
    plot(sf::st_geometry(Grid[[2]]), reset = FALSE)
    points(coordLay, pch = 15, col = "black")
    points(coordLay[as.numeric(turbInx), ][1],
      coordLay[as.numeric(turbInx), ][2],
      pch = 15, col = "purple"
    )

    legend(
      x = "bottom",
      legend = c(
        "Starting Location", "Selected Turbine", "Randomly generated Location",
        "Suitable Location", "Relocated due to Turbine Collision"
      ),
      col = c("black", "purple", "blue", "green", "red"), lwd = 1, lty = c(0, 0),
      pch = c(15, 15, 3, 20, 20)
    )
  }

  ## Run Random Search  ################
  RandResult <- vector(mode = "list", length = runs)
  for (i in 1:runs) {
    ## Copy the original layout (really need that?)
    coordLayRnd <- coordLay
    ## Get random steps for x/y and add to coords of "problematic" turbine
    maxAlterX <- runif(1, min = -maxFac, max = maxFac)
    maxAlterY <- runif(1, min = -maxFac, max = maxFac)
    cordNew <- coordLay[as.numeric(turbInx), ]
    cordNew[1] <- cordNew[1] + maxAlterX
    cordNew[2] <- cordNew[2] + maxAlterY
    if (plot) {
      points(cordNew[1], cordNew[2], col = "blue", pch = 3)
    }

    ## Assign new coordinates to "problematic" turbine
    coordLayRnd[as.numeric(turbInx), ] <- cordNew

    ## Check if turbines are colliding #####################
    pointsDistBl <- st_as_sf(data.frame(coordLayRnd), coords = c("X", "Y"))
    pointsDist <- st_distance(pointsDistBl)
    distMin <- pointsDist[which(pointsDist < maxDist & pointsDist != 0)]


    while (length(distMin) > 0) {
      pointsDistBl <- st_as_sf(data.frame(coordLayRnd), coords = c("X", "Y"))
      pointsDist <- st_distance(pointsDistBl)
      distMin <- pointsDist[which(pointsDist < maxDist & pointsDist != 0)]
      if (length(distMin) != 0) {
        pointsDist <- data.frame(pointsDist)
        colnames(pointsDist) <- 1:length(pointsDist)

        ## TODO - docs. whats going on here and why
        pointsDist <- round(pointsDist, 2)
        distMin <- round(distMin, 2)
        distMin <- distMin[duplicated(distMin)]

        ## Copy the original layout (really need that? AGAIN ?)
        cordNew <- coordLay[as.numeric(turbInx), ]
        ## Get random steps for x/y and add to coords of "problematic" turbine
        maxAlterX <- runif(1, min = -maxFac, max = maxFac)
        maxAlterY <- runif(1, min = -maxFac, max = maxFac)
        ## Try new random steps
        cordNew[1] <- cordNew[1] + maxAlterX
        cordNew[2] <- cordNew[2] + maxAlterY

        if (plot) {
          points(
            x = cordNew[1],
            y = cordNew[2],
            col = "red", pch = 20
          )
        }

        ## Assign new random steps
        coordLayRnd[as.numeric(turbInx), ] <- cordNew
      }
    }
    if (plot) {
      points(x = cordNew[1], y = cordNew[2], col = "green", pch = 20)
    }
    #####################

    ## Arrange random points to input for calculate_energy
    coordLayRnd <- cbind(coordLayRnd,
      "ID" = as.integer(layout_start[, "Rect_ID"]),
      "bin" = 1
    )
    coordLayRnd <- coordLayRnd[, c("ID", "X", "Y", "bin")]


    # Calculate energy and save in list with length n ################
    resCalcen <- calculate_energy(
      layout = coordLayRnd,
      reference_height = ref_height,
      rotor_height = rotor_height,
      surface_roughness = 0.3, wake_angle = max_angle, wake_distance = max_dist,
      wind = winddata,
      rotor = rotor_radius,
      area = area, terrain = phys$terrain,
      elevation = phys$elevation, ccl_raster = phys$ccl_raster,
      weibull = phys$weibull
    )

    ## Process Data ###################
    ee <- lapply(resCalcen, function(x) {
      subset.matrix(x,
        subset = !duplicated(x[, "Punkt_id"]),
        select = c(
          "Bx", "By", "Windrichtung", "RotorR", "TotAbschProz", "V_New",
          "Rect_ID", "Energy_Output_Red", "Energy_Output_Voll",
          "Parkwirkungsgrad"
        )
      )
    })

    # get Energy Output and Efficiency rate for every wind direction
    enOut <- lapply(ee, function(x) {
      subset.matrix(x,
        subset = c(TRUE, rep(FALSE, length(ee[[1]][, 1]) - 1)),
        select = c("Windrichtung", "Energy_Output_Red", "Parkwirkungsgrad")
      )
    })
    enOut <- do.call("rbind", enOut)

    # Add the Probability of every direction
    # Calculate the relative Energy outputs respective to the probability of the wind direction
    enOut <- cbind(enOut, "probabDir" = probabDir)
    enOut <- cbind(enOut, "Eneralldire" = enOut[, "Energy_Output_Red"] * (enOut[, "probabDir"] / 100))

    # Calculate the sum of the relative Energy outputs
    enOut <- cbind(enOut, "EnergyOverall" = sum(enOut[, "Eneralldire"]))

    # Calculate the sum of the relative Efficiency rates respective to the probability of the
    # wind direction
    enOut <- cbind(enOut, "Efficalldire" = sum(enOut[, "Parkwirkungsgrad"] * (enOut[, "probabDir"] / 100)))

    # Get the total Wake Effect of every Turbine for all Wind directions
    AbschGesamt <- lapply(ee, function(x) {
      x[, "TotAbschProz"]
    })
    AbschGesamt <- do.call("cbind", AbschGesamt)
    AbschGesamt <- rowSums(AbschGesamt)

    # Get the original X / Y - Coordinates of the selected individual
    xundyOrig <- coordLayRnd[, 2:3]

    # Add the Efficieny and the Energy Output of all wind directions and add the total
    # Wake Effect of every Point Location
    # Include the Run of the genertion to the data frame
    xundyOrig <- cbind(xundyOrig,
      "EfficAllDir" = enOut[1, "Efficalldire"],
      "EnergyOverall" = enOut[1, "EnergyOverall"],
      "AbschGesamt" = AbschGesamt,
      "Run" = i
    )


    # Get the Rotor Radius and the Rect_IDs of the park configuration
    dt <- ee[[1]]
    # layout_start
    dt <- subset.matrix(dt, select = c("RotorR", "Rect_ID"))

    # Bind the Efficiency,Energy,WakeEffect,Run to the Radius and Rect_IDs
    dt <- cbind(xundyOrig,
      dt,
      "bestGARun" = bestGARun
    )
    RandResult[[i]] <- dt
  }
  return(RandResult)
}

ga_input_matrix <- function(result, run = 1) {
  cell <- result[run, "inputData"][[1]]
  if (is.list(cell) && is.matrix(cell[[1]])) {
    return(cell[[1]])
  }
  cell
}

ga_flag_true <- function(x) {
  if (is.logical(x) && length(x) == 1L) {
    return(isTRUE(x))
  }
  identical(toupper(trimws(as.character(x))), "TRUE")
}

random_search_physics <- function(result, area, run = 1,
                                  terrain = NULL, weibull = NULL,
                                  weibull_src = NULL, ccl = NULL,
                                  ccl_roughness = NULL) {
  inp <- ga_input_matrix(result, run)
  ref_height <- as.numeric(inp["Reference Height", 1])
  rotor_height <- as.numeric(inp["Rotor Height", 1])
  rotor_radius <- as.numeric(inp["Rotorradius", 1])

  ga_terrain <- FALSE
  if ("Topographie" %in% rownames(inp)) {
    ga_terrain <- ga_flag_true(inp["Topographie", 1])
  }
  ga_weibull <- FALSE
  if ("Active Weibull" %in% rownames(inp)) {
    ga_weibull <- ga_flag_true(inp["Active Weibull", 1])
  }

  if (is.null(terrain)) {
    terrain <- ga_terrain
  }
  elevation <- NULL
  ccl_raster <- NULL
  terrain_on <- !isFALSE(terrain)
  if (terrain_on) {
    td <- terrain_resolve(
      result, terrain, area, ccl, ccl_roughness,
      plot = FALSE, verbose = FALSE
    )
    elevation <- td$srtm_crop
    ccl_raster <- td$cclRaster
    terrain <- TRUE
  }

  weibull_ras <- FALSE
  if (isFALSE(weibull)) {
    weibull_ras <- FALSE
  } else if (inherits(weibull, c("SpatRaster", "RasterLayer", "stars"))) {
    weibull_ras <- if (inherits(weibull, "SpatRaster")) {
      weibull
    } else {
      terra::rast(weibull)
    }
  } else if (!is.null(weibull_src)) {
    weibull_ras <- weibull_speed_raster(weibull_src, area)
  } else if (isTRUE(weibull) || ga_weibull) {
    warning(
      "random_search: Weibull was on in the GA, but weibull_src is missing. ",
      "The rasters are not stored in result. Using the wind rose.",
      call. = FALSE
    )
  }

  list(
    ref_height = ref_height,
    rotor_height = rotor_height,
    rotor_radius = rotor_radius,
    terrain = isTRUE(terrain),
    elevation = elevation,
    ccl_raster = ccl_raster,
    weibull = weibull_ras
  )
}
