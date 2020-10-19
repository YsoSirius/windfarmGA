#' @title Randomize the output of the Genetic Algorithm
#' @name random_search
#' @description Perform a random search in the grid cells, to further optimize
#'   the output of the wind farm layout.
#'
#' @export
#'
#' @param result The resulting matrix of the function \code{\link{genetic_algorithm}}
#' or \code{\link{windfarmGA}}
#' @param Polygon1 The Polygon for the wind farm area.
#' @param best Which best individuals should be the starting conditions for a
#'   random search. The default is 1.
#' @param n The number of random searches to be performed. Default is 20.
#' @param Plot Should the random search be plotted? Default is FALSE
#' @param max_dist A numeric value multiplied by the rotor radius to perform
#'   collision checks. Default is 2.2
#' @param GridMethod Should the polygon be divided into rectangular or hexagonal
#'   grid cells? The default is rectangular grid cells and hexagonal grid cells
#'   are computed when assigning "h" or "hexagon" to this input variable. The
#'   randomly generated points might be placed outside their hexagons.
#'
#' @family Randomization
#' @return Returns a list.
#'
#' @examples \donttest{
#' load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
#'
#' new <- random_search(resultrect, polygon, n = 20, best = 4)
#' plot_random_search(resultRS = new, result = resultrect, Polygon1 = polygon, best = 2)
#' }
random_search <- function(result, Polygon1, n, best, Plot, GridMethod, max_dist = 2.2) {
  # Order the resulting layouts with highest Energy output
  resldat <- do.call("rbind", result[,"bestPaEn"])
  maxDist <- as.numeric(result[,"inputData"][[1]]['Rotorradius',]) * max_dist

  ## Remove duplicated layouts based on x, y energy / efficiency
  resldat <- resldat[!duplicated(resldat[, 1:4]), ]

  ## TODO - Performance and structure ---
  ## Missing arguments ########
  if (missing(Plot)) {
    Plot <- FALSE
  }
  if (missing(n)) {
    n <- 20
  }
  if (missing(best)) {
    best <- 1
  }
  if (missing(GridMethod)) {
    GridMethod <- "Rectangular"
  }

  if (Plot) {
    plot.new()
    opar <- par(no.readonly = TRUE)
    par(mfrow = c(1, 1))
    on.exit(opar)
  }

  ## Process Data ########
  ## Remove duplicated "Runs", assign do resldat and sort by Energy
  resldat <- resldat[!duplicated(resldat[, 'Run']),, drop=FALSE]
  resldat <- as.data.frame(resldat)
  # colnames(resldat) <- c("X", "Y", "EfficAllDir", "EnergyOverall", "AbschGesamt", "Run",
                         # "RotorR", "Rect_ID", "Parkfitness")
  resldat$GARun <- 1:nrow(resldat)
  ## Sort by EnergyOverall / column 4, pass cran tests??
  resldat <- resldat[order(resldat[, 4], decreasing = TRUE),]

  if (best > nrow(resldat)) {
    message(paste0("Only ",nrow(resldat), " unique layouts found. Set 'best' to ",
                   nrow(resldat)))
    best <- nrow(resldat)
  }
  bestGARunIn <- resldat$GARun[1:best]

  resolu <- max(as.numeric(result[bestGARunIn[1],]$inputData["Resolution",][1]))
  rotRad <- max(as.numeric(result[bestGARunIn[1],]$inputData["Rotorradius",][1]))
  propu  <- as.numeric(result[bestGARunIn[1],]$inputData["Percentage of Polygon",][1])
  winddata <- result[bestGARunIn[1],]$inputWind
  ## Get max factor for alteration of coordination
  maxFac <- rotRad * (resolu / (rotRad * 2))

  ## Windata Formatting ###################
  winddata <- windata_format(winddata)
  probabDir <- winddata[[2]]
  winddata <- winddata[[1]]
  ###################
  ## Init arguments ########
  Polygon1 <- isSpatial(shape = Polygon1)

  GridMethod <- toupper(GridMethod)
  ## Decide if the space division should be rectangular or in hexagons.
  if (GridMethod != "HEXAGON" & GridMethod != "H") {
    # Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
    Grid <- grid_area(shape = Polygon1, resol = resolu, prop = propu, 
                       plotGrid = FALSE)
  } else {
    # Calculate a Grid with hexagonal grid cells
    Grid <- hexa_area(Polygon1, resolu / 2)
  }

  ## Get reference /turbine height and rotor radius of 1 individual.
  # TODO - if 3D-wake possible, turbine height must be evaluated in the loop
  ## If different rotor radii, it must also go in the loop
  ref_height <- as.numeric(result[1, ]$inputData[12, ])
  rotor_height <- as.numeric(result[1, ]$inputData[13, ])
  rotor_radius <- as.numeric(result[1,]$inputData[1,])

  RandResultAll <- vector(mode = "list", length = best)
  ########

  ## Optimize Layout in Random Setps
  for (o in 1:best) {
    bestGARun <- bestGARunIn[o]

    ## Get the starting layout of windfarm[o]
    layout_start <- result[bestGARun, ]$bestPaEn
    coordLay <- layout_start[, 1:2]

    if (Plot) {
      raster::plot(Grid[[2]])
      points(coordLay, pch = 15, col = "black")
      
      legend(x = "bottom", 
              legend = c("Starting Location", "Randomly generated Location", 
                       "Suitable Location", "Relocated due to Turbine Collision"),
              col = c("black", "blue","green", "red"), lwd = 1, lty = c(0, 0), 
              pch = c(15, 3, 1, 20))
    }

    ## Run n random searches on windfarm[o]
    RandResult <- vector(mode = "list", length = n)
    for (i in 1:n) {
      coordLayTmp <- vector(mode = "list", length = length(coordLay[,1]))
      ## For every turbine, alter x/y coordinates randomly
      for (j in 1:length(coordLay[,1])) {
        maxAlterX <- runif(1, min = -maxFac, max = maxFac)
        maxAlterY <- runif(1, min = -maxFac, max = maxFac)
        cordNew <- coordLay[j,]
        cordNew[1] <- cordNew[1] + maxAlterX
        cordNew[2] <- cordNew[2] + maxAlterY
        if (Plot) {
          points(cordNew[1], cordNew[2], col = "blue", pch = 3)
        }
        coordLayTmp[[j]] <- cordNew
      }
      coordsj <- do.call("rbind", coordLayTmp)

      ## Check if turbines are not colliding #####################
      pointsDistBl <- sp::SpatialPoints(coordsj)
      pointsDist <- sp::spDists(sp::SpatialPoints(coordsj))
      distMin <- pointsDist[which(pointsDist < maxDist & pointsDist != 0)]

      while (length(distMin) > 0) {
        pointsDistBl <- sp::SpatialPoints(coordsj)
        pointsDist <- sp::spDists(sp::SpatialPoints(coordsj))
        distMin <- pointsDist[which(pointsDist < maxDist & pointsDist != 0)]
        if (length(distMin) != 0) {
          pointsDist <- data.frame(pointsDist)
          colnames(pointsDist) <- 1:length(pointsDist)

          pointsDist <- round(pointsDist, 2)
          distMin <- round(distMin, 2)
          distMin <- distMin[duplicated(distMin)]

          ColRowMin <- which(pointsDist == distMin, 
                             useNames = TRUE, arr.ind = TRUE)

          CoordsWrongOrigin <- coordLay[ColRowMin[1, 2],]

          maxAlterX <- runif(1, min = -maxFac, max = maxFac)
          maxAlterY <- runif(1, min = -maxFac, max = maxFac)
          cordNew <- CoordsWrongOrigin
          cordNew[1] <- cordNew[1] + maxAlterX
          cordNew[2] <- cordNew[2] + maxAlterY

          if (Plot) {
            points(x = coordsj[ColRowMin[1, 2],1],
                   y = coordsj[ColRowMin[1, 2],2],
                   col = "red", cex = 1.2, pch = 20)
          }
          coordsj[ColRowMin[1, 2],] <- cordNew
        }
      }
      if (Plot) {
        points(coordsj, col = "green", cex = 1.5)
      }
      #####################


      ## Arrange random points to input for calculate_energy
      coordsj <- cbind(coordsj,
                       "ID" = 1,
                       "bin" = 1)
      coordsj <- coordsj[, c("ID", "X", "Y", "bin")]

      # Calculate energy and save in list with length n ################
      resCalcen <- calculate_energy(sel = coordsj,
                               referenceHeight = ref_height,
                               RotorHeight = rotor_height,
                               SurfaceRoughness = 0.3,
                               wnkl = 20, distanz = 100000,
                               resol = resolu, dirSpeed = winddata,
                               RotorR = rotor_radius,
                               polygon1 = Polygon1, topograp = FALSE,
                               srtm_crop = NULL, cclRaster = NULL, 
                               weibull = FALSE)

      ## TODO - optimize all next lines (calculate_energy has already beeter method)
      ## Do not need lapply if winddirection == 1
      ee  <- lapply(resCalcen, function(x){
        subset.matrix(x, subset = !duplicated(x[,"Punkt_id"]))
      })

      ## Select only important columns for all winddirections
      ee  <- lapply(ee, function(x){
        subset.matrix(x, select = c("Bx", "By", "Windrichtung", "RotorR", "TotAbschProz",
                                    "V_New", "Rect_ID", "Energy_Output_Red",
                                    "Energy_Output_Voll", "Parkwirkungsgrad"))
      })

      # get Energy Output and Efficiency rate for every wind direction
      enOut <- lapply(ee, function(x){
        subset.matrix(x, 
                      subset = c(TRUE, rep(FALSE, length(ee[[1]][, 1]) - 1)),
                      select = c("Windrichtung", "Energy_Output_Red", 
                                 "Parkwirkungsgrad"))})
      enOut <- do.call("rbind", enOut)

      # Add the Probability of every direction
      # Calculate the relative Energy outputs relative to wind direction probabilities
      enOut <- cbind(enOut, "probabDir" = probabDir)
      enOut <- cbind(enOut, "Eneralldire" = 
                       enOut[, "Energy_Output_Red"] * (enOut[, "probabDir"] / 100))

      # Calculate the sum of the relative Energy outputs
      enOut <- cbind(enOut, "EnergyOverall" = sum(enOut[, "Eneralldire"]))

      # Calculate the sum of the relative Efficiency rates respective to 
      # the probability of the wind direction
      enOut <- cbind(enOut, "Efficalldire" = 
                       sum(enOut[, "Parkwirkungsgrad"] * (enOut[, "probabDir"] / 100)))

      # Get the total Wake Effect of every Turbine for all Wind directions
      total_wake <- lapply(ee, function(x){
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
                         "Run" = i)

      # Get the Rotor Radius and the Rect_IDs of the park configuration
      ## TODO - rotor radius is already saved outisde the loop and Rect_ID is just dummy 
      dt <-  ee[[1]]
      dt <- subset.matrix(dt, select = c("RotorR", "Rect_ID"))

      # Bind the Efficiency,Energy,WakeEffect,Run to the Radius and Rect_IDs
      dt <- cbind(xundyOrig,
                  dt,
                  "bestGARun" = bestGARun)

      RandResult[[i]] <- dt
    }
    ## Group list together
    RandResult <- do.call("rbind", RandResult)
    RandResultAll[[o]] <- RandResult
  }

  return(RandResultAll)
}
