#' @title RandomSearchTurb Randomise the location of a single turbine.
#' @name RandomSearchTurb
#' @description Perform a random search for a single turbine, to
#' further optimize the output of the wind farm layout.
#' 
#' @export
#'
#' @importFrom sp SpatialPoints
#' @importFrom calibrate textxy
#' @importFrom raster plot
#' 
#'
#' @param result The resulting matrix of the function 'genAlgo' or
#' 'windfarmGA'.
#' @param Polygon1 The Polygon for the wind farm area. 
#' @param n The number of random searches to be perfomed. Default is 20.
#' @param Plot Should the random serach be plotted? Default is TRUE
#' @param max_dist A numeric value multiplied by the rotor radius to perform
#' collision checks. Default is 2.2
#' @param GridMethod Should the polygon be divided into rectangular or
#' hexagonal grid cells? The default is rectangular grid cells and hexagonal
#' grid cells are computed when assigning "h" or "hexagon" to this input
#' variable. The randomly generated points may also be paced outside of 
#' their hexagon. 
#'
#' @return Returns a list.
#'
#' @examples \donttest{
#' load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
#' 
#' Res = RandomSearchTurb(result = resultrect, Polygon1 = polygon, n = 10)
#' RandomSearchPlot(resultRS = Res, result = resultrect, Polygon1 = polygon, best =2)
#' }
#' @author Sebastian Gatscha
RandomSearchTurb <- function(result, Polygon1, n, Plot, GridMethod, max_dist = 2.2) {
  ## TODO - Performance and structure ---

  ## Missing attrs, Data Config ############################
  # Order the resulting layouts with highest Energy output
  resldat <- do.call("rbind", result[,"bestPaEn"])
  maxDist <- as.numeric(result[,"inputData"][[1]]['Rotorradius',]) * max_dist
  if (missing(Plot)) {
    Plot <- TRUE
  }
  if (missing(n)) {
    n <- 20
  }
  if (missing(GridMethod)) {
    GridMethod <- "Rectangular"
  }
  GridMethod <- toupper(GridMethod)
  if (Plot) {
    plot.new()
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
    par(mfrow = c(1, 1))
  }

  ## Remove duplicated "Runs", assign do resldat and sort by Energy
  resldat <- as.data.frame(resldat[!duplicated(resldat[,'Run']),])
  resldat$GARun <- 1:nrow(resldat)
  resldat <- resldat[order(resldat[, "EnergyOverall"], decreasing = TRUE),]

  ## Get the GA-run of the best layout
  bestGARun <- resldat$GARun[1]
  resolu <- as.numeric(result[bestGARun,]$inputData["Resolution",][1])
  rotRad <- as.numeric(result[bestGARun,]$inputData["Rotorradius",][1])
  propu  <- as.numeric(result[bestGARun,]$inputData["Percentage of Polygon",][1])
  winddata <- result[bestGARun,]$inputWind

  ## Decide if the space division should be rectangular or in hexagons.
  if (GridMethod != "HEXAGON" & GridMethod != "H") {
    # Calculate a Grid and indexed coordinates of all grid cell centers
    Grid <- GridFilter(shape = Polygon1, resol = resolu, 
                       prop = propu, plotGrid = FALSE)
  } else {
    # Calculate a Grid with hexagonal grid cells
    Grid <- HexaTex(Polygon1, resolu / 2)
  }

  ## Get max factor for alteration of coordination
  maxFac <- rotRad * (resolu / (rotRad * 2))
  ##############################

  ## Windata Formatting ###################
  winddata <- windata_format(winddata)
  # winddata <- windfarmGA:::windata_format(winddata)
  
  probabDir <- winddata[[2]]
  winddata <- winddata[[1]]
  ###################

  ## Get the starting layout of windfarm[o]
  layout_start <- result[bestGARun,]$bestPaEn
  
  ### Turbine Indexing (Must be plotted?) ################
  raster::plot(Grid[[2]])
  points(x = layout_start[, "X"], y = layout_start[, "Y"], pch = 15)
  calibrate::textxy(X = layout_start[, "X"], Y = layout_start[, "Y"], 
                    labs = layout_start[, "Rect_ID"], cex = 1.5, offset = 0.75)
  turbInx <- ""

  while (!turbInx %in% layout_start[,'Rect_ID']) {
    cat("Enter the number of the turbine you want to optimize.")
    turbInx <- readline(prompt = "Please enter the corresponding number: ")
  }
  turbInx <- which(layout_start[, 'Rect_ID'] == as.numeric(turbInx))
  ################

  ## Run Random Search  ################
  coordLay <- layout_start[, 1:2]
  if (Plot) {
    raster::plot(Grid[[2]])
    points(coordLay, pch = 15, col = "black")
    points(coordLay[as.numeric(turbInx), ][1],
           coordLay[as.numeric(turbInx), ][2], pch = 15, col = "purple")

    legend( x = "bottom", 
            legend = c("Starting Location", "Selected Turbine", "Randomly generated Location",
                     "Suitable Location", "Relocated due to Turbine Collision"),
            col = c("black", "purple", "blue", "green", "red"), lwd = 1, lty = c(0, 0),
            pch = c(15, 15, 3, 20, 20))
  }

  ## Run n random searches on windfarm[o]
  RandResult <- vector(mode = "list", length = n)
  for (i in 1:n) {
    ## Copy the original layout (really need that?)
    coordLayRnd <- coordLay
    ## Get random steps for x/y and add to coords of "problematic" turbine
    maxAlterX <- runif(1, min = -maxFac, max = maxFac)
    maxAlterY <- runif(1, min = -maxFac, max = maxFac)
    cordNew <- coordLay[as.numeric(turbInx),]
    cordNew[1] <- cordNew[1] + maxAlterX
    cordNew[2] <- cordNew[2] + maxAlterY
    if (Plot) {
      points(cordNew[1], cordNew[2], col = "blue", pch = 3)
    }

    ## Assign new coordinates to "problematic" turbine
    coordLayRnd[as.numeric(turbInx),] <- cordNew 
    # points(coordLayRnd, col="red", pch=20)

    ## Check if turbines are colliding #####################
    pointsDistBl <- sp::SpatialPoints(coordLayRnd)
    pointsDist <- sp::spDists(sp::SpatialPoints(coordLayRnd))
    distMin <- pointsDist[which(pointsDist < maxDist & pointsDist != 0)]


    while (length(distMin) > 0) {
      pointsDistBl <- sp::SpatialPoints(coordLayRnd)
      pointsDist <- sp::spDists(sp::SpatialPoints(coordLayRnd))
      distMin <- pointsDist[which(pointsDist < maxDist & pointsDist != 0)]
      if (length(distMin) != 0) {
        pointsDist <- data.frame(pointsDist)
        colnames(pointsDist) <- 1:length(pointsDist)

        ## TODO - docs. whats going on here and why
        pointsDist <- round(pointsDist, 2) 
        distMin <- round(distMin, 2)
        distMin <- distMin[duplicated(distMin)]


        ## TODO - yeah somethins wrong with the whole loop.. and no docs...
        ColRowMin <- which(pointsDist == distMin, useNames = T, arr.ind = T)

        ## Copy the original layout (really need that? AGAIN ?)
        cordNew <- coordLay[as.numeric(turbInx), ]
        ## Get random steps for x/y and add to coords of "problematic" turbine
        maxAlterX <- runif(1, min = -maxFac, max = maxFac)
        maxAlterY <- runif(1, min = -maxFac, max = maxFac)
        ## Try new random steps
        cordNew[1] <- cordNew[1] + maxAlterX
        cordNew[2] <- cordNew[2] + maxAlterY

        if (Plot) {
          points(x = cordNew[1],
                 y = cordNew[2],
                 col = "red", pch = 20)
        }

        ## Assign new random steps
        coordLayRnd[as.numeric(turbInx),] <- cordNew
      }
    }
    if (Plot) {
      points(x = cordNew[1], y = cordNew[2], col = "green", pch = 20)
      # points(coordLayRnd, col = "green", pch = 20)
    }
    #####################

    ## Arrange random points to input for calculateEn
    coordLayRnd <- cbind(coordLayRnd,
                     "ID" = 1,
                     "bin" = 1)
    coordLayRnd <- coordLayRnd[, c("ID", "X", "Y", "bin")]


    # Calculate energy and save in list with length n ################
    resCalcen <- calculateEn(sel = coordLayRnd,
                             referenceHeight = as.numeric(result[bestGARun,]$inputData[12, ]),
                             RotorHeight = as.numeric(result[bestGARun,]$inputData[13, ]),
                             SurfaceRoughness = 0.3, wnkl = 20, distanz = 100000,
                             resol = resolu, dirSpeed = winddata,
                             RotorR = as.numeric(result[bestGARun,]$inputData[1,]),
                             polygon1 = Polygon1, topograp = FALSE,
                             srtm_crop = NULL, cclRaster = NULL, weibull = FALSE)

    ee  <- lapply(resCalcen, function(x) {
      subset.matrix(x, subset = !duplicated(x[,'Punkt_id']))
    })

    ee  <- lapply(ee, function(x) {
      subset.matrix(x, select = c('Bx','By','Windrichtung','RotorR','TotAbschProz','V_New',
                                  'Rect_ID','Energy_Output_Red', 'Energy_Output_Voll',
                                  'Parkwirkungsgrad'))
    })

    # get Energy Output and Efficiency rate for every wind direction
    enOut <- lapply(ee, function(x) {
      subset.matrix(x,
                    subset = c(TRUE, rep(FALSE, length(ee[[1]][, 1]) - 1)),
                    select = c('Windrichtung', 'Energy_Output_Red', 'Parkwirkungsgrad'))})
    enOut <- do.call("rbind", enOut)

    # Add the Probability of every direction
    # Calculate the relative Energy outputs respective to the probability of the wind direction
    enOut <- cbind(enOut, 'probabDir' = probabDir)
    enOut <- cbind(enOut, 'Eneralldire' = enOut[,'Energy_Output_Red'] * (enOut[,'probabDir'] / 100))

    # Calculate the sum of the relative Energy outputs
    enOut <- cbind(enOut, 'EnergyOverall' = sum(enOut[,'Eneralldire']))

    # Calculate the sum of the relative Efficiency rates respective to the probability of the 
    # wind direction
    enOut <- cbind(enOut, 'Efficalldire' = sum(enOut[,'Parkwirkungsgrad'] * (enOut[,'probabDir'] / 100)))

    # Get the total Wake Effect of every Turbine for all Wind directions
    AbschGesamt <- lapply(ee, function(x){ 
      x[,'TotAbschProz']
    })
    AbschGesamt <- do.call("cbind", AbschGesamt)
    AbschGesamt <- rowSums(AbschGesamt)

    # Get the original X / Y - Coordinates of the selected individual
    xundyOrig <- coordLayRnd[, 2:3]

    # Add the Efficieny and the Energy Output of all wind directions and add the total 
    # Wake Effect of every Point Location
    # Include the Run of the genertion to the data frame
    xundyOrig <- cbind(xundyOrig,
                       'EfficAllDir' = enOut[1, 'Efficalldire'],
                       'EnergyOverall' = enOut[1, 'EnergyOverall'],
                       'AbschGesamt' = AbschGesamt,
                       'Run' = i)


    # Get the Rotor Radius and the Rect_IDs of the park configuration
    dt <-  ee[[1]]
    # layout_start
    dt <- subset.matrix(dt, select = c('RotorR', 'Rect_ID'))

    # Bind the Efficiency,Energy,WakeEffect,Run to the Radius and Rect_IDs
    dt <- cbind(xundyOrig,
                dt,
                "bestGARun" = bestGARun)

    ################

    RandResult[[i]] <- dt
  }
  ################

  return(RandResult)
}
