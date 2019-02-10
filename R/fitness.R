#' @title Evaluate the Individual Fitness values
#' @name fitness
#' @description The fitness values of the individuals in the
#' current population are calculated after having evaluated their energy
#' outputs in \code{\link{calculateEn}}. This function reduces the resulting
#' energy outputs to a single fitness value for every individual.
#'
#' @export
#'
#' @importFrom raster extent rasterize
#' @importFrom foreach foreach %dopar% 
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster
#'
#' @param selection A list containing all individuals of the current
#' population.
#' @param referenceHeight The height at which the incoming wind speeds were
#' measured. 
#' @param RotorHeight The desired height of the turbine. 
#' @param SurfaceRoughness A surface roughness length of the considered area
#' in m. 
#' @param Polygon The considered area as shapefile. 
#' @param resol1 The resolution of the grid in meter.
#' @param rot The desired rotor radius in meter.
#' @param dirspeed The wind data as list.
#' @param srtm_crop A list of 3 raster, with 1) the elevation, 2) an orographic
#' and 3) a terrain raster. Calculated in \code{\link{genAlgo}}
#' @param topograp Logical value that indicates whether the terrain effect
#' model is activated (TRUE) or deactivated (FALSE).
#' @param cclRaster A Corine Land Cover raster, that has to be adapted
#' previously by hand with the surface roughness lenght for every land cover
#' type. Is only used, when the terrain effect model is activated.
#' @param weibull A raster representing the estimated wind speeds
#' @param Parallel Boolean value, indicating whether parallel processing
#' should be used. The parallel and doParallel packages are used for parallel 
#' processing.
#' @param numCluster If Parallel is TRUE, this variable defines the number
#' of clusters to be used.
#'
#' @return Returns a list with every individual, consisting of X & Y
#' coordinates, rotor radii, the runs and the selected grid cell IDs, and
#' the resulting energy outputs, efficiency rates and fitness values.
#'
#' @examples \donttest{
#' ## Create a random rectangular shapefile
#' library(sp)
#' Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
#'                     c(4499991, 2669343), c(4499991, 2668272)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' plot(Polygon1,axes=TRUE)
#'
#' ## Create a uniform and unidirectional wind data.frame and plots the
#' ## resulting wind rose
#' ## Uniform wind speed and single wind direction
#' wind <- data.frame(ws = 12, wd = 0)
#' # windrosePlot <- plotWindrose(data = wind, spd = wind$ws,
#' #                dir = wind$wd, dirres=10, spdmax=20)
#'
#' ## Calculate a Grid and an indexed data.frame with coordinates and
#' ## grid cell IDs.
#' Grid1 <- GridFilter(shape = Polygon1,resol = 200,prop = 1);
#' Grid <- Grid1[[1]]
#' AmountGrids <- nrow(Grid)
#'
#' wind <- list(wind, probab = 100)
#' startsel <- StartGA(Grid,10,20);
#' fit <- fitness(selection = startsel, referenceHeight = 100, RotorHeight=100,
#'                SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,rot=20,
#'                dirspeed = wind, srtm_crop="", topograp=FALSE, cclRaster="",
#'                Parallel = FALSE)
#' head(fit)
#' }
#'
#' @author Sebastian Gatscha
fitness           <- function(selection, referenceHeight, RotorHeight,
                              SurfaceRoughness, Polygon, resol1,
                              rot, dirspeed, srtm_crop, topograp, cclRaster,
                              weibull, Parallel, numCluster) {

  ## Missing Arguments? #############
  if (missing(srtm_crop)){
    srtm_crop <- NULL
  }
  if (missing(cclRaster)){
    cclRaster <- NULL
  }
  if (missing(Parallel)){
    Parallel <- FALSE
  }
  if (missing(weibull)){
    weibull <- FALSE
  }
  #############

  probability_direction <- dirspeed[[2]]
  dirspeed <- dirspeed[[1]]

  ## Layout Saving 1   ###################
  # selconfig <- sapply(selection, function(i) {paste0(i$ID, collapse = ",")})
  # if (exists("globalparks")) {
  #   if (verbose) {
  #     cat("\n")
  #     print("come in here?")
  #     print(length(globalparks))
  #   }
  #   # any(duplicated(selconfig))
  #   if (any(selconfig %in% names(globalparks)) | any(duplicated(selconfig))) {
  #     # browser()
  #     alr_known_ind = which(selconfig %in% names(globalparks))
  #     gP_ind <- which(names(globalparks) %in% selconfig)
  #     alr_known = globalparks[gP_ind]
  #     selection = selection[-alr_known_ind]
  #     cat("\n")
      # cat("Some Layouts are already known. Get values from cached results
      #     instead of recalculating")
  #     # browser()
  #   }
  # }
  ###################

  known <- TRUE
  if (known) {
    # Calculate EnergyOutput for every config i and for
    # every angle j - in Parallel
    if (Parallel == TRUE) {
      e <- foreach::foreach(k = 1:length(selection)) %dopar% {
        windfarmGA::calculateEn(
          sel = selection[[k]], referenceHeight = referenceHeight,
          RotorHeight = RotorHeight, SurfaceRoughness = SurfaceRoughness,
          wnkl = 20, distanz = 100000,
          polygon1 = Polygon, resol = resol1, RotorR = rot, dirSpeed = dirspeed,
          srtm_crop = srtm_crop, topograp = topograp, cclRaster = cclRaster,
          weibull = weibull)
      }
    }

    euniqu <- vector("list", length(selection))
    for (i in 1:length(selection)){
      if (!Parallel) {
        # Calculate EnergyOutput for every config i and for
        # every angle j - not Parallel
        e <- calculateEn(
          sel = selection[[i]], referenceHeight = referenceHeight,
          RotorHeight = RotorHeight, SurfaceRoughness = SurfaceRoughness,
          wnkl = 20, distanz = 100000,
          polygon1 = Polygon, resol = resol1, RotorR = rot, dirSpeed = dirspeed,
          srtm_crop = srtm_crop, topograp = topograp, cclRaster = cclRaster,
          weibull = weibull)

        ee  <- lapply(e, function(x) {
          subset.matrix(x, subset = !duplicated(x[, "Punkt_id"]))
        })

      } else {
        ## Get a list from unique Grid_ID elements for every park
        ## configuration respective to every winddirection considered.
        ## Since caluclateEn was run over all selections already
        ## we just need to process the result stored in the list e.
        ee  <- lapply(e[[i]], function(x) {
          subset.matrix(x, subset = !duplicated(x[, "Punkt_id"]))
        })
      }

      ## Select only relevant information from list
      ee  <- lapply(ee, function(x){
        subset.matrix(x, select = c("Bx", "By", "Windrichtung", "RotorR",
                                    "TotAbschProz", "V_New", "Rect_ID",
                                    "Energy_Output_Red",
                                    "Energy_Output_Voll",
                                    "Parkwirkungsgrad"))
      })

      ## get Energy Output and Efficiency rate for every wind direction
      enOut <- lapply(ee, function(x){
        subset.matrix(x,
                      subset = c(TRUE, rep(FALSE, length(ee[[1]][, 1]) - 1)),
                      select = c("Windrichtung",
                                 "Energy_Output_Red",
                                 "Parkwirkungsgrad"))
      })


      ## TODO - Make this lot easier, vectorize it all
      #######################
      enOut <- do.call("rbind", enOut)

      # Add the Probability of every direction
      # Calculate the relative Energy outputs respective to the
      # probability of the wind direction
      enOut <- cbind(enOut, "probability_direction" = probability_direction)
      enOut <- cbind(enOut, "Eneralldire" = enOut[, "Energy_Output_Red"] *
                       (enOut[, "probability_direction"] / 100))

      # Calculate the sum of the relative Energy outputs
      enOut <- cbind(enOut, "EnergyOverall" = sum(enOut[, "Eneralldire"]))

      # Calculate the sum of the relative Efficiency rates respective to
      # the probability of the wind direction
      enOut <- cbind(
        enOut, "Efficalldire" = sum(enOut[, "Parkwirkungsgrad"] *
                                   (enOut[, "probability_direction"] / 100)))

      # Get the total Wake Effect of every Turbine for all Wind directions
      AbschGesamt <- lapply(ee, function(x) {
        x[, "TotAbschProz"]
      })
      AbschGesamt <- do.call("cbind", AbschGesamt)
      AbschGesamt <- rowSums(AbschGesamt)

      # Get the original X / Y - Coordinates of the selected individual
      xundyOrig <- selection[[i]][, 2:3]

      # Add the Efficieny and the Energy Output of all wind directions and
      # add the total Wake Effect of every Point Location
      # Include the Run of the genertion to the data frame
      xundyOrig <- cbind(xundyOrig,
                         "EfficAllDir" = enOut[1, "Efficalldire"],
                         "EnergyOverall" = enOut[1, "EnergyOverall"],
                         "AbschGesamt" = AbschGesamt,
                         "Run" = i)
      #######################


      ## Get the Rotor Radius and the Rect_IDs of the park configuration
      dt <- subset.matrix(ee[[1]], select = c("RotorR", "Rect_ID"))
      ## Bind the Efficiency,Energy,WakeEffect,Run to the Radius and Rect_IDs
      dt <- cbind(xundyOrig, dt)

      ## Add this information to the the i-th element of the list
      euniqu[[i]] <- dt
    }

    ## Split one from every run and select only Energy information
    maxparkeff <- do.call(rbind, lapply(euniqu, function(x) {
      # subset.matrix(x[1,], select = "EnergyOverall")
      x[1, "EnergyOverall"]
    }))

    ## TODO - Get a better Fitness Function!!!!!
    ## Save as Fitness (Its just a copy of overall Energy Output right now).
    colnames(maxparkeff) <- "Parkfitness"

    ## Assign every park constellation the Parkfitness Value
    euniqu <- lapply(1:length(euniqu), function(i) {
      cbind(euniqu[[i]], "Parkfitness" = maxparkeff[i, ])
    })
  }

  names(euniqu) <- unlist(lapply(euniqu, function(i) {
    paste0(i[, "Rect_ID"], collapse = ",")
  }))
  ## Layout Saving 2   ###################
  # if (exists("globalparks")) {
  #   if (any(selconfig %in% names(globalparks)) | any(duplicated(selconfig))) {
  #     euniqu = c(euniqu, alr_known)
  #   }
  #   globalparks <- c(euniqu[!names(euniqu) %in% names(globalparks)], globalparks)
  #   globalparks <<- globalparks[!duplicated(names(globalparks))]
  #   if(any(duplicated(names(globalparks)))){
  #     cat("some layouts are saved multiple times. Remove before adding to globalparks")
  #   }
  # } else {
  #   globalparks <<- euniqu
  # }
  ###################

  return(euniqu)
}