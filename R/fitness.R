#' @title Evaluate the Individual Fitness values
#' @name fitness
#' @description The fitness values of the individuals in the current population
#'   are calculated after having evaluated their energy outputs in
#'   \code{\link{calculate_energy}}. This function reduces the resulting energy
#'   outputs to a single fitness value for every individual.
#'
#' @export
#'
#' @param selection A list containing all individuals of the current population.
#' @param referenceHeight The height at which the incoming wind speeds were
#'   measured.
#' @param RotorHeight The desired height of the turbine.
#' @param SurfaceRoughness A surface roughness length of the considered area in
#'   m.
#' @param Polygon The considered area as shapefile.
#' @param resol1 The resolution of the grid in meter.
#' @param rot The desired rotor radius in meter.
#' @param dirspeed The wind data as list.
#' @param srtm_crop A list of 3 raster, with 1) the elevation, 2) an orographic
#'   and 3) a terrain raster. Calculated in \code{\link{genetic_algorithm}}
#' @param topograp Logical value that indicates whether the terrain effect model
#'   is activated (TRUE) or deactivated (FALSE).
#' @param cclRaster A Corine Land Cover raster, that has to be adapted
#'   previously by hand with the surface roughness length for every land cover
#'   type. Is only used, when the terrain effect model is activated.
#' @param weibull A raster representing the estimated wind speeds
#' @param Parallel Boolean value, indicating whether parallel processing should
#'   be used. The parallel and doParallel packages are used for parallel
#'   processing.
#' @param numCluster If Parallel is TRUE, this variable defines the number of
#'   clusters to be used.
#'
#' @family Genetic Algorithm Functions
#' @return Returns a list with every individual, consisting of X & Y
#'   coordinates, rotor radii, the runs and the selected grid cell IDs, and the
#'   resulting energy outputs, efficiency rates and fitness values.
#'
#' @examples \donttest{
#' ## Create a random rectangular shapefile
#' library(sp)
#' Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
#'                     c(4499991, 2669343), c(4499991, 2668272)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+init=epsg:3035"
#' proj4string(Polygon1) <- CRS(Projection)
#'
#' ## Create a uniform and unidirectional wind data.frame and plots the
#' ## resulting wind rose
#' ## Uniform wind speed and single wind direction
#' wind <- data.frame(ws = 12, wd = 0)
#' # windrosePlot <- plot_windrose(data = wind, spd = wind$ws,
#' #                dir = wind$wd, dirres=10, spdmax=20)
#'
#' ## Calculate a Grid and an indexed data.frame with coordinates and
#' ## grid cell IDs.
#' Grid1 <- grid_area(shape = Polygon1,resol = 200,prop = 1);
#' Grid <- Grid1[[1]]
#' AmountGrids <- nrow(Grid)
#'
#' wind <- list(wind, probab = 100)
#' startsel <- init_population(Grid,10,20);
#' fit <- fitness(selection = startsel, referenceHeight = 100, RotorHeight=100,
#'                SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,rot=20,
#'                dirspeed = wind, srtm_crop="", topograp=FALSE, cclRaster="",
#'                Parallel = FALSE)
#' }
fitness           <- function(selection, referenceHeight, RotorHeight,
                              SurfaceRoughness, Polygon, resol1,
                              rot, dirspeed, srtm_crop, topograp, cclRaster,
                              weibull, Parallel, numCluster) {

  ## Missing Arguments? #############
  if (missing(srtm_crop)) {
    srtm_crop <- NULL
  }
  if (missing(cclRaster)) {
    cclRaster <- NULL
  }
  if (missing(Parallel)) {
    Parallel <- FALSE
  }
  if (missing(weibull)) {
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
  # known <- TRUE
  # if (known) {
  ###################

  # Calculate EnergyOutput for every config i and for
  # every angle j - in Parallel
  if (Parallel == TRUE) {
    e <- foreach::foreach(k = 1:length(selection)) %dopar% {
      windfarmGA::calculate_energy(
        sel = selection[[k]], referenceHeight = referenceHeight,
        RotorHeight = RotorHeight, SurfaceRoughness = SurfaceRoughness,
        wnkl = 20, distanz = 100000,
        polygon1 = Polygon, resol = resol1, RotorR = rot, dirSpeed = dirspeed,
        srtm_crop = srtm_crop, topograp = topograp, cclRaster = cclRaster,
        weibull = weibull)
    }
  }

  euniqu <- vector("list", length(selection))
  for (i in 1:length(selection)) {
    if (!Parallel) {
      # Calculate EnergyOutput for every config i and for
      # every angle j - not Parallel
      e <- calculate_energy(
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

    ## TODO - can column selection happen later?
    ## Select only relevant information from list
    ee  <- lapply(ee, function(x){
      subset.matrix(x, select = c("Bx", "By", "Windrichtung", "RotorR",
                                  "TotAbschProz", "V_New", "Rect_ID",
                                  "Energy_Output_Red",
                                  "Energy_Output_Voll",
                                  "Parkwirkungsgrad"))
    })

    ## get Energy Output and Efficiency rate for every wind direction
    res_energy <- lapply(ee, function(x){
      subset.matrix(x, subset = c(TRUE, rep(FALSE, length(ee[[1]][, 1]) - 1)))
    })
    res_energy <- do.call("rbind", res_energy)
    res_energy <- res_energy[, c("Windrichtung",
                                 "Energy_Output_Red",
                                 "Parkwirkungsgrad"), drop = FALSE]
    
    # Add the Probability of every direction
    # Calculate the relative Energy outputs respective to the
    # probability of the wind direction
    res_energy <- cbind(res_energy, 
                            "probability_direction" = probability_direction)
    res_energy <- cbind(res_energy, 
                            "Eneralldire" = res_energy[, "Energy_Output_Red"] *
                              (res_energy[, "probability_direction"] / 100))

    # Calculate the sum of the relative Energy outputs
    res_energy <- cbind(res_energy, 
                            "EnergyOverall" = sum(res_energy[, "Eneralldire"]))

    # Calculate the sum of the relative Efficiency rates respective to
    # the probability of the wind direction
    res_energy <- cbind(res_energy, 
                        "Efficalldire" = sum(
                          res_energy[, "Parkwirkungsgrad"] *
                            (res_energy[, "probability_direction"] / 100)))
    
    # Get the total Wake Effect of every Turbine for all Wind directions
    total_wake <- lapply(ee, function(x) {
      x[, "TotAbschProz"]
    })
    total_wake <- do.call("cbind", total_wake)
    total_wake <- rowSums(total_wake)

    # Get the original X / Y - Coordinates of the selected individual
    xy_individuals <- selection[[i]][, 2:3]

    # Add the Efficieny and the Energy Output of all wind directions and
    # add the total Wake Effect of every Point Location
    # Include the Run of the genertion to the data frame
    xy_individuals <- cbind(xy_individuals,
                       "EfficAllDir" = res_energy[1, "Efficalldire"],
                       "EnergyOverall" = res_energy[1, "EnergyOverall"],
                       "AbschGesamt" = total_wake,
                       "Run" = i)
    #######################


    ## Get the Rotor Radius and the Rect_IDs of the park configuration
    dt <- subset.matrix(ee[[1]], select = c("RotorR", "Rect_ID"))
    ## Bind the Efficiency,Energy,WakeEffect,Run to the Radius and Rect_IDs
    dt <- cbind(xy_individuals, dt)

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
  # }

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