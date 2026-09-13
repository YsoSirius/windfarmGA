#' @title Evaluate the Individual Fitness values
#' @name fitness
#' @description The fitness of all individuals in the current population
#'   is calculated after their energy output has been evaluated in
#'   \code{\link{calculate_energy}}. This function reduces the resulting energy
#'   outputs to a single fitness value for each individual.
#'
#' @export
#'
#' @inheritParams genetic_algorithm
#' @param population A list of individuals (layouts with X/Y and cell IDs).
#' @param wind Wind data as returned by [windata_format()] (`list(df, probab)`).
#' @param elevation Terrain list from [terrain_model()] (elevation, orography,
#'   roughness). Unused when `terrain` is `FALSE`.
#' @param ccl_raster Land-cover roughness raster from [terrain_model()].
#' @param weibull Raster of estimated wind speeds, or `FALSE`.
#'
#' @family Genetic Algorithm Functions
#' @return Returns a list with every individual, consisting of X & Y
#'   coordinates, rotor radii, the runs and the selected grid cell IDs, and the
#'   resulting energy outputs, efficiency rates and fitness values.
#'
#' @examples \donttest{
#' ## Create a random rectangular shapefile
#' library(sf)
#' area <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)
#'   ))),
#'   crs = 3035
#' ))
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
#' Grid1 <- grid_area(area = area, size = 200, prop = 1)
#' Grid <- Grid1[[1]]
#' AmountGrids <- nrow(Grid)
#'
#' wind <- list(wind, probab = 100)
#' startsel <- init_population(Grid, 10, 20)
#' fit <- fitness(
#'   population = startsel, reference_height = 100, rotor_height = 100,
#'   surface_roughness = 0.3, area = area, rotor = 20,
#'   wind = wind, terrain = FALSE, parallel = FALSE
#' )
#' }
fitness <- function(population, reference_height, rotor_height,
                    surface_roughness, area, rotor, wind,
                    elevation = NULL, terrain = FALSE,
                    ccl_raster = NULL, weibull = FALSE,
                    parallel = FALSE, n_cluster = 2) {
  selection <- population
  Polygon <- area
  rot <- rotor
  dirspeed <- wind
  srtm_crop <- elevation
  cclRaster <- ccl_raster

  ## Wind Data ###########
  probability_direction <- dirspeed[[2]]
  dirspeed <- dirspeed[[1]]

  ## Get maximum angle and maximum distance ###########
  wnkl_max <- getOption("windfarmGA.max_angle")
  dist_max <- getOption("windfarmGA.max_distance")
  bbox_m <- matrix(sf::st_bbox(Polygon), ncol = 2, byrow = FALSE)
  park_center <- apply(bbox_m, 1, mean)

  ## Calculate Energy Output ###########
  # For every selection i and every angle j - in parallel
  if (parallel == TRUE) {
    if (!is_foreach_installed()) {
      stop(
        "The package 'foreach' is required for this function, but it is not installed.\n",
        "Please install it with `install.packages('foreach')`"
      )
    }
    `%dopar%` <- foreach::`%dopar%`
    e <- suppressWarnings(foreach::foreach(
      k = 1:length(selection),
      .packages = "windfarmGA"
    ) %dopar% {
      windfarmGA::calculate_energy(
        layout = selection[[k]], reference_height = reference_height,
        rotor_height = rotor_height, surface_roughness = surface_roughness,
        wake_angle = wnkl_max, wake_distance = dist_max,
        area = Polygon, rotor = rot, wind = dirspeed,
        elevation = srtm_crop, terrain = terrain, ccl_raster = cclRaster,
        weibull = weibull, park_center = park_center
      )
    })
  }

  euniqu <- vector("list", length(selection))
  for (i in 1:length(selection)) {
    if (!parallel) {
      # For every selection i and every angle j - not in parallel
      e <- calculate_energy(
        layout = selection[[i]], reference_height = reference_height,
        rotor_height = rotor_height, surface_roughness = surface_roughness,
        wake_angle = wnkl_max, wake_distance = dist_max,
        area = Polygon, rotor = rot, wind = dirspeed,
        elevation = srtm_crop, terrain = terrain, ccl_raster = cclRaster,
        weibull = weibull, park_center = park_center
      )

      ee <- lapply(e, function(x) {
        subset.matrix(x, subset = !duplicated(x[, "Punkt_id"]))
      })
    } else {
      ## Get a list from unique Grid_ID elements for every park
      ## configuration respective to every winddirection considered.
      ## Since caluclateEn was run over all selections already
      ## we just need to process the result stored in the list e.
      ee <- lapply(e[[i]], function(x) {
        subset.matrix(x, subset = !duplicated(x[, "Punkt_id"]))
      })
    }

    ## TODO - can column selection happen later?
    ## Select only relevant information from list
    ee <- lapply(ee, function(x) {
      subset.matrix(x, select = c(
        "Bx", "By", "Windrichtung", "RotorR",
        "TotAbschProz", "V_New", "Rect_ID",
        "Energy_Output_Red",
        "Energy_Output_Voll",
        "Parkwirkungsgrad"
      ))
    })

    ## get Energy Output and Efficiency rate for every wind direction
    res_energy <- lapply(ee, function(x) {
      subset.matrix(x, subset = c(TRUE, rep(FALSE, length(ee[[1]][, 1]) - 1)))
    })
    res_energy <- do.call("rbind", res_energy)
    res_energy <- res_energy[, c(
      "Windrichtung",
      "Energy_Output_Red",
      "Parkwirkungsgrad"
    ), drop = FALSE]

    # Add the Probability of every direction
    # Calculate the relative Energy outputs respective to the
    # probability of the wind direction
    res_energy <- cbind(res_energy,
      "probability_direction" = probability_direction
    )
    res_energy <- cbind(res_energy,
      "Eneralldire" = res_energy[, "Energy_Output_Red"] *
        (res_energy[, "probability_direction"] / 100)
    )

    # Calculate the sum of the relative Energy outputs
    res_energy <- cbind(res_energy,
      "EnergyOverall" = sum(res_energy[, "Eneralldire"])
    )

    # Calculate the sum of the relative Efficiency rates respective to
    # the probability of the wind direction
    res_energy <- cbind(res_energy,
      "Efficalldire" = sum(
        res_energy[, "Parkwirkungsgrad"] *
          (res_energy[, "probability_direction"] / 100)
      )
    )

    # Get the total Wake Effect of every Turbine for all Wind directions
    total_wake <- lapply(ee, function(x) {
      x[, "TotAbschProz"]
    })
    total_wake <- do.call("cbind", total_wake)
    total_wake <- rowSums(total_wake)

    # Get the original X / Y - Coordinates of the selected individual
    xy_individuals <- selection[[i]][, 2:3, drop = FALSE]

    # Add the Efficieny and the Energy Output of all wind directions and
    # add the total Wake Effect of every Point Location
    # Include the Run of the genertion to the data frame
    xy_individuals <- cbind(xy_individuals,
      "EfficAllDir" = res_energy[1, "Efficalldire"],
      "EnergyOverall" = res_energy[1, "EnergyOverall"],
      "AbschGesamt" = total_wake,
      "Run" = i
    )
    #######################


    ## Get the Rotor Radius and the Rect_IDs of the park configuration
    dt <- subset.matrix(ee[[1]], select = c("RotorR", "Rect_ID"))
    ## Bind the Efficiency,Energy,WakeEffect,Run to the Radius and Rect_IDs
    dt <- cbind(xy_individuals, dt)

    ## Add this information to the the i-th element of the list
    euniqu[[i]] <- dt
  }

  ## Split one from every run and select only Energy information
  maxparkeff <- vapply(euniqu, function(x) {
    energy <- x[1, "EnergyOverall"]
    effic <- x[1, "EfficAllDir"]
    w <- getOption("windfarmGA.fitness_efficiency_weight", 1)
    energy * ((effic / 100)^w)
  }, numeric(1))

  ## Assign every park constellation the Parkfitness Value
  euniqu <- lapply(1:length(euniqu), function(i) {
    cbind(euniqu[[i]], "Parkfitness" = maxparkeff[i])
  })

  names(euniqu) <- unlist(lapply(euniqu, function(i) {
    paste0(i[, "Rect_ID"], collapse = ",")
  }))

  return(euniqu)
}
