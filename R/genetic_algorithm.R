#' @title Run a Genetic Algorithm to optimize a wind farm layout
#' @name genetic_algorithm
#' @description Run a Genetic Algorithm to optimize the layout of wind turbines
#'   on a given area. The algorithm works with a fixed amount of turbines, a
#'   fixed rotor radius and a mean wind speed value for every incoming wind
#'   direction.
#'
#' @export
#'
#' @param Polygon1 The considered area as SpatialPolygon, SimpleFeature Polygon
#'   or coordinates as matrix/data.frame
#' @param GridMethod Should the polygon be divided into rectangular or hexagonal
#'   grid cells? The default is `Rectangular` grid. Hexagonal grids
#'   are computed when assigning `h` or `hexagon` to this input variable.
#' @param Rotor The rotor radius in meter
#' @param n The amount of turbines
#' @param fcrR A numeric value used for grid spacing. Default is \code{5}
#' @param referenceHeight The height at which the incoming wind speeds were
#'   measured. Default is \code{RotorHeight}
#' @param RotorHeight The height of the turbine hub
#' @param SurfaceRoughness A surface roughness length in meters.
#'   With the terrain effect model, a surface roughness is calculated for every
#'   grid cell using the elevation and land cover data. Default is \code{0.3}
#' @param sourceCCL The path to the Corine Land Cover raster (.tif). Only
#'   required when the terrain effect model is activated.
#' @param sourceCCLRoughness The source to the adapted Corine Land Cover legend
#'   as .csv file. Only required when terrain effect model is activated. As
#'   default a .csv file within this package (\file{~/extdata}) is taken that
#'   was already adapted manually.
#' @param Proportionality A numeric value used for the grid calculation, as it
#'   determines the percentage a grid cell must overlay the area.
#'   Default is \code{1}
#' @param iteration The number of iterations. Default is \code{20}
#' @param mutr Mutation probability per turbine (swap with an unused cell).
#'   Default is \code{2/n}. Each individual swaps at least
#'   \code{getOption("windfarmGA.min_swaps")} cells (default 1).
#' @param vdirspe A data.frame containing the wind speeds, directions and
#'   probabilities. See \code{\link{windata_format}}.
#' @param topograp Boolean value, which indicates if the terrain effect model
#'   should be enabled or not. Default is \code{FALSE}
#' @param elitism Boolean value, which indicates whether elitism should be
#'   activated or not. If \code{TRUE}, the current best layout is archived
#'   unchanged and each elite produces several mutated copies plus mixes with
#'   weaker layouts (\code{windfarmGA.elite_children} /
#'   \code{windfarmGA.elite_mix}). The elite count starts at \code{nelit} and
#'   rises while refining a stall, then drops by one during a disturbance
#'   pulse. Default is \code{TRUE}
#' @param nelit If \code{elitism} is TRUE, this input determines the amount
#'   of individuals in the elite group. Default is 3
#' @param selstate Determines which selection method is used, "FIX" selects a
#'   constant percentage and "VAR" selects a variable percentage, depending on
#'   the development of the fitness values. Default is "VAR"
#' @param crossPart1 Unused by the combinatorial genome (set crossover).
#'   Kept for API compatibility with the legacy binary operators
#'   \code{\link{crossover}} (`EQU` / `RAN`). Default is \code{"EQU"}
#' @param trimForce If \code{TRUE} the algorithm will use a probabilistic
#'   approach to correct the windfarms to the desired amount of turbines.
#'   If \code{FALSE} the adjustment will be random. Default is \code{FALSE}.
#'   Unused by the combinatorial genome; kept for compatibility with
#'   \code{\link{trimton}}.
#' @param Projection A spatial reference system. Depending on your PROJ-version,
#'   it should either be a numeric `EPSG-code` or a `Proj4-string`.
#'   Default is \code{EPSG:3035}
#' @param weibull A boolean value that specifies whether to take Weibull
#'   parameters into account. If \code{TRUE}, the wind speed values
#'   of \code{vdirspe} are ignored. The algorithm will calculate the mean
#'   wind speed for every wind turbine according to the Weibull parameters.
#'   Default is \code{FALSE}
#' @param weibullsrc A list of Weibull parameter rasters, where the first list
#'   item must be the shape parameter raster `k` and the second item must be the
#'   scale parameter raster `a` of the Weibull distribution. If no list is
#'   given, then rasters included in the package are used instead, which
#'   currently only cover Austria. This variable is only used
#'   if \code{weibull = TRUE}.
#' @param Parallel A boolean value, indicating whether parallel processing
#'   should be used. The *parallel* and *doParallel* packages are used for
#'   parallel processing. Default is \code{FALSE}
#' @param numCluster If \code{Parallel} is TRUE, this variable defines the
#'   number of clusters to be used. Default is \code{2}
#' @param verbose If TRUE it will print information for every generation.
#'   Default is \code{FALSE}
#' @param plotit If TRUE it will plot the best windfarm of every generation.
#'   Default is \code{FALSE}
#'
#' @family Genetic Algorithm Functions
#' @return The result is a matrix with aggregated values per generation; the
#'   best individual regarding energy and efficiency per generation, some fuzzy
#'   control variables per generation, a list of all fitness values per
#'   generation, the amount of individuals after each process, a matrix of all
#'   energy, efficiency and fitness values per generation, the selection and
#'   crossover parameters, a matrix with the generational difference in maximum
#'   and mean energy output, a matrix with the given inputs, a dataframe with
#'   the wind information, the mutation rate per generation and a matrix with
#'   all tested wind farm layouts.
#'
#' @details A terrain effect model can be included in the optimization process.
#'   Therefore, a digital elevation model will be downloaded automatically via
#'   the \code{elevatr::get_elev_raster} function. A land cover raster can also
#'   downloaded automatically from the EEA-website, or the path to a raster file
#'   can be passed to \code{sourceCCL}. The algorithm uses an adapted version of
#'   the Raster legend ("clc_legend.csv"), which is stored in the package
#'   directory \file{~/inst/extdata}. To use other values for the land cover
#'   roughness lengths, insert a column named \strong{"Rauhigkeit_z"} to the
#'   .csv file, assign a surface roughness length to all land cover types. Be
#'   sure that all rows are filled with numeric values and save the file with
#'   \strong{";"} separation. Assign the path of the file to the input variable
#'   \code{sourceCCLRoughness} of this function.
#'
#'   Fitness is \eqn{EnergyOverall \times (EfficAllDir/100)^w} with
#'   \code{w = getOption("windfarmGA.fitness_efficiency_weight")}. Hub-height
#'   wind speeds use a logarithmic profile unless
#'   \code{options(windfarmGA.wind_profile = "power")} restores the legacy
#'   power law. Power uses \code{options(windfarmGA.Cp)} (default 0.45) and
#'   optional cut-in / rated / cut-out speeds. Layouts are encoded as \code{n}
#'   unique grid-cell IDs (set crossover and swap mutation). Selection defaults
#'   to \code{VAR} (percentage follows fitness progress). Mutation, immigrants
#'   and unused-cell injection prefer rarely visited cells. Crossover is spatial
#'   with probability \code{options(windfarmGA.spatial_crossover)} (default 0.5).
#'   Evaluated layouts are cached. A flat global max is not a stop signal.
#'   The run ends at \code{iteration}, or earlier only after
#'   \code{options(windfarmGA.stall_generations)} consecutive generations
#'   with no new layout, no newly visited cell and no new best fitness
#'   (set to \code{0} to disable). Operator rates cycle like seasons, still
#'   only selection / set-crossover / swap-mutation: explore (rates rise on
#'   stall) until \code{options(windfarmGA.refine_min_gen)} (default 18) and
#'   \code{options(windfarmGA.refine_after)} (default 12) generations without
#'   a new max at coverage \eqn{\ge 0.35}; then refine (inject toward 0.15,
#'   mutation toward \code{2/n}, selection toward about 45\%). After
#'   \code{options(windfarmGA.refine_hold)} generations in refine (default 25),
#'   a short disturbance pulse of \code{options(windfarmGA.explore_pulse)}
#'   generations (default 10) raises the same rates even if new maxes are
#'   still trickling in, then refine resumes. Elites get a short local search
#'   each generation: one turbine slides to a neighbouring empty cell
#'   (not a random cell anywhere on the grid). The legacy binary operators
#'   \code{\link{crossover}}, \code{\link{mutation}} and
#'   \code{\link{trimton}} remain available.
#'
#' @examples \dontrun{
#' ## Create a random rectangular shapefile
#' library(sf)
#'
#' Polygon1 <- sf::st_as_sf(sf::st_sfc(
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
#' ## Runs an optimization run for 20 iterations with the
#' ## given shapefile (Polygon1), the wind data.frame (data.in),
#' ## 12 turbines (n) with rotor radii of 30m (Rotor) and rotor height of 100m.
#' result <- genetic_algorithm(
#'   Polygon1 = Polygon1,
#'   n = 12,
#'   vdirspe = data.in,
#'   Rotor = 30,
#'   RotorHeight = 100
#' )
#' plot_windfarmGA(result = result, Polygon1 = Polygon1)
#' }
genetic_algorithm <- function(Polygon1, GridMethod, Rotor, n, fcrR,
                              referenceHeight, RotorHeight, SurfaceRoughness,
                              Proportionality, iteration, mutr, vdirspe,
                              topograp, elitism, nelit, selstate, crossPart1,
                              trimForce, Projection, sourceCCL,
                              sourceCCLRoughness, weibull, weibullsrc,
                              Parallel, numCluster, verbose = FALSE,
                              plotit = FALSE) {

  ## set Graphic Params ###############
  if (plotit) {
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(par(oldpar))
    plot.new()
    graphics::par(ask = FALSE)
  }

  ## MISSING ARGUMENTS ###############
  if (missing(fcrR)) {
    fcrR <- 5
  }
  if (missing(topograp)) {
    topograp <- FALSE
  }
  if (missing(GridMethod)) {
    GridMethod <- "Rectangular"
  }
  if (missing(Parallel)) {
    Parallel <- FALSE
  }
  if (missing(numCluster)) {
    numCluster <- 2
  }
  if (missing(weibull)) {
    weibull <- FALSE
  }
  if (missing(selstate)) {
    selstate <- "VAR"
  }
  if (missing(crossPart1)) {
    crossPart1 <- "EQU"
  }
  if (missing(SurfaceRoughness)) {
    SurfaceRoughness <- 0.3
  }
  if (missing(Proportionality)) {
    Proportionality <- 1
  }
  if (missing(mutr)) {
    mutr <- NULL
  }
  if (missing(elitism)) {
    elitism <- TRUE
  }
  if (missing(nelit)) {
    nelit <- 3
  }
  if (missing(trimForce)) {
    trimForce <- FALSE
  }
  if (missing(RotorHeight)) {
    stop("The variable 'RotorHeight' is not defined. Assign the turbine heights to 'RotorHeight'.")
  }
  if (missing(referenceHeight)) {
    referenceHeight <- RotorHeight
  }
  if (missing(iteration)) {
    iteration <- 20
  }
  if (missing(Projection)) {
    if (utils::compareVersion(sf::sf_extSoftVersion()[[3]], "6") > 0) {
      ProjLAEA <- 3035
    } else {
      ProjLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
    }
  } else {
    ProjLAEA <- Projection
  }
  if (missing(vdirspe)) {
    stop("No Winddata is given.")
  }
  if (missing(n)) {
    stop("The variable 'n' is not defined. Assign the number of turbines to 'n'.")
  }
  if (is.null(mutr)) {
    mutr <- 2 / n
  }
  if (missing(Rotor)) {
    stop("The variable 'Rotor' is not defined. Assign the rotor radius to 'Rotor'.")
  }


  ## INIT VARIABLES 1 #################
  selstate <- toupper(selstate)
  crossPart1 <- toupper(crossPart1)

  ## Is the Polygon Spatial / SF / coordinates - It will transform to sf-Polygons
  Polygon1 <- isSpatial(Polygon1, ProjLAEA)
  if (is.na(st_crs(Polygon1))) {
    stop("The input area is not projected.")
  }

  ## Grid size calculation
  resol2 <- fcrR * Rotor

  ## Max Amount of individuals in the Crossover-Method
  CrossUpLimit <- getOption("windfarmGA.max_population", 300)

  ## Start Parallel Cluster ###############
  ## Is Parallel processing activated? Check the max number of cores and set to max-1 if value exceeds.
  if (Parallel) {
    if (!is_parallel_installed()) {
      stop(
        "The package 'parallel' is required for this function, but it is not installed.\n",
        "Please install it with `install.packages('parallel')`"
      )
    }
    if (!is_doparallel_installed()) {
      stop(
        "The package 'doParallel' is required for this function, but it is not installed.\n",
        "Please install it with `install.packages('doParallel')`"
      )
    }
    if (!is_foreach_installed()) {
      stop(
        "The package 'foreach' is required for this function, but it is not installed.\n",
        "Please install it with `install.packages('foreach')`"
      )
    }
    max_cores <- parallel::detectCores()
    if (numCluster > max_cores) {
      warning("Maximum number of cores is: ", max_cores, "\n'numCluster' will be set to: ", max_cores - 1)
      numCluster <- max_cores - 1
    }
    type_cluster <- "PSOCK"
    cl <- parallel::makeCluster(numCluster, type = type_cluster)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  }

  ## WEIBULL ###############
  ## Is Weibull activated? If no source is given, take values from package
  if (weibull) {
    if (verbose) message("Weibull Distribution is used.")

    if (missing(weibullsrc)) {
      stop(
        "No weibull data is given in `weibullsrc`.\nIt must be a list of 2 rasters:\n",
        "  - shape parameter raster\n", "  - scale parameter raster"
      )
    } else {
      if (verbose) message("Weibull data is used.\n")

      ## Project Shapefile to raster, Crop/Mask and project raster back
      if (!inherits(weibullsrc[[1]], "SpatRaster")) {
        weibullsrc[[1]] <- terra::rast(weibullsrc[[1]])
      }
      if (!inherits(weibullsrc[[2]], "SpatRaster")) {
        weibullsrc[[2]] <- terra::rast(weibullsrc[[2]])
      }
    }
      ## Project shapefile to raster CRS, then crop/mask both Weibull rasters
      shape_project <- st_transform(Polygon1, crs = st_crs(weibullsrc[[2]]))
      weibl_k <- terra::crop(x = weibullsrc[[1]], y = shape_project, mask = TRUE)
      weibl_a <- terra::crop(x = weibullsrc[[2]], y = shape_project, mask = TRUE)

    estim_speed_raster <- weibl_a * gamma(1 + (1 / values(weibl_k)))
    estim_speed_raster <- terra::project(
      estim_speed_raster,
      terra::crs(Polygon1)
    )
  } else {
    estim_speed_raster <- FALSE
  }

  ## CHECK INPUTS ###############
  ## Check if Input Data is correct and prints it out.
  if (crossPart1 != "EQU" && crossPart1 != "RAN") {
    crossPart1 <- readinteger()
  }
  if (selstate != "FIX" && selstate != "VAR") {
    selstate <- readintegerSel()
  }
  topgraphie_text <- topograp
  if (inherits(topograp, "SpatRaster") ||
    inherits(topograp, "RasterLayer") ||
    inherits(topograp, "stars")) {
    topgraphie_text <- TRUE
  }
  inputData <- list(
    Input_Data = rbind(
      "Rotorradius" = Rotor,
      "Number of turbines" = n,
      "Grid Shape Factor" = fcrR,
      "Iterations" = iteration,
      "Mutation Rate" = mutr,
      "Percentage of Polygon" = Proportionality,
      "Topographie" = topgraphie_text,
      "Elitarism" = elitism,
      "Elite count" = nelit,
      "Selection Method" = selstate,
      "Trim Force Method Used" = trimForce,
      "Crossover Method Used" = crossPart1,
      "Reference Height" = referenceHeight,
      "Rotor Height" = RotorHeight,
      "Resolution" = resol2,
      "Parallel Processing" = Parallel,
      "Number Clusters" = numCluster,
      "Active Weibull" = weibull,
      "Grid Method" = GridMethod,
      "Projection" = ProjLAEA
    )
  )

  inputWind <- list(Windspeed_Data = vdirspe)
  if (verbose) {
    print(inputData)
    print(inputWind)
  }

  ## Winddata Formatting #######################
  winddata <- windata_format(vdirspe)

  #######################
  ## Project Polygon ###############
  if (utils::compareVersion(sf::sf_extSoftVersion()[[3]], "6") > 0) {
    if (suppressWarnings(!isTRUE(all.equal(
      st_crs(Polygon1),
      st_crs(ProjLAEA)
    )))) {
      Polygon1 <- sf::st_transform(Polygon1, ProjLAEA)
    }
  } else {
    if (as.character(terra::crs(Polygon1)) != ProjLAEA) {
      Polygon1 <- sf::st_transform(Polygon1, ProjLAEA)
    }
  }

  ## Make GRID ###############
  ## Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
  GridMethod <- toupper(GridMethod)
  ## Decide if the space division should be rectangular or in hexagons.
  if (GridMethod != "HEXAGON" && GridMethod != "H") {
    # Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
    Grid1 <- grid_area(Polygon1, resol2, Proportionality)
    Grid <- Grid1[[1]]
    grid_filtered <- Grid1[[2]]
  } else {
    # Calculate a Grid with hexagonal grid cells
    Grid1 <- hexa_area(Polygon1, resol2)
    Grid <- Grid1[[1]]
    grid_filtered <- Grid1[[2]]
  }
  n_gridcells <- nrow(Grid)
  grid_nbr <- grid_neighbors(Grid)

  ## INIT VARIABLES 2 ###############
  ## Determine the amount of initial individuals and create initial population.
  nStart <- (n_gridcells * n) / iteration
  if (nStart < 100) {
    nStart <- 100
  }
  if (nStart > CrossUpLimit) {
    nStart <- CrossUpLimit
  }
  nStart <- ceiling(nStart)
  startsel <- init_population(Grid, n, nStart)
  ## Initialize all needed variables as list.
  maxParkwirkungsg <- 0
  allparkcoeff <- vector("list", iteration)
  bestPaEn <- vector("list", iteration)
  bestPaEf <- vector("list", iteration)
  fuzzycontr <- vector("list", iteration)
  fitnessValues <- vector("list", iteration)
  nindiv <- vector("list", iteration)
  clouddata <- vector("list", iteration)
  selcross <- vector("list", iteration)
  beorwor <- vector("list", iteration)
  mut_rate <- vector("list", iteration)
  allCoords <- vector("list", iteration)

  ## TERRAIN EFFECT MODEL ###############
  ## Checks if terrain effect model is activated, and makes necessary caluclations.
  if (isFALSE(topograp)) {
    if (verbose) {
      message("Topography and orography are not taken into account.")
    }
    srtm_crop <- ""
    cclRaster <- ""
  } else {
    terrain_data <- terrain_model(topograp, Polygon1, sourceCCL, sourceCCLRoughness, plotit, verbose)
    srtm_crop <- terrain_data$srtm_crop
    cclRaster <- terrain_data$cclRaster
    topograp <- TRUE
  }


  ## GENETIC ALGORITHM #################
  if (verbose) {
    message("\nStart Genetic Algorithm ...")
  }
  rbPal <- grDevices::colorRampPalette(c("red", "green"))
  mut_adapt <- mutr
  p_inj <- getOption("windfarmGA.crossover_inject", 0.25)
  mut_floor <- max(1 / n, mutr * 0.5)
  mut_ceil <- min(0.15, max(mutr, 3 / n))
  mut_target <- mutr
  ga_phase <- "explore"
  ga_phase_age <- 0L
  refine_after <- as.integer(getOption("windfarmGA.refine_after", 12L))
  refine_min_gen <- as.integer(getOption("windfarmGA.refine_min_gen", 18L))
  refine_hold <- as.integer(getOption("windfarmGA.refine_hold", 25L))
  explore_pulse <- as.integer(getOption("windfarmGA.explore_pulse", 10L))
  fit_cache <- new.env(parent = emptyenv())
  visit <- stats::setNames(integer(nrow(Grid)), as.character(Grid[, "ID"]))
  best_so_far <- -Inf
  stall <- 0L
  idle <- 0L
  stall_limit <- as.integer(getOption("windfarmGA.stall_generations", 40L))
  eval_fit <- function(selection) {
    fitness_with_cache(
      cache = fit_cache,
      selection = selection,
      referenceHeight = referenceHeight,
      RotorHeight = RotorHeight,
      SurfaceRoughness = SurfaceRoughness,
      Polygon = Polygon1,
      resol1 = resol2,
      rot = Rotor,
      dirspeed = winddata,
      srtm_crop = srtm_crop,
      topograp = topograp,
      cclRaster = cclRaster,
      weibull = estim_speed_raster,
      Parallel = Parallel,
      numCluster = numCluster
    )
  }
  i <- 1
  while (i <= iteration) {
    if (!verbose) {
      message(".", appendLF = FALSE)
    }
    used_at_start <- sum(visit > 0)
    n_new_ls <- 0L
    ## FITNESS (and get_grids) ###############
    if (i == 1) {
      fit <- eval_fit(startsel)
    } else {
      getRectV <- get_grids(mut1, Grid)
      fit <- eval_fit(getRectV)
    }
    n_new_pop <- attr(fit, "n_new")
    if (is.null(n_new_pop)) {
      n_new_pop <- 0L
    }

    ## Fitness Result Processing ###############
    allparks <- do.call("rbind", fit)
    allparksUni <- subset.matrix(allparks,
      subset = !duplicated(allparks[, "Run"])
    )

    allCoords[[i]] <- allparks
    maxparkfitness <- round(max(allparksUni[, "Parkfitness"]), 4)
    meanparkfitness <- round(mean(allparksUni[, "Parkfitness"]), 3)
    minparkfitness <- round(min(allparksUni[, "Parkfitness"]), 3)
    MaxEnergyRedu <- round(max(allparksUni[, "EnergyOverall"]), 2)
    MeanEnergyRedu <- round(mean(allparksUni[, "EnergyOverall"]), 2)
    MinEnergyRedu <- round(min(allparksUni[, "EnergyOverall"]), 2)
    maxParkwirkungsg <- round(max(allparksUni[, "EfficAllDir"]), 2)
    meanParkwirkungsg <- round(mean(allparksUni[, "EfficAllDir"]), 2)
    minParkwirkungsg <- round(min(allparksUni[, "EfficAllDir"]), 2)
    allparkcoeff[[i]] <- cbind(
      maxparkfitness, meanparkfitness, minparkfitness,
      MaxEnergyRedu, MeanEnergyRedu, MinEnergyRedu,
      maxParkwirkungsg, meanParkwirkungsg, minParkwirkungsg
    )

    tb_vis <- table(as.integer(allparks[, "Rect_ID"]))
    nm_vis <- as.character(as.integer(names(tb_vis)))
    hit_vis <- nm_vis %in% names(visit)
    visit[nm_vis[hit_vis]] <- visit[nm_vis[hit_vis]] + as.integer(tb_vis)[hit_vis]
    if (maxparkfitness > best_so_far + 1e-8) {
      best_so_far <- maxparkfitness
      stall <- 0L
    } else {
      stall <- stall + 1L
    }

    clouddata[[i]] <- subset.matrix(allparksUni,
      select = c(
        "EfficAllDir",
        "EnergyOverall",
        "Parkfitness"
      )
    )

    if (verbose) {
      message(c(
        "\n\n", i, ": Round. Max Energy ", allparkcoeff[[i]][, "MaxEnergyRedu"],
        " W and Efficiency ", allparkcoeff[[i]][, "maxParkwirkungsg"], " %"
      ))
    }

    ## Highest Energy Output
    xd <- max(allparks[, "EnergyOverall"])
    ind <- allparks[, "EnergyOverall"] == xd
    bestPaEn[[i]] <- allparks[ind, ][1:n, , drop = FALSE]
    ## Highest Efficiency
    xd1 <- max(allparks[, "EfficAllDir"])
    ind1 <- allparks[, "EfficAllDir"] == xd1
    bestPaEf[[i]] <- allparks[ind1, ][1:n, , drop = FALSE]

    # Print out most relevant information on Generation i
    afvs <- allparks[allparks[, "EnergyOverall"] == max(
      allparks[, "EnergyOverall"]
    ), ]
    if (verbose) {
      message(paste("How many individuals exist: ", length(fit)))
      message(paste(
        "How many parks are in local Optimum: ",
        (length(afvs[, 1]) / n)
      ))
    }
    nindivfit <- length(fit)

    if (plotit) {
      lebre <- length(unique(bestPaEn[[i]][, "AbschGesamt"]))
      if (lebre < 2) {
        Col <- "green"
      } else {
        Col <- rbPal(lebre)[as.numeric(cut(-bestPaEn[[i]][, "AbschGesamt"],
          breaks = lebre
        ))]
      }
      lebre2 <- length(unique(bestPaEf[[i]][, "AbschGesamt"]))
      if (lebre2 < 2) {
        Col1 <- "green"
      } else {
        Col1 <- rbPal(lebre2)[as.numeric(cut(-bestPaEf[[i]][, "AbschGesamt"],
          breaks = lebre2
        ))]
      }
    }

    x <- round(bestPaEn[[i]][, "EnergyOverall"][[1]], 2)
    y <- round(bestPaEn[[i]][, "EfficAllDir"][[1]], 2)
    e <- bestPaEn[[i]][, "EfficAllDir"]
    x1 <- round(bestPaEf[[i]][, "EnergyOverall"][[1]], 2)
    y1 <- round(bestPaEf[[i]][, "EfficAllDir"][[1]], 2)
    e1 <- bestPaEf[[i]][, "EfficAllDir"]
    ##################

    if (plotit) {
      graphics::par(mfrow = c(1, 2))
      plot(st_geometry(Polygon1),
        col = "lightblue",
        main = paste(
          i, "Round \n Best Energy Output: ", x,
          "W/h \n Efficiency: ", y, "%"
        ),
        sub = paste("\n Number of turbines: ", length(e))
      )
      plot(grid_filtered, add = TRUE)
      graphics::points(bestPaEn[[i]][, "X"], bestPaEn[[i]][, "Y"],
        col = Col, pch = 20, cex = 1.5
      )
      plot(st_geometry(Polygon1),
        col = "lightblue",
        main = paste(
          i, "Round \n Best Efficiency Output: ",
          x1, "W/h \n Efficiency: ", y1, "%"
        ),
        sub = paste("\n Number of turbines: ", length(e1))
      )
      plot(grid_filtered, add = TRUE)
      graphics::points(bestPaEf[[i]][, "X"], bestPaEf[[i]][, "Y"],
        col = Col1, pch = 20, cex = 1.5
      )
    }

    ## Fuzzy Control ###############
    if (i > 20) {
      besPE <- do.call("rbind", lapply(bestPaEn[1:i], function(x) {
        max(x[, "EnergyOverall"])
      }))
      maxBisher <- max(besPE)
      WhichMaxBs <- which(besPE == max(besPE))

      if (length(WhichMaxBs) >= 2) {
        BestForNo <- bestPaEn[sample(WhichMaxBs, 2)]
        BestForNo[[1]][, "Run"] <- length(fit) + 1
        BestForNo[[2]][, "Run"] <- length(fit) + 2
      } else {
        BestForNo <- bestPaEn[WhichMaxBs]
        BestForNo <- append(BestForNo, BestForNo)
        BestForNo[[1]][, "Run"] <- length(fit) + 1
        BestForNo[[2]][, "Run"] <- length(fit) + 2
      }

      last7 <- besPE[i:(i - 5)]
      if (!any(last7 == maxBisher)) {
        if (verbose) {
          message(paste(
            "Park with highest Fitness level to date ",
            "is replaced in the list.", "\n"
          ))
        }
        fit <- append(fit, BestForNo)
      }
    }
    if (i == 1) {
      ## TODO I do have such a matrix already with that info or??
      t0 <- subset.matrix(allparks, !duplicated(allparks[, "Run"]))
      t0 <- t0[, "Parkfitness"]
      fitnessValues[[i]] <- t0
      rangeFitnessVt0 <- range(t0)
      maxt0 <- max(t0)
      meant0 <- mean(t0)
      allcoef0 <- c(rangeFitnessVt0, meant0)
      fuzzycontr[[i]] <- rbind(allcoef0)
      colnames(fuzzycontr[[i]]) <- c("Min", "Max", "Mean")
      teil <- 2
      if (selstate == "VAR") {
        teil <- 1.8
      }
      beorwor[[i]] <- cbind(0, 0)
    }
    ## Seasonal rates: explore, refine, then a disturbance pulse if refine lasts.
    if (i >= 2 && i <= iteration) {
      t0 <- subset.matrix(allparks, !duplicated(allparks[, "Run"]))
      t0 <- t0[, "Parkfitness"]
      fitnessValues[[i]] <- t0
      rangeFitnessVt0 <- range(t0)
      meant0 <- mean(t0)
      t1 <- fitnessValues[[i - 1]]
      rangeFitnessVt1 <- range(t1)
      meant1 <- mean(t1)
      n_used <- length(unique(as.integer(allparks[, "Rect_ID"])))
      coverage <- n_used / n_gridcells
      allcoef1 <- c(rangeFitnessVt0, meant0)
      allcoef2 <- c(rangeFitnessVt1, meant1)
      fuzzycontr[[i]] <- rbind(allcoef1, allcoef2)
      colnames(fuzzycontr[[i]]) <- c("Min", "Max", "Mean")

      rates <- adapt_operator_rates(
        phase = ga_phase,
        stall = stall,
        coverage = coverage,
        generation = i,
        teil = teil,
        mut_adapt = mut_adapt,
        p_inj = p_inj,
        mut_floor = mut_floor,
        mut_ceil = mut_ceil,
        mut_target = mut_target,
        refine_after = refine_after,
        refine_min_gen = refine_min_gen,
        phase_age = ga_phase_age,
        refine_hold = refine_hold,
        explore_pulse = explore_pulse
      )
      ga_phase <- rates$phase
      ga_phase_age <- rates$phase_age
      pri <- rates$pri
      teil <- rates$teil
      mut_adapt <- rates$mut_adapt
      p_inj <- rates$p_inj

      if (teil <= 4 / 3 + 1e-8 && verbose) {
        message(paste("Max 75% selected. SP: ", teil))
      }
      if (length(fit) <= 20) {
        teil <- 1
        if (verbose) {
          message(paste("Less than 20 individuals. Select all. SP: ", teil))
        }
      }

      if (verbose) {
        message(paste(
          "Fitness of this population (", i, "), ", pri,
          ". phase=", ga_phase, " age=", ga_phase_age,
          " stall=", stall, " inject=", p_inj,
          " mut=", mut_adapt, " sel=", round(100 / teil, 1),
          "% coverage=", round(coverage, 2)
        ))
      }
      beorwor[[i]] <- cbind(max(t0) - max(t1), coverage)
    }

    ## SELECTION #################
    if (selstate == "FIX") {
      if (teil == 1) {
        teil <- 1
      } else {
        teil <- 2
      }
    }

    p_i <- p_inj
    mut_i <- mut_adapt
    n_elite_used <- 0L
    if (isTRUE(elitism)) {
      n_elite_used <- adapt_elite_n(nelit, length(fit), ga_phase, stall)
    }

    selec6best <- selection(
      fit = fit, Grid = Grid, teil = teil,
      elitism = elitism, nelit = max(1L, n_elite_used),
      selstate = selstate, verbose = verbose
    )

    ids_sel <- selec6best[[1]]
    if (verbose) {
      message(paste(
        "Selection  -  Amount of Individuals: ",
        ncol(ids_sel)
      ))
    }
    nindivsel <- ncol(ids_sel)

    ## CROSSOVER #################
    crossOut <- set_crossover(
      ids = ids_sel, grid_ids = Grid[, "ID"],
      uplimit = CrossUpLimit, verbose = verbose, seed = NULL,
      p_inject = p_i, grid_xy = Grid, visit = visit
    )
    if (verbose) {
      message(paste(
        "Crossover  -  Amount of Individuals: ",
        ncol(crossOut)
      ))
    }
    nindivcros <- ncol(crossOut)
    selcross[[i]] <- cbind(cross = round(p_i, 4), teil)

    ## MUTATION #################
    mut <- swap_mutation(
      ids = crossOut, grid_ids = Grid[, "ID"], p = mut_i, visit = visit
    )
    mut_rate[[i]] <- mut_i
    if (verbose) {
      message(paste("Mutation   -  Amount of Individuals: ", ncol(mut),
                    " p=", mut_i))
    }
    nindivmut <- ncol(mut)

    mut1 <- mut
    n_imm <- as.integer(getOption("windfarmGA.immigrants", 3L))
    if (n_imm > 0) {
      gids <- as.integer(Grid[, "ID"])
      imm_w <- 1 / (1 + as.numeric(visit[as.character(gids)]))
      imm_w[!is.finite(imm_w)] <- 1
      if (sum(imm_w) <= 0) {
        imm_w[] <- 1
      }
      imm <- vapply(seq_len(n_imm), function(k) {
        sort(sample(gids, n, prob = imm_w))
      }, integer(n))
      if (!is.matrix(imm)) {
        imm <- matrix(imm, nrow = n)
      }
      mut1 <- cbind(mut1, imm)
    }

    ## Archive the best; breed extra children from the elite
    n_elite_kids <- 0L
    if (elitism && n_elite_used > 0L) {
      n_elite <- min(n_elite_used, length(fit))
      fit_order <- order(vapply(fit, function(x) x[1, "Parkfitness"], 1),
                         decreasing = TRUE)
      elite_ids <- vapply(fit_order[seq_len(n_elite)], function(idx) {
        sort(as.integer(fit[[idx]][, "Rect_ID"]))
      }, integer(n))
      if (!is.matrix(elite_ids)) {
        elite_ids <- matrix(elite_ids, nrow = n)
      }
      worse_ids <- NULL
      rest <- fit_order[-seq_len(n_elite)]
      if (length(rest)) {
        half <- rest[seq.int(from = max(1L, ceiling(length(rest) / 2)),
                             to = length(rest))]
        worse_ids <- vapply(half, function(idx) {
          sort(as.integer(fit[[idx]][, "Rect_ID"]))
        }, integer(n))
        if (!is.matrix(worse_ids)) {
          worse_ids <- matrix(worse_ids, nrow = n)
        }
      }
      n_mut_el <- as.integer(getOption("windfarmGA.elite_children", 3L))
      n_mix_el <- as.integer(getOption("windfarmGA.elite_mix", 2L))
      if (stall >= 10L) {
        n_mut_el <- n_mut_el + 2L
      }
      kids <- elite_offspring(
        elite_ids, worse_ids, Grid[, "ID"],
        n_mut = n_mut_el, n_mix = n_mix_el,
        mut_p = max(mut_i, 1 / n)
      )
      if (!is.null(kids) && ncol(as.matrix(kids)) > 0) {
        n_elite_kids <- ncol(as.matrix(kids))
      }
      mut1 <- cbind(mut1, elite_ids[, 1, drop = FALSE], kids)
    }

    n_ls <- as.integer(getOption("windfarmGA.local_search_elites", 5L))
    n_try <- as.integer(getOption("windfarmGA.local_search_tries", 6L))
    if (stall >= 5L) {
      n_try <- n_try + 2L
    }
    if (stall >= 15L) {
      n_try <- n_try + 2L
    }
    n_ls <- min(n_ls, length(fit))
    if (n_ls > 0 && n_try > 0) {
      fo <- order(vapply(fit, function(x) x[1, "Parkfitness"], 1),
                  decreasing = TRUE)
      ls_mat <- NULL
      for (e in seq_len(n_ls)) {
        cur <- as.integer(fit[[fo[e]]][, "Rect_ID"])
        best_f <- fit[[fo[e]]][1, "Parkfitness"]
        best_ids <- cur
        for (t in seq_len(n_try)) {
          cand <- matrix(
            neighbor_swap(best_ids, grid_nbr, n_moves = 1L),
            ncol = 1
          )
          fnew <- eval_fit(get_grids(cand, Grid))
          n_ls_new <- attr(fnew, "n_new")
          if (!is.null(n_ls_new)) {
            n_new_ls <- n_new_ls + as.integer(n_ls_new)
          }
          pf <- fnew[[1]][1, "Parkfitness"]
          if (isTRUE(pf > best_f)) {
            best_f <- pf
            best_ids <- as.integer(cand[, 1])
          }
        }
        ls_mat <- cbind(ls_mat, sort(best_ids))
      }
      mut1 <- cbind(mut1, ls_mat)
    }

    n_before <- ncol(mut1)
    if (n_before > 1) {
      keys <- apply(mut1, 2, function(x) paste(sort(x), collapse = ","))
      mut1 <- mut1[, !duplicated(keys), drop = FALSE]
    }
    n_dup <- n_before - ncol(mut1)

    if (verbose) {
      message(paste(
        "Population -  Amount of Individuals: ",
        ncol(mut1), " duplicates dropped: ", n_dup,
        " elites: ", n_elite_used
      ))
    }

    n_cells_now <- length(unique(as.integer(allparks[, "Rect_ID"])))
    n_cells_elite <- NA_real_
    n_take <- min(max(0L, as.integer(n_elite_used)), nrow(allparksUni))
    if (n_take > 0L) {
      ord_el <- order(allparksUni[, "Parkfitness"], decreasing = TRUE)
      top_run <- allparksUni[ord_el[seq_len(n_take)], "Run"]
      n_cells_elite <- length(unique(as.integer(
        allparks[allparks[, "Run"] %in% top_run, "Rect_ID"]
      )))
    }
    nindiv[[i]] <- cbind(
      evaluated = nindivfit,
      selected = nindivsel,
      crossover = nindivcros,
      mutated = nindivmut,
      duplicates = n_dup,
      elites = n_elite_used,
      elite_kids = n_elite_kids,
      cells = n_cells_now,
      cells_elite = n_cells_elite,
      cells_cum = sum(visit > 0)
    )

    n_used <- sum(visit > 0)
    if (stall == 0L || (n_new_pop + n_new_ls) > 0 || n_used > used_at_start) {
      idle <- 0L
    } else {
      idle <- idle + 1L
    }
    if (stall_limit > 0 && idle >= stall_limit) {
      if (verbose) {
        message(
          "Stop: no new layouts or cells for ", idle,
          " generations (max still ", best_so_far, ")."
        )
      }
      break
    }
    i <- i + 1
  }

  ## Reduce list, if algorithm didnt run all iterations #################
  mut_rate <- mut_rate[lapply(mut_rate, length) != 0]
  beorwor <- beorwor[lapply(beorwor, length) != 0]
  selcross <- selcross[lapply(selcross, length) != 0]
  clouddata <- clouddata[lapply(clouddata, length) != 0]
  allparkcoeff <- allparkcoeff[lapply(allparkcoeff, length) != 0]
  bestPaEn <- bestPaEn[lapply(bestPaEn, length) != 0]
  bestPaEf <- bestPaEf[lapply(bestPaEf, length) != 0]
  fuzzycontr <- fuzzycontr[lapply(fuzzycontr, length) != 0]
  fitnessValues <- fitnessValues[lapply(fitnessValues, length) != 0]
  nindiv <- nindiv[lapply(nindiv, length) != 0]
  allCoords <- allCoords[lapply(allCoords, length) != 0]

  ## Bind the results together and Output them. #################
  alldata <- cbind(
    allparkcoeff, bestPaEn, bestPaEf,
    fuzzycontr, fitnessValues, nindiv,
    clouddata, selcross, beorwor,
    inputData, inputWind, mut_rate, allCoords
  )

  return(alldata)
}

#' @title Transform to Simple Feature Polygons
#' @name isSpatial
#' @description Helper Function, which transforms SpatialPolygons or coordinates
#'   in matrix/data.frame - form to a Simple Feature Polygon
#'
#' @export
#'
#' @param shape An area as SpatialPolygon, SimpleFeature Polygon or coordinates
#'   as matrix/data.frame
#' @param proj Which Projection should be assigned to matrix / data.frame
#'   coordinates
#'
#' @family Helper Functions
#' @return A Simple Feature Polygon
#'
#' @details If the columns are named, it will look for common abbreviation to
#'   match x/y or long/lat columns. If the columns are not named, the first 2
#'   numeric columns are taken.
#'
#' @examples \donttest{
#' library(sf)
#' df <- rbind(
#'   c(4498482, 2668272), c(4498482, 2669343),
#'   c(4499991, 2669343), c(4499991, 2668272)
#' )
#' isSpatial(df)
#'
#' Polygon1 <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)
#'   ))),
#'   crs = 3035
#' ))
#' isSpatial(st_coordinates(Polygon1), 3035)
#' }
isSpatial <- function(shape, proj) {
  if (inherits(shape, "Spatial")) {
    shape <- st_as_sf(shape)
    ## This is needed for grid_area. Attribute names must have same length
    shape$names <- "layer"
  } else if (class(shape)[1] == "data.frame" ||
             class(shape)[1] == "matrix") {
    ## If coordinate names are found, take those columns,
    ## otherwise take the first 2
    if (length(colnames(shape))) {
      accep_cols_x <- c("L*N", "X")
      accep_cols_y <- c("L*T", "Y", "BREITE")
      sum_col_match <- sum(sapply(
        c(accep_cols_x, accep_cols_y), grepl,
        toupper(colnames(shape))
      ))
      if (sum_col_match >= 2) {
        x_col_match <- which(sapply(
          lapply(accep_cols_x, grepl, toupper(colnames(shape))),
          any
        ))
        y_col_match <- which(sapply(
          lapply(accep_cols_y, grepl, toupper(colnames(shape))),
          any
        ))

        x_col_index <- which(grepl(
          accep_cols_x[x_col_match],
          toupper(colnames(shape))
        ))
        y_col_index <- which(grepl(
          accep_cols_y[y_col_match],
          toupper(colnames(shape))
        ))

        pltm <- shape[, c(x_col_index[1], y_col_index[1])]
      } else {
        col_numeric <- which(sapply(shape[1, ], is.numeric))
        pltm <- shape[, col_numeric]
      }
    } else {
      col_numeric <- which(sapply(shape[1, ], is.numeric))
      pltm <- shape[, col_numeric]
    }
    colnames(pltm) <- c("x", "y")
    pltm <- data.frame(pltm)
    shape <- st_cast(st_combine(
      sf::st_as_sf(pltm, coords = c("x", "y"))$geometry
    ), "POLYGON")

    if (!missing(proj)) {
      if (is.character(proj)) {
        epsg_match <- regmatches(
          proj, regexpr("(?i)epsg:([0-9]+)", proj, perl = TRUE)
        )
        if (length(epsg_match) && nzchar(epsg_match)) {
          proj <- as.integer(sub("(?i)epsg:", "", epsg_match, perl = TRUE))
        }
      }
      st_crs(shape) <- proj
    }
  }
  return(shape)
}

#' @title Transform Winddata
#' @name windata_format
#' @description Helper Function, which transforms winddata to an acceptable
#'   format
#'
#' @export
#'
#' @param df The wind data with speeds, direction and optionally a probability
#'   column. If not assigned, it will be calculated
#'
#' @family Helper Functions
#' @return A list of windspeed and probabilities
#'
#' @examples \donttest{
#' wind_df <- data.frame(
#'   ws = c(12, 30, 45),
#'   wd = c(0, 90, 150),
#'   probab = 30:32
#' )
#' windata_format(wind_df)
#'
#' wind_df <- data.frame(
#'   speed = c(12, 30, 45),
#'   direction = c(90, 90, 150),
#'   probab = c(10, 20, 60)
#' )
#' windata_format(wind_df)
#'
#' wind_df <- data.frame(
#'   speed = c(12, 30, 45),
#'   direction = c(400, 90, 150)
#' )
#' windata_format(wind_df)
#' }
windata_format <- function(df) {
  wind_df <- data.frame(df)
  if (!all(colnames(wind_df) %in% c("ws", "wd"))) {
    # Assume that we've been given a wind_df frame.
    # Lets find the correct columns
    if (length(colnames(wind_df)) &&
      all(!colnames(wind_df) %in% c("X1", "X2", "X3"))) {
      accep_speed <- c("SPEED", "GESCH", "V", "WS")
      accep_direc <- c("DIR", "RICHT", "WD")
      accep_proba <- c("PRO", "WAHR")
      sum_col_match <- sum(sapply(
        c(accep_speed, accep_direc, accep_proba),
        grepl, toupper(colnames(wind_df))
      ))
      if (sum_col_match >= 2) {
        speed_match <- which(sapply(
          lapply(accep_speed, grepl, toupper(colnames(wind_df))),
          any
        ))
        direc_match <- which(sapply(
          lapply(accep_direc, grepl, toupper(colnames(wind_df))),
          any
        ))
        probab_match <- which(sapply(
          lapply(accep_proba, grepl, toupper(colnames(wind_df))),
          any
        ))
        speed_index <- which(grepl(
          accep_speed[speed_match],
          toupper(colnames(wind_df))
        ))
        direc_index <- which(grepl(
          accep_direc[direc_match],
          toupper(colnames(wind_df))
        ))
        if (length(probab_match) != 0) {
          probab_index <- which(grepl(
            accep_proba[probab_match],
            toupper(colnames(wind_df))
          ))
          wind_df[, c(speed_index[1], direc_index[1], probab_index[1])]
          colnames(wind_df) <- c("ws", "wd", "probab")
        } else {
          wind_df[, c(speed_index[1], direc_index[1])]
          colnames(wind_df) <- c("ws", "wd")
        }
      } else {
        col_numeric <- which(sapply(wind_df[1, ], is.numeric))
        wind_df <- wind_df[, col_numeric]
        colnames(wind_df) <- c("ws", "wd")
      }
    } else {
      col_numeric <- which(sapply(wind_df[1, ], is.numeric))
      wind_df <- wind_df[, col_numeric]
      if (length(colnames(wind_df)) == 2) {
        colnames(wind_df) <- c("ws", "wd")
      } else {
        colnames(wind_df) <- c("ws", "wd", "probab")
      }
    }
  }
  wind_df$wd <- round(wind_df$wd, 0)
  wind_df$wd <- round(wind_df$wd / 100, 1) * 100
  ## If no probabilites are given, assign uniform distributed ones.
  if (anyNA(colnames(wind_df))) {
    which(is.na(colnames(wind_df)))
    colnames(wind_df)[3] <- "probab"
  }
  if (any(names(wind_df) == "probab") == FALSE) {
    wind_df$probab <- 100 / nrow(wind_df)
  }
  ## Checks if all the sum of possibility is  100
  if (sum(wind_df$probab) != 100) {
    wind_df$probab <- wind_df$probab * (100 / sum(wind_df$probab))
  }
  ## Checks if duplicated wind directions are at hand
  if (any(duplicated(wind_df$wd))) {
    for (i in 1:length(wind_df[duplicated(wind_df$wd) == FALSE, 1])) {
      ## Get duplicated direction rows
      temp <- wind_df[wind_df$wd == wind_df[duplicated(
        wind_df$wd
      ) == FALSE, ][i, "wd"], ]
      ## Sum up speed and probability
      temp$ws <- sum(temp$ws * (temp$probab / sum(temp$probab)))
      temp$probab <- sum(temp$probab * (temp$probab / sum(temp$probab)))
      ## Assign new/uniwue windspeed and probablity per direction
      wind_df[wind_df$wd == wind_df[duplicated(
        wind_df$wd
      ) == FALSE, ][i, "wd"], ]$ws <- round(temp$ws, 2)[1]
      wind_df[wind_df$wd == wind_df[duplicated(
        wind_df$wd
      ) == FALSE, ][i, "wd"], ]$probab <- round(temp$probab, 2)[1]
    }
  }
  ## Delete duplicated direction rows
  wind_df <- wind_df[!duplicated(wind_df$wd) == TRUE, ]
  ## Order by direction
  wind_df <- wind_df[with(wind_df, order(wd)), ]
  ## Sum up probabilites to 100% again
  if (sum(wind_df$probab) != 100) {
    wind_df$probab <- wind_df$probab * (100 / sum(wind_df$probab))
  }
  probabDir <- wind_df$probab
  if (any(wind_df$wd > 360)) {
    wind_df[wind_df$wd > 360, "wd"] <- wind_df[wind_df$wd > 360, "wd"] - 360
  }
  wind_df <- as.matrix(wind_df)
  winddata <- list(wind_df, probabDir)
  return(winddata)
}
