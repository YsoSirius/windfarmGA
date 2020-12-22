#' @name windfarmGA
#' @description The initiating function of an optimization run which will
#'   interactively check user-inputs. If all inputs are correct, an optimization
#'   will be started.
#'
#' @export
#'
#' @param dns The data source name (interpretation varies by driver — for some
#'   drivers, dsn is a file name, but may also be a folder)
#' @param layer The layer name
#'
#' @seealso \code{\link{genetic_algorithm}}
#' @family Genetic Algorithm Functions
#'
#' @inherit genetic_algorithm details return params title
windfarmGA <- function(dns, layer, Polygon1, GridMethod, Projection, 
                       sourceCCL, sourceCCLRoughness,
                       vdirspe, Rotor = 30, fcrR = 5, n = 10, topograp = FALSE,
                       iteration = 20, referenceHeight = 50,
                       RotorHeight = 50, SurfaceRoughness = 0.14, 
                       Proportionality = 1, mutr = 0.008, 
                       elitism = TRUE, nelit = 7,
                       selstate = "FIX", crossPart1 = "EQU", trimForce = TRUE, 
                       weibull, weibullsrc,
                       Parallel, numCluster, verbose = FALSE, plotit = FALSE) {
  ## Plotting Settings #########
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mfrow = c(1,2), ask = FALSE)
  
  ######## Check Polygon  ######## 
  ## Check if Polygon given is correctly
  if (!missing(Polygon1)) {
    if (!missing(Projection)) {
      Polygon1 <- isSpatial(Polygon1, Projection)
    } else {
      Polygon1 <- isSpatial(Polygon1)
    }
    plot(Polygon1, col = "red", main = "Original Input Shapefile", axes =TRUE)
    title(sub = st_crs(Polygon1)$input, line = 1)
    readline(prompt = "\nHit <ENTER> if this is your Polygon")
  }
  ## Load Polygon from a Source File. Just if dns and layer are not missing.
  if (!missing(dns) & !missing(layer)) {
    # Input the Source of the desired Polygon
    Polygon1 <- sf::st_read(dsn = dns, layer = layer)
    plot(Polygon1, col = "red", main = "Original Input Shapefile", axes =TRUE)
    title(sub = paste("CRS:", st_crs(Polygon1)$input), line = 1)
    readline(prompt = "\nHit <ENTER> if this is your Polygon")
  }
  PROJ6 <- utils::compareVersion(sf::sf_extSoftVersion()[[3]], "6") > 0
  
  ##  Project the Polygon to LAEA if it is not already.
  if (missing(Projection)) {
    cat("No Projection is given. Take Lambert Azimuthal Equal Area Projection (EPSG:3035).\n")
    if (PROJ6) {
      Projection <- 3035
    } else {
      Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
    }
  } else {
    Projection <- Projection
  }
  if (is.na(st_crs(Polygon1))) {
    cat("Polygon is not projected. The spatial reference WGS 84 (EPSG:4326) is assumed.\n")
    if (PROJ6) {
      st_crs(Polygon1) <- 4326
    } else {
      Projection <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
    }
  }
  if (PROJ6) {
    if (!isTRUE(all.equal(st_crs(Polygon1), st_crs(Projection)))) {
      Polygon1 <- sf::st_transform(Polygon1, st_crs(Projection))
    }
  } else {
    if (as.character(raster::crs(Polygon1)) != Projection) {
      Polygon1 <- sf::st_transform(Polygon1, st_crs(Projection))
    }
  }
  plot(Polygon1, col = "red", main = "Projected Input Shapefile", axes =TRUE)
  title(sub = paste("CRS:", st_crs(Polygon1)$input), line = 1)
  readline(prompt = "\nHit <ENTER> if this is your Polygon")

  ######## CHECK Crossover and Selection Params ######## 
  ## Check if Crossover Method is chosen correctly.
  if (missing(crossPart1)) {crossPart1 <- readinteger()}
  if (crossPart1 != "EQU" & crossPart1 != "RAN") {
    crossPart1 <- readinteger()
  }
  ## Check if Selection Method is chosen correctly.
  if (missing(selstate)) {selstate <- readintegerSel()}
  if (selstate != "FIX" & selstate != "VAR") {
    selstate <- readintegerSel()
  }

  ######## Check Wind Data ############
  ## Check if Input Wind data is given correctly.
  if (missing(vdirspe)) {
    stop("\n##### No wind data.frame is given. \nThis input is required for an optimization run")
  }
  plot.new()
  plot_windrose(data = vdirspe, spd = vdirspe[, 'ws'], dir = vdirspe[, 'wd'])
  readline(prompt = "\nPress <ENTER> if the windrose looks correct?")
  
  ######## Check Numeric and logical Inputs ############
  ## Check if Rotor,fcrR,n,iteration,RotorHeight,
  ## SurfaceRoughness,Proportionality,mutr,nelit are numeric
  ChekNumer <- is.numeric(c(Rotor, n, fcrR, iteration, referenceHeight, 
                            RotorHeight, SurfaceRoughness, Proportionality, mutr, nelit))
  if (ChekNumer == FALSE) {
    cat("################### GA WARNING MESSAGE ###################")
    stop("\n##### A required numeric input is not numeric.\n
         See documentation.")
  }
  ## Check if topograp,elitism,trimForce are logical
  ChekLogic <- is.logical(c(topograp, elitism, trimForce))
  if (ChekLogic == FALSE) {
    cat("################### GA WARNING MESSAGE ###################")
    stop("\n##### A required logical input is not logical.\n
         See documentation.")
  }
  
  ######## Check Gridding Params ############
  ## Check if Grid with given inputs is correctly
  if (missing(GridMethod)) {
    GridMethod <- "Rectangular"
  }
  GridMethod <- toupper(GridMethod)
  ## Decide if the space division should be rectangular or in hexagons.
  if (GridMethod == "HEXAGON" | GridMethod == "H") {
    Grid <- hexa_area(shape = Polygon1, 
                    size = (Rotor * fcrR) / 2, plotGrid = TRUE)
  } else {
    Grid <- grid_area(shape = Polygon1, size = (Rotor * fcrR), 
                       prop = Proportionality, plotGrid = TRUE)
  }
  cat("\nIs the grid spacing appropriate?\n")
  # InputDaccor <- readline(prompt = "Hit 'ENTER' if the the grid is corrent and 'n' if you like to change some inputs.")
  
  cat("Type 'ENTER' if the the grid is corrent and 'n' if you like to change some inputs.")
  InputDaccor <- readLines(n = 1, con = getOption("windfarmGA.connection"))
  
  InputDaccor <- tolower(InputDaccor)
  if  (InputDaccor == "n") {
    cat("################### GA WARNING MESSAGE ###################\n")
    stop("\n##### The grid spacing is not as required. \n",
         "Adjust the rotor radius (Rotor), the fraction of the radius (fcrR) or the spatial reference system (Projection).\n",
         "You can also use the function `grid_area` / `hexa_area` first, to see the resulting grid.\n"
         )
  }

  ######## Check Topographic Model and Weibull Params  ######## 
  if (missing(topograp)) {
    topograp <- FALSE
  }    
  if (missing(weibull)) {
    weibull <- FALSE
  }  
  if (weibull) {
    if (!missing(weibullsrc)) {
      if (!class(weibullsrc) == "list") {
        cat(paste("\nweibullsrc class: \n1 - ", class(weibullsrc), "\n"))
        stop("\n'weibullsrc' must be a list with two rasters. List item 1 should be the shape parameter (k) raster
            and list item 2 should be the scale parameter (a) raster of a weibull distribution.")
      }
      if (!class(weibullsrc[[1]])[1] == "RasterLayer" | !class(weibullsrc[[2]])[1] == "RasterLayer" ) {
        cat(paste("\nlist item classes: \n1 - ", class(weibullsrc[[1]]), "\n2 - ", class(weibullsrc[[2]])), "\n")
        stop("\nOne of the given list items is not a raster.")
      }
    }
  }
  
  ######## Check Parallel Params  ######## 
  if (missing(Parallel)) {
    Parallel <- FALSE
  }
  if (missing(numCluster)) {
    numCluster <- 2
  }
  if (Parallel == TRUE) {
    # numPossClus <- as.integer(Sys.getenv("NUMBER_OF_PROCESSORS"))
    numPossClus <- parallel::detectCores()
    if (numCluster > numPossClus) {
      cat("\nNumber of clusters is bigger than the amount of available cores. Reduce to max.")
      numCluster <- numPossClus
    }
  }

  ######## RUN GENETIC ALGORITHM ##############
  cat("Run Algorithm:\n")
  result <- genetic_algorithm(Polygon1 = Polygon1, GridMethod = GridMethod, 
                    Rotor = Rotor, n = n, fcrR = fcrR, iteration = iteration,
                    vdirspe = vdirspe, topograp = topograp,
                    referenceHeight = referenceHeight, RotorHeight = RotorHeight, 
                    SurfaceRoughness = SurfaceRoughness, 
                    Proportionality = Proportionality,
                    mutr = mutr, elitism = elitism, nelit = nelit,
                    selstate = selstate, crossPart1 = crossPart1, 
                    trimForce = trimForce,
                    Projection = Projection, sourceCCL = sourceCCL, 
                    sourceCCLRoughness = sourceCCLRoughness,
                    weibull = weibull, weibullsrc = weibullsrc, 
                    Parallel = Parallel, numCluster = numCluster,
                    verbose = verbose, plotit = plotit)

  invisible(result)
}

