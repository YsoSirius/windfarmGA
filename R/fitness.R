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
#' population. (list)
#' @param referenceHeight The height at which the incoming wind speeds were
#' measured. (numeric)
#' @param RotorHeight The desired height of the turbine. (numeric)
#' @param SurfaceRoughness A surface roughness length of the considered area
#' in m. (numeric)
#' @param Polygon The considered area as shapefile. (SpatialPolygons)
#' @param resol1 The resolution of the grid in meter. (numeric)
#' @param rot The desired rotor radius in meter. (numeric)
#' @param dirspeed The wind speed and direction data.frame. (data.frame)
#' @param srtm_crop Logical value that indicates whether the terrain effect
#' model is activated (TRUE) or deactivated (FALSE). (logical)
#' @param topograp Logical value that indicates whether the terrain effect
#' model is activated (TRUE) or deactivated (FALSE). (logical)
#' @param cclRaster A Corine Land Cover raster, that has to be adapted
#' previously by hand with the surface roughness lenght for every land cover
#' type. Is only used, when the terrain effect model is activated. (raster)
#' @param weibull A logical value that specifies whether to take Weibull
#' parameters into account. If weibull==TRUE, the wind speed values from the
#' 'dirSpeed' data frame are ignored. The algorithm will calculate the mean
#' wind speed for every wind turbine according to the Weibull parameters.
#' (logical)
#' @param weibullsrc A list of Weibull parameter rasters, where the first list
#' item must be the shape parameter raster k and the second item must be the
#' scale parameter raster a of the Weibull distribution. If no list is given,
#' then rasters included in the package are used instead, which currently
#' only cover Austria. This variable is only used if weibull==TRUE. (list)
#' @param Parallel Boolean value, indicating whether parallel processing
#' should be used. The parallel and doParallel packages are used for parallel 
#' processing.
#' @param numCluster If Parallel is TRUE, this variable defines the number
#' of clusters to be used.
#' @param verbose If TRUE, will print out further information. 
#'
#' @return Returns a list with every individual, consisting of X & Y
#' coordinates, rotor radii, the runs and the selected grid cell IDs, and
#' the resulting energy outputs, efficiency rates and fitness values.
#' (list)
#'
#' @examples \donttest{
#' ## Create a random rectangular shapefile
#' library(sp)
#' library(windfarmGA)
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
#' data.in <- as.data.frame(cbind(ws=12,wd=0))
#' # windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
#' #                dir = data.in$wd, dirres=10, spdmax=20)
#'
#' ## Calculate a Grid and an indexed data.frame with coordinates and
#' ## grid cell IDs.
#' Grid1 <- GridFilter(shape = Polygon1,resol = 200,prop = 1);
#' Grid <- Grid1[[1]]
#' AmountGrids <- nrow(Grid)
#'
#' startsel <- StartGA(Grid,10,20);
#' wind <- as.data.frame(cbind(ws=12,wd=0))
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
                              rot, dirspeed,srtm_crop,topograp,cclRaster,
                              weibull, weibullsrc, Parallel, numCluster, verbose){
  verbose = F

  ## Missing Arguments ? ToDo: beautify this #############
  if (missing(srtm_crop)){
    srtm_crop=NULL
  }
  if (missing(cclRaster)){
    cclRaster=NULL
  }  
  if (missing(Parallel)){
    Parallel = FALSE
  }
  if (missing(weibull)){
    weibull = FALSE
    weibullsrc = NULL
  }
  #############
  
  
  ## Winddata Formatting ###################
  dirspeed$wd <- round(dirspeed$wd,0)
  dirspeed$wd <-  round(dirspeed$wd/100,1)*100;
  ## If no probabilites are given, assign uniform distributed ones.
  if (any(names(dirspeed) == "probab") == FALSE) {
    dirspeed$probab <- 100/nrow(dirspeed)
  }
  ## Checks if all wind directions/speeds have a possibility greater than 0.
  dirspeed$probab <- round(dirspeed$probab,0)
  if (sum(dirspeed$probab) != 100) {
    dirspeed$probab <- dirspeed$probab*(100/sum(dirspeed$probab))
  }
  ## Checks if duplicated wind directions are at hand
  if   (any(duplicated(dirspeed$wd) == TRUE)) {
    for (i in 1:nrow(dirspeed[duplicated(dirspeed$wd) == FALSE,])){
      temp <- dirspeed[dirspeed$wd ==  dirspeed[duplicated(
        dirspeed$wd) == FALSE,][i,'wd'],];
      temp$ws < -with(temp, sum(ws * (probab / sum(probab))));
      temp$probab <- with(temp, sum(probab * (probab/sum(probab))));

      dirspeed[dirspeed$wd ==  dirspeed[duplicated(
        dirspeed$wd) == FALSE,][i,'wd'],]$ws <- round(temp$ws,2)[1]
      dirspeed[dirspeed$wd ==  dirspeed[duplicated(
        dirspeed$wd) == FALSE,][i,'wd'],]$probab <- round(temp$probab,2)[1]
    }
  }
  dirspeed <- dirspeed[!duplicated(dirspeed$wd) == TRUE,];
  dirspeed <- dirspeed[with(dirspeed, order(wd)),]
  if (sum(dirspeed$probab) != 100) {
    dirspeed$probab <- dirspeed$probab * (100 / sum(dirspeed$probab))
  }
  probabDir <- dirspeed$probab;
  pp <- sum(probabDir)/100;   probabDir <- probabDir/pp;
  
  ## TODO - Move this to genAlgo (not necesseray here and just inefficient) 
  ## DO i need rasterize anyway? Can i not create vector 1
  ## Can a windraster be included? last method in calculateEn doesnt need windraster
  
  # windraster <- raster::rasterize(Polygon, 
  #                                 raster::raster(raster::extent(Polygon),
  #                                                ncol = 180, nrow = 180),
  #                                 field = 1)
  ###################
  
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
  #     cat("Some Layouts are already known. Get values from cached results instead of recalculating")
  #     # browser()
  #   }
  # }
  ###################
  
  known=TRUE
  if (known) {
    # Calculate EnergyOutput for every config i and for every angle j - in Parallel
    if (Parallel == TRUE) {
      e <- foreach::foreach(i = 1:length(selection), .combine = 'c') %dopar% {
        windfarmGA::calculateEn(sel = selection[[i]], referenceHeight = referenceHeight, 
                                RotorHeight = RotorHeight, SurfaceRoughness = SurfaceRoughness, 
                                # windraster = windraster, 
                                wnkl = 20, distanz=100000, 
                                polygon1 = Polygon, resol = resol1, RotorR = rot, dirSpeed = dirspeed,
                                srtm_crop = srtm_crop, topograp = topograp, cclRaster = cclRaster, 
                                weibull = weibull, weibullsrc = weibullsrc)
      }
    } else {
      e <- vector("list", length(selection));
    }
    ###################
    
    euniqu <- vector("list",length(selection)) ;
    for (i in 1:length(selection)){
      if (!Parallel) {
        
        ## TODO
        ########################################################
        ########################################################
        # browser()
        ## Do i need e? Just euniue is givn back. What about in parallel?
        ## Mehrere Windrichtungen?
        ########################################################
        ########################################################
        
        # Calculate EnergyOutput for every config i and for every angle j - not Parallel
        e[[i]] <- calculateEn(sel = selection[[i]], referenceHeight = referenceHeight, 
                              RotorHeight = RotorHeight, SurfaceRoughness = SurfaceRoughness, 
                              # windraster = windraster,
                              wnkl = 20, distanz=100000,
                              polygon1 = Polygon, resol=resol1, RotorR = rot, dirSpeed = dirspeed,
                              srtm_crop = srtm_crop, topograp = topograp,cclRaster = cclRaster, 
                              weibull = weibull, weibullsrc = weibullsrc)
        # ee  <- lapply(e[[i]], function(x){split(x, duplicated(x[,'Punkt_id']))$'FALSE'})
        ee  <- lapply(e[[i]], function(x){subset.matrix(x, subset = !duplicated(x[,'Punkt_id']))})

      } else {
        ## TODO Test parallel --- 
        
        # Get a list from unique Grid_ID elements for every park configuration respective 
        # to every winddirection considered.
        # ee  <- lapply(e[i], function(x){split(x, duplicated(x$Punkt_id))$'FALSE'})
        ee  <- lapply(e[i], function(x){subset.matrix(x, subset = !duplicated(x[,'Punkt_id']))})
      }
  
      if (TRUE) {
      # Select only relevant information from list
      # ee  <- lapply(ee, function(x){
      #   dplyr::select(x,-Ax,-Ay,-Laenge_B,-Laenge_A,-Windmean,-WakeR,-A_ov, -Punkt_id)})
      ee  <- lapply(ee, function(x){
        subset.matrix(x, select = c('Bx','By','Windrichtung','RotorR','TotAbschProz','V_New',
                                    'Rect_ID','Energy_Output_Red', 'Energy_Output_Voll',
                                    'Parkwirkungsgrad'))})
      
      
      
      # TODO   
      # get Energy Output and Efficiency rate for every wind direction
      # enOut <- lapply(ee, function(x){ x[1,c(3,8,10)]}); 
      enOut <- lapply(ee, function(x){
        subset.matrix(x, 
                      subset = c(T,rep(F,length(ee[[1]][,1])-1)),
                      select = c('Windrichtung','Energy_Output_Red','Parkwirkungsgrad'))})
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
      AbschGesamt <- lapply(ee, function(x){ x[,'TotAbschProz']})
      AbschGesamt <- do.call("cbind", AbschGesamt)
      AbschGesamt <- rowSums(AbschGesamt)

      # Get the original X / Y - Coordinates of the selected individual
      xundyOrig <- selection[[i]][,2:3]
      
      # Add the Efficieny and the Energy Output of all wind directions and add the total 
      # Wake Effect of every Point Location
      # Include the Run of the genertion to the data frame
      xundyOrig <- cbind(xundyOrig, 
                         'EfficAllDir' = enOut[1,'Efficalldire'],
                         'EnergyOverall' = enOut[1,'EnergyOverall'],
                         'AbschGesamt' = AbschGesamt,
                         'Run' = i)

  
      # Get the Rotor Radius and the Rect_IDs of the park configuration
      dt <-  ee[[1]] 
      dt <- subset.matrix(dt, select = c('RotorR','Rect_ID'))

      # Bind the Efficiency,Energy,WakeEffect,Run to the Radius and Rect_IDs
      dt <- cbind(xundyOrig, dt)
      
      # Add this information to the the i-th element of the list
      euniqu[[i]] <- dt
      }
    }
  
    # Split one from every run and select only Energy information
    # maxparkeff <- do.call("rbind", (lapply(euniqu, function(x) {
      # x <- dplyr::select(x[1,],EnergyOverall)})))
    maxparkeff <- do.call(rbind, lapply(euniqu, function(x) {
      subset.matrix(x[1,],select = 'EnergyOverall')}))
    
    # Calculate Parkfitness for every individual.
    # maxparkeff <- cbind(maxparkeff, 'Parkfitness' = maxparkeff$EnergyOverall)
    colnames(maxparkeff) <- 'Parkfitness'
    
    # Select only the Fitness Value
    # maxparkeff <- maxparkeff[,2]
  
    # Assign every park constellation the Parkfitness Value
    euniqu <- lapply(1:length(euniqu), function(i) {
      cbind(euniqu[[i]], 'Parkfitness' = maxparkeff[i,])
    })
    
    euniqu
  }
  names(euniqu) <- unlist(lapply(euniqu, function(i) {paste0(i[,'Rect_ID'], collapse = ",")}))

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
