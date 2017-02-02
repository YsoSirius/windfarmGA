#' @title Evaluate the Individual fFtness values
#' @name fitness
#' @description The fitness values of the individuals in the
#' current population are calculated, after having evaluated their energy
#' outputs in \code{\link{calculateEn}}. This function reduces the resulting
#' energy outputs to a single fitness value for every individual.
#'
#' @export
#'
#'
#' @importFrom raster extent rasterize
#' @importFrom dplyr select
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
#'
#' @return Returns a list with every individual, consisting of X & Y
#' coordinates, rotor radii, the runs and the selected grid cell IDs, and
#' the resulting energy outputs, efficiency rates and fitness values.
#' (list)
#'
#' @author Sebastian Gatscha
fitness           <- function(selection, referenceHeight, RotorHeight,
                              SurfaceRoughness, Polygon, resol1,
                              rot, dirspeed,srtm_crop,topograp,cclRaster){

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
  if   (any(duplicated(dirspeed$wd)==TRUE)) {
    for (i in 1:nrow(dirspeed[duplicated(dirspeed$wd)==F,])){
      temp <- dirspeed[dirspeed$wd ==  dirspeed[duplicated(
        dirspeed$wd)==F,][i,'wd'],];
      temp$ws <-with(temp, sum(ws * (probab/sum(probab))));
      temp$probab <- sum(temp$probab);
      dirspeed[dirspeed$wd ==  dirspeed[duplicated(
        dirspeed$wd)==F,][i,'wd'],]$ws <- round(temp$ws,2)[1]
      dirspeed[dirspeed$wd ==  dirspeed[duplicated(
        dirspeed$wd)==F,][i,'wd'],]$probab <- round(temp$probab,2)[1]
    }
  }
  dirspeed <- dirspeed[!duplicated(dirspeed$wd)==TRUE,];
  dirspeed <- dirspeed[with(dirspeed, order(wd)), ]
  if (sum(dirspeed$probab) != 100) {
    dirspeed$probab <- dirspeed$probab*(100/sum(dirspeed$probab))
  }
  probabDir <- dirspeed$probab;
  pp <- sum(probabDir)/100;   probabDir <- probabDir/pp; sum(probabDir)

  windraster <-raster::rasterize(Polygon, raster::raster(raster::extent(Polygon), ncol=180, nrow=180),field=1)

  e = vector("list",length(selection));euniqu=vector("list",length(selection)) ;
  for (i in 1:length(selection)){
    ## Calculate EnergyOutput for every config i and for every angle j
    e[[i]] <- calculateEn(selection[[i]], referenceHeight, RotorHeight,SurfaceRoughness,
                          windraster = windraster, wnkl = 20, distanz=100000, polygon1 = Polygon,
                          resol=resol1, RotorR = rot, dirSpeed = dirspeed, srtm_crop,topograp,cclRaster)

    # Get a list from unique Grid_ID elements for every park configuration respective to every winddirection considered.
    ee  <- lapply(e[[i]], function(x){split(x, duplicated(x$Punkt_id))$'FALSE'})
    # Select only relevant information from list
    ee  <- lapply(ee, function(x){dplyr::select(x,-Ax,-Ay,-Laenge_B,-Laenge_A,-Windmean,-WakeR,-A_ov, -Punkt_id)})
    # get Energy Output and Efficiency rate for every wind direction and make a data frame
    enOut <- lapply(ee, function(x){ x[1,c(3,8,10)]}); enOut <- do.call("rbind", enOut)
    # Add the Probability of every direction
    enOut$probabDir <- probabDir
    # Calculate the relative Energy outputs respective to the probability of the wind direction
    enOut$Eneralldire <- enOut$Energy_Output_Red * (enOut$probabDir/100);
    # Calculate the sum of the relative Energy outputs
    enOut$EnergyOverall <- sum(enOut$Eneralldire);
    # Calculate the sum of the relative Efficiency rates respective to the probability of the wind direction
    enOut$Efficalldire <- sum(enOut$Parkwirkungsgrad * (enOut$probabDir/100))

    # Get the total Wake Effect of every Turbine for all Wind directions
    AbschGesamt <- lapply(ee, function(x){ data.frame(x$TotAbschProz)});
    AbschGesamt <- do.call("cbind",AbschGesamt)
    AbschGesamt$RowSum <- rowSums(AbschGesamt);
    AbschGesamt <- AbschGesamt$RowSum

    # Get the original X / Y - Coordinates of the selected individual
    xundyOrig <- selection[[i]][,2:3]; xundyOrig
    # Add the Efficieny and the Energy Output of all wind directions and add the total Wake Effect of every Point Location
    xundyOrig$EfficAllDir <- enOut$Efficalldire[1];xundyOrig$EnergyOverall <- enOut$EnergyOverall[1];xundyOrig$AbschGesamt <- AbschGesamt
    # Include the Run of the genertion to the data frame
    xundyOrig$Run <- i

    # Get the Rotor Radius and the Rect_IDs of the park configuration
    dt <-  ee[[1]]; dt <- dplyr::select(dt,RotorR, Rect_ID);dt;
    # Bind the Efficiency,Energy,WakeEffect,Run to the Radius and Rect_IDs
    dt <- cbind(xundyOrig,dt)
    # Add this information to the the i-th element of the list
    euniqu[[i]] <- dt
  }

  # Split one from every run and select only Energy information
  maxparkeff <- do.call("rbind", (lapply(euniqu, function(x) { x <- dplyr::select(x[1,],EnergyOverall)})))

  # Calculate Parkfitness for every individual.
  #maxparkeff$Parkfitness <- maxparkeff$EnergyOverall/ (sum(maxparkeff$EnergyOverall))
  #maxparkeff$Parkfitness <- maxparkeff$EnergyOverall/ (median(maxparkeff$EnergyOverall))
  #maxparkeff$Parkfitness <- ((maxparkeff$EnergyOverall)*length(maxparkeff$EnergyOverall))/sum(maxparkeff$EnergyOverall);
  maxparkeff$Parkfitness <- maxparkeff$EnergyOverall

  # And select only the Fitness Value
  maxparkeff <- dplyr::select(maxparkeff, Parkfitness)

  # Assign every park constellation the Parkfitness Value
  for (i in 1:length(euniqu)) {
    euniqu[i][[1]]$Parkfitness <- maxparkeff[i,]
  }; #euniqu

  return(euniqu)
}

##importFrom GenAlgo calculateEn


