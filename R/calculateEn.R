#' @title Calculate Energy Outputs of Individuals
#' @name calculateEn
#' @description  Calculate the energy output and efficiency rates of an
#'    individual in the current population under all given wind directions
#'    and speeds. If the terrain effect model is activated, the main
#'    calculations to model those effects will be done in this function.
#'
#' @export
#'
#' @importFrom raster extract calc cellStats terrain resample overlay plot
#' crop extent mask projectRaster cellFromXY
#' @importFrom sp SpatialPoints coordinates spTransform proj4string bbox
#' @importFrom calibrate textxy
#' @importFrom maptools elide
#' @importFrom graphics mtext plot par points
#' @importFrom grDevices topo.colors
#'
#' @param sel A data.frame of an individual of the current population
#' (data.frame)
#' @param referenceHeight The height at which the incoming wind speeds
#' were measured (numeric)
#' @param RotorHeight The desired height of the turbines (numeric)
#' @param SurfaceRoughness A surface roughness length of the
#' considered area in m. If the terrain effect model is activated, a
#' surface roughness will be calculated for every grid cell with the
#' elevation and land cover information (numeric)
#' @param wnkl Indicates the angle at which no wake influences are
#' considered (numeric)
#' @param distanz Indicates the distance after which the wake effects are
#' considered to be eliminated (numeric)
#' @param polygon1 The considered area as shapefile (SpatialPolygons)
#' @param resol The resolution of the grid in meter (numeric)
#' @param RotorR The desired rotor radius in meter (numeric)
#' @param dirSpeed The wind speed and direction data.frame (data.frame)
#' @param topograp Logical value that indicates whether the
#' terrain effect model is activated (TRUE) or deactivated (FALSE)
#' (logical)
#' @param srtm_crop An SRTM raster for the considered area. It is only used
#' when the terrain effect model is activated (raster)
#' @param cclRaster A Corine Land Cover raster that has to be downloaded
#' previously. See also the details at \code{\link{windfarmGA}}
#' The raster will only be used when the terrain effect model is activated.
#' (raster)
#' @param weibull A logical value that specifies whether to take Weibull
#' parameters into account. If weibull==TRUE, the wind speed values from the
#' 'dirSpeed' data frame are ignored. The algorithm will calculate the mean
#' wind speed for every wind turbine according to the Weibull parameters.
#' Default is FALSE. (logical)
#' @param weibullsrc A list of Weibull parameter rasters, where the first list
#' item must be the shape parameter raster k and the second item must be the
#' scale parameter raster a of the Weibull distribution. If no list is given,
#' then rasters included in the package are used instead, which currently
#' only cover Austria. This variable is only used if weibull==TRUE. (list)
#'
#' @return Returns a list of an individual of the current generation
#' with resulting wake effects, energy outputs, efficiency rates for every
#' wind direction. The length of the list will be the amount of incoming
#' wind directions.
#'
#' @examples \dontrun{
#' ## Create a random shapefile
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
#' ## Create a uniform and unidirectional wind data.frame and plot the
#' ## resulting wind rose
#' data.in <- data.frame(ws = 12, wd = 0)
#' windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
#'                 dir = data.in$wd, dirres=10, spdmax=20)
#'
#' ## Assign the rotor radius and a factor of the radius for grid spacing.
#' Rotor= 50; fcrR= 3
#' resGrid <- GridFilter(shape = Polygon1, resol = Rotor*fcrR, prop=1,
#'                       plotGrid = TRUE)
#' ## Assign the indexed data frame to new variable. Element 2 of the list
#' ## is the grid, saved as SpatialPolygon.
#' resGrid1 <- resGrid[[1]]
#'
#' ## Create an initial population with the indexed Grid, 15 turbines and
#' ## 100 individuals.
#' resStartGA <- StartGA(Grid = resGrid1, n = 15, nStart = 100)
#'
#' ## Calculate the expected energy output of the first individual of the
#' ## population.
#' par(mfrow = c(1,2))
#' plot(Polygon1); points(resStartGA[[1]][,'X'],resStartGA[[1]][,'Y'], pch=20,cex=2)
#' plot(resGrid[[2]], add = TRUE)
#' resCalcEn <- calculateEn(sel=resStartGA[[1]],referenceHeight= 50,
#'                    RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
#'                    distanz = 100000, resol = 200,dirSpeed = data.in,
#'                    RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
#'                    weibull = FALSE)
#' length(resCalcEn)
#' str(resCalcEn)
#' resCalcEn <- as.data.frame(resCalcEn)
#' plot(Polygon1, main = resCalcEn$Energy_Output_Red[[1]])
#' points(x = resCalcEn$Bx, y = resCalcEn$By, pch = 20)
#'
#'
#' ## Create a variable and multidirectional wind data.frame and plot the
#' ## resulting wind rose
#' data.in10 <- data.frame(ws = runif(10,1,25), wd = runif(10,0,360))
#' windrosePlot <- plotWindrose(data = data.in10, spd = data.in10$ws,
#'                 dir = data.in10$wd, dirres=10, spdmax=20)
#'
#' ## Calculate the energy outputs for the first individual with more than one
#' ## wind direction.
#' resCalcEn <- calculateEn(sel=resStartGA[[1]],referenceHeight= 50,
#'                    RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
#'                    distanz = 100000, resol = 200,dirSpeed = data.in10,
#'                    RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
#'                    weibull = FALSE)
#' length(resCalcEn)
#' str(resCalcEn)
#' }
#' @author Sebastian Gatscha
#'
calculateEn       <- function(sel, referenceHeight, RotorHeight, SurfaceRoughness,
                              wnkl,distanz, polygon1, resol, RotorR, dirSpeed,
                              srtm_crop, topograp, cclRaster, weibull, weibullsrc){
  
  # sel=resStartGA[[1]];referenceHeight= 50;
  # RotorHeight= 50; SurfaceRoughness = 0.14;wnkl = 20;
  # distanz = 100000; resol = 200;dirSpeed = data.in;
  # RotorR = 50; polygon1 = Polygon1; topograp = FALSE;
  # weibull = TRUE
  
  
  
  sel1 <- sel[,2:3];
  ## Assign constant values
  cT <- 0.88;   air_rh <- 1.225;   k <- 0.075;  plotit <- FALSE
  
  ## TODO - this can go in some upper level
  pcent = apply(sp::bbox(polygon1), 1, mean)
  
  ## Create a dummy vector of 1 for the wind speeds.
  windpo = rep(1, length(sel1[,1]))
  
  ## Get the Coordinates of the individual / wind farm. Change dirSpeed to matrix
  xyBgldMa <- sel1
  
  ## Terrain Effect Model:
  ## TODO - Change all raster::extract to no buffer
  if (topograp) {
    ## Calculates Wind multiplier. Hills will get higher values, valleys will get lower values.
    orogr1 <- raster::calc(srtm_crop, function(x) {x/(raster::cellStats(srtm_crop, mean, na.rm = TRUE))})
    orogrnum <- raster::extract(x = orogr1, y = xyBgldMa, small = TRUE, fun = mean, na.rm = FALSE);
    orogrnum[is.na(orogrnum)] <- mean(orogrnum, na.rm = TRUE)
    windpo <- windpo * orogrnum
    
    
    ## Get Elevation of Turbine Locations to estimate the air density at the resulting height
    heightWind <- raster::extract(x = srtm_crop, y = xyBgldMa, small = TRUE, fun = max, na.rm = FALSE);
    heightWind[is.na(heightWind)] <- mean(heightWind, na.rm = TRUE)
    
    ## Plot the elevation and the wind speed multiplier rasters
    if (plotit){
      par(mfrow=c(2,1))
      plot(srtm_crop, main="SRTM Elevation Data");points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round(heightWind,0),cex=0.8);plot(polygon1,add=T)
      plot(orogr1, main="Wind Speed Multipliers");points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round(windpo,3),cex=0.8);plot(polygon1,add=T)
    }
    
    ## Get Air Density and Pressure from Height Values from the function "BaroHoehe"
    HeighttoBaro <- matrix(heightWind); colnames(HeighttoBaro) <- "HeighttoBaro"
    air_dt <- BaroHoehe(matrix(HeighttoBaro),HeighttoBaro)
    air_rh <- as.numeric(air_dt$rh);
    ## Plot he normal and corrected Air Density Values
    if (plotit){
      par(mfrow=c(1,1))
      plot(srtm_crop, main="Normal Air Density",col=topo.colors(10));points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = rep(1.225,nrow(sel1)),cex=0.8);plot(polygon1,add=T)
      raster::plot(srtm_crop, main="Corrected Air Density",col=topo.colors(10));points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round(air_dt$rh,2),cex=0.8);plot(polygon1,add=T)
    }
    
    ## Corine Land Cover Surface Roughness values and Elevation Roughness
    SurfaceRoughness0 <- raster::extract(x = cclRaster, y = xyBgldMa, small = TRUE, fun = mean, na.rm = FALSE);
    SurfaceRoughness0[is.na(SurfaceRoughness0)] <- mean(SurfaceRoughness0, na.rm = TRUE)
    
    elrouind <- raster::terrain(srtm_crop,"roughness")
    SurfaceRoughness1 <- raster::extract(x = elrouind, y = xyBgldMa, small = T, fun =  mean, na.rm = FALSE);
    SurfaceRoughness1[is.na(SurfaceRoughness1)] <- mean(SurfaceRoughness1, na.rm = TRUE)
    SurfaceRoughness <- SurfaceRoughness * (1 + (SurfaceRoughness1 / max(raster::res(srtm_crop))));
    elrouindn <- raster::resample(elrouind, cclRaster, method="ngb")
    modSurf <- raster::overlay(x = cclRaster,y = elrouindn, fun = function(x,y){return(x*(1+y/max(raster::res(srtm_crop))))})
    ## Plot the different Surface Roughness Values
    if (plotit){
      graphics::par(mfrow=c(1,1)); cexa=0.9
      raster::plot(cclRaster, main="Corine Land Cover Roughness");
      graphics::points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round(SurfaceRoughness0,2),cex=cexa);
      plot(polygon1,add=T)
      raster::plot(x= raster::terrain(srtm_crop,"roughness",neighbors = 4),
                   main="Elevation Roughness Indicator");
      graphics::points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round((SurfaceRoughness1),2),cex=cexa);
      plot(polygon1,add=T)
      plot(modSurf, main="Modified Surface Roughness");
      graphics::points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round((SurfaceRoughness),2),cex=cexa);
      plot(polygon1,add=T)
    }
    
    ## New Wake Decay Constant calculated with new surface roughness values
    k <- 0.5/(log(RotorHeight/SurfaceRoughness))
    ## Plot resulting Wake Decay Values
    if (plotit){
      graphics::par(mfrow=c(1,1)); cexa=0.9
      plot(x= raster::terrain(srtm_crop,"roughness",neighbors = 4),
           main="Adapted Wake Decay Values - K");
      graphics::points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round((k),3),cex=cexa);
      plot(polygon1,add=T)
    }
  }
  

  ## For every wind direction, calculate the energy output. Do so by rotating Polygon for all angles and
  ## analyze, which turbine is affected by another one to calculate total energy output.
  ## Save Output in a list.
  alllist <- vector("list", length(dirSpeed[,1]))
  for (index in 1:length(dirSpeed[,2])) {
    # index=1
    
    ## Get mean windspeed for every turbine location from windraster
    pointWind <- windpo * dirSpeed[index,'ws']
    
    if (weibull){
      k_param <- weibullsrc[[1]]
      a_param <- weibullsrc[[2]]
      
      ## TODO This can actually go out in genAlgo or fitness. Doesnt need to be evaluated over and over again
      ## Just Erwartungswert must be given as raster or even matrix.
      Erwartungswert <- a_param * (gamma(1 + (1/k_param)))
      Erwartungswert <- raster::projectRaster(Erwartungswert, crs = proj4string(polygon1))
      
      if (plotit){
        weibl_k <- projectRaster(k_param, crs = proj4string(polygon1))
        weibl_a <- projectRaster(a_param, crs = proj4string(polygon1))
        par(mfrow=c(3,1), ask=F)
        plot(Erwartungswert, main="Mean Weibull"); plot(polygon1, add=T)
        plot(weibl_a, main="A Weibull Parameter"); plot(polygon1, add=T)
        plot(weibl_k, main="K Weibull Parameter"); plot(polygon1, add=T)
      }
      
      xys <- raster::cellFromXY(Erwartungswert, xy = xyBgldMa)
      Erwartungswertxy <- Erwartungswert[xys]
      
      pointWind <- windpo * Erwartungswertxy
    }
    
    ## Calculate Windspeed according to Rotor Height using wind profile law. 
    ## TODO MISSING: Other law possible with log
    pointWind <- pointWind * ((RotorHeight / referenceHeight)^SurfaceRoughness)
    pointWind[is.na(pointWind)] <- 0
    
    ## Get the current incoming wind direction and assign to "angle"
    angle <- -dirSpeed[index, 'wd']
    
    ## If activated, plots the turbine locations with angle 0 and opens a 
    ## second frame for rotated turbine lovations
    if (plotit){
      par(mfrow=c(1,2))
      plot(polygon1, main="Shape at angle 0");
      points(xyBgldMa[,1],xyBgldMa[,2],pch=20)
      textxy(xyBgldMa[,1],xyBgldMa[,2], labs = dimnames(xyBgldMa)[[1]],cex=0.8)
      Polygon3 = maptools::elide(polygon1, rotate=angle, center=apply(bbox(polygon1), 1, mean));
      plot(Polygon3, main=c("Shape at angle:", (-1*angle)))
      mtext(paste("Direction: ", index, "\nfrom total: ", nrow(dirSpeed)), side = 1)
    }
    
    ## Rotate Coordinates by the incoming wind direction
    xyBgldMa = rotate_CPP(xyBgldMa[,1], xyBgldMa[,2], pcent[1], pcent[2], angle)
    colnames(xyBgldMa) <- c("x","y")
    
    
    ## If activated, plots the rotated turbines in red.
    if (plotit){
      points(xyBgldMa, col="red",pch=20)
    }
    
    ## If Height is taken into account. 3D Modelling of Wake and Overlapping Areas
    DatFram <- cbind(pointWind, xyBgldMa)
    colnames(DatFram) = c("Windmittel","X","Y")
    row.names(DatFram) = NULL
    
    ## Get the influecing points given with incoming wind direction angle
    ## and reduce then to data frame
    tmp <- InfluPoints(t = xyBgldMa, wnkl =  wnkl, dist = distanz, 
                       polYgon = polygon1, dirct = angle)
    dfAll <- do.call("rbind", tmp)
    
    ## Create a list for every turbine
    ## Assign Windspeed to a filtered list with all turbines and add the desired 
    ## rotor radius to the data frame
    tmp <- lapply(seq_len(max(dfAll[,'Punkt_id'])), function(i) {
      cbind(subset.matrix(dfAll, dfAll[,'Punkt_id'] == i, 
                          select = c('Punkt_id','Ax','Ay','Bx','By','Laenge_B',
                                     'Laenge_A','alpha','Windrichtung')), 
            "Windmean" = DatFram[,1L][i])
    })
    windlist <- do.call("rbind", tmp)
    row.names(windlist) = NULL
    windlist <- cbind(windlist, 
                      "RotorR" = as.numeric(RotorR))
    
    
    ## Calculate the wake Radius and the rotor area for every turbine.
    lnro <- length(windlist[,1])
    tmp <- sapply(1:lnro, function(i) {
      if (topograp) {
        k <- k[windlist[i,'Punkt_id']]
      }
      wakeradius_CPP(windlist[i,'Laenge_B'], FALSE, as.numeric(windlist[i,'RotorR']), k)
    })
    windlist <- cbind(windlist, 
                      "WakeR" = tmp[1,], 
                      "Rotorflaeche" = tmp[2,])
    
    ## Calculate the overlapping area and the overlapping percentage.
    tmp <- sapply(1:lnro, function(o) {
      Rotorf <- windlist[o, 'RotorR']
      leA <- windlist[o, 'Laenge_A']
      wakr <- windlist[o, 'WakeR'];
      if (windlist[o, 'Laenge_B'] == 0) {
        aov <- 0;
      } else {
        if ((wakr - Rotorf) >= leA && leA >= 0) {
          aov <- (windlist[o, 'RotorR']^2) * pi;
        }
        if ((wakr + Rotorf) <= leA) {
          aov <- 0
        }
        if ((wakr - Rotorf) <= leA && leA <= (wakr + Rotorf))  {
          aov <- wake_CPP(Rotorf = Rotorf,  wakr = wakr, leA = leA)
        }
      }
      if (aov != 0) {
        absch <- ((aov / windlist[o, 'Rotorflaeche']) * 100)
      } else {
        absch <- 0;
      }
      rbind(aov, absch)
    })
    windlist <- cbind(windlist, 
                      "A_ov" = round(tmp[1,], 4),
                      "AbschatInProz" = round(tmp[2,], 4))
    
    ## Calculate the wind velocity reduction.
    tmp <- unlist(lapply(1:lnro, function(p) {
      RotrR <- windlist[p, 'RotorR']
      a <- {1 - sqrt(1 - cT)}
      s <- windlist[p, 'Laenge_B']/RotrR
      if (topograp){
        b <- (1 + (k[windlist[p, 'Punkt_id']] * s))^2;
      } else {
        b <- (1 + (k * s))^2
      }
      aov <- windlist[p, 'A_ov'] / windlist[p, 'Rotorflaeche']
      windlist[p, 'Windmean'] * (aov *(a / b))
    }))
    names(tmp) = NULL
    windlist <- cbind(windlist, 
                      "V_red" = tmp)
    
    
    ## Calculate multiple wake effects, total wake influence, the new resulting wind velocity
    ## and add the Grid IDs.
    whichh <- windlist[,'Punkt_id']
    windlist <- cbind(windlist, 
                      "V_i" = 0, 
                      "TotAbschProz" = 0, 
                      "V_New" = 0, 
                      "Rect_ID" = 0)
    
    # windlist[,'V_i'] <- unsplit(lapply(split(windlist[,'V_red'], factor(whichh)), function(i) {
      # sqrt(sum(i^2))}), factor(whichh))    
    windlist[,'V_i'] <- unlist(lapply(unique(whichh), function(i) {
      sums = sqrt(sum(windlist[windlist[,'Punkt_id'] == i, 'V_red'] ^ 2))
      rep(sums, length(windlist[windlist[,'Punkt_id'] == i, 'V_red']))
    }))
    
    windlist[,'TotAbschProz'] <- unlist(lapply(unique(whichh), function(i) {
      absch <- windlist[whichh == i,'AbschatInProz']
      rep(sum(absch), length(absch))
    }))
    windlist[,'V_New'] <- unlist(lapply(unique(whichh), function(i) {
      windlist[whichh == i,'Windmean'] - windlist[whichh == i,'V_i']
    }))
    windlist[,'Rect_ID'] <-  unlist(lapply(unique(whichh), function(i) {
      rep(sel[i,'ID'], length(windlist[whichh == i,1]))
    }))
    
    
    ## Get a reduced dataframe and split duplicated Point_id, since a turbine with fixed Point_id,
    ## can have several influencing turbines and therefore several data frame elements
    windlist2 <- subset.matrix(windlist, select = c("Punkt_id","Ax","Ay","Bx","By","Laenge_B","Laenge_A",
                                                    "Windrichtung","Windmean","RotorR","WakeR","A_ov","TotAbschProz",
                                                    "V_New","Rect_ID"))
    # windlist2 <- windlist
    windlist1 <- subset.matrix(windlist2, subset = !duplicated(windlist2[,'Punkt_id']))
    
    
    ## Calculate Full and reduced Energy Outputs in kW and Park Efficienca in %. Assign the values to
    ## the list. NOTE (0.2965 = 0.593 * 0.5)
    # EneOutRed <- round(sum(0.2965 * air_rh * (windlist1[,'V_New']^3) *
    #                          ((windlist1[,'RotorR']^2) * pi), na.rm = TRUE) / 1000, 4)
    # EneOutFul <- round(sum(0.2965 * air_rh * (windlist1[,'Windmean']^3) *
    #                          ((windlist1[,'RotorR']^2) * pi), na.rm = TRUE) / 1000, 4)
    EneOutRed <- energy_calc_CPP(windlist1[,'V_New'], windlist1[,'RotorR'], air_rh)
    EneOutFul <- energy_calc_CPP(windlist1[,'Windmean'], windlist1[,'RotorR'], air_rh)
    
    Effic <- (EneOutRed * 100) / EneOutFul
    
    windlist2 <- cbind(windlist2, 
                       "Energy_Output_Red" = EneOutRed, 
                       "Energy_Output_Voll" = EneOutFul,
                       "Parkwirkungsgrad" = Effic)
    
    windlist2[,'Windrichtung'] = windlist2[,'Windrichtung'] * (-1)
    
    alllist[[index]] <- windlist2
  }
  
  ## Return the list with all relevant information
  invisible(alllist)
}
