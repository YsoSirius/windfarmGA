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
#' crop extent mask projectRaster
#' @importFrom dplyr filter select
#' @importFrom sp SpatialPoints coordinates spTransform proj4string
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
#' @param windraster Dummy windraster for the considered area with value 1
#' (raster)
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
#' ## Initialize a dummy wind speed raster with value 1
#' windraster <-raster::rasterize(Polygon1, raster::raster(
#'                                raster::extent(Polygon1),
#'                                ncol=180, nrow=180),field=1)
#'
#' ## Create a uniform and unidirectional wind data.frame and plot the
#' ## resulting wind rose
#' data.in <- as.data.frame(cbind(ws=12,wd=0))
#' windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
#'                 dir = data.in$wd, dirres=10, spdmax=20)
#'
#' ## Assign the rotor radius and a factor of the radius for grid spacing.
#' Rotor= 50; fcrR= 3
#' resGrid <- GridFilter(shape = Polygon1,resol = Rotor*fcrR, prop=1,
#'                       plotGrid =TRUE)
#' ## Assign the indexed data frame to new variable. Element 2 of the list
#' ## is the grid, saved as SpatialPolygon.
#' resGrid1 <- resGrid[[1]]
#'
#' ## Create an initial population with the indexed Grid, 15 turbines and
#' ## 100 individuals.
#' resStartGA <- StartGA(Grid = resGrid1,n = 15,nStart = 100)
#'
#' ## Calculate the expected energy output of the first individual of the
#' ## population.
#' par(mfrow=c(1,2))
#' plot(Polygon1); points(resStartGA[[1]]$X,resStartGA[[1]]$Y, pch=20,cex=2)
#' plot(resGrid[[2]],add=TRUE)
#' resCalcEn <-calculateEn(sel=resStartGA[[1]],referenceHeight= 50,
#'                    RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
#'                    distanz = 100000, resol = 200,dirSpeed = data.in,
#'                    RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
#'                    windraster = windraster)
#' length(resCalcEn)
#' str(resCalcEn)
#' resCalcEn <- as.data.frame(resCalcEn)
#' plot(Polygon1, main = resCalcEn$Energy_Output_Red[[1]])
#' points(x = resCalcEn$Bx, y = resCalcEn$By, pch = 20)
#'
#'
#' ## Create a variable and multidirectional wind data.frame and plot the
#' ## resulting wind rose
#' data.in10 <- as.data.frame(cbind(ws=runif(10,1,25),wd=runif(10,0,360)))
#' windrosePlot <- plotWindrose(data = data.in10, spd = data.in10$ws,
#'                 dir = data.in10$wd, dirres=10, spdmax=20)
#'
#' ## Calculate the energy outputs for the first individual with more than one
#' ## wind direction.
#' resCalcEn <-calculateEn(sel=resStartGA[[1]],referenceHeight= 50,
#'                    RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
#'                    distanz = 100000, resol = 200,dirSpeed = data.in10,
#'                    RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
#'                    windraster = windraster)
#' length(resCalcEn)
#' str(resCalcEn)
#'
#' ## Take Weibull Paramter Raster from the package. (Only for Austria)
#' plot(Polygon1); points(resStartGA[[1]]$X,resStartGA[[1]]$Y, pch=20,cex=2)
#' plot(resGrid[[2]],add=TRUE)
#' resCalcEn <-calculateEn(sel=resStartGA[[1]], referenceHeight=50,
#'                         RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
#'                         distanz = 100000, resol = 200,dirSpeed = data.in,
#'                         RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
#'                         windraster = windraster, weibull = TRUE)
#' length(resCalcEn)
#' str(resCalcEn)
#' resCalcEn <- as.data.frame(resCalcEn)
#' plot(Polygon1, main = resCalcEn$Energy_Output_Red[[1]])
#' points(x = resCalcEn$Bx, y = resCalcEn$By, pch = 20)
#'
#' ## Use your own rasters for the Weibull parameters.
#' araster <- "/..pathto../a_param_raster.tif"
#' kraster <- "/..pathto../k_param_raster.tif"
#' weibullrasters <- list(raster(kraster), raster(araster))
#' plot(Polygon1); points(resStartGA[[1]]$X,resStartGA[[1]]$Y, pch=20,cex=2)
#' plot(resGrid[[2]],add=TRUE)
#' resCalcEn1 <-calculateEn(sel=resStartGA[[1]], referenceHeight= 50,
#'                          RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
#'                          distanz = 100000, resol = 200,dirSpeed = data.in,
#'                          RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
#'                          windraster = windraster, weibull = TRUE,
#'                          weibullsrc = weibullrasters)
#' length(resCalcEn1)
#' str(resCalcEn1)
#' resCalcEn1 <- as.data.frame(resCalcEn1)
#' plot(Polygon1, main = resCalcEn1$Energy_Output_Red[[1]])
#' points(x = resCalcEn1$Bx, y = resCalcEn1$By, pch = 20)
#' }
#' @author Sebastian Gatscha
#'
calculateEn       <- function(sel, referenceHeight, RotorHeight,SurfaceRoughness,
                              windraster,wnkl,distanz, polygon1,resol,RotorR,dirSpeed,
                              srtm_crop,topograp,cclRaster, weibull, weibullsrc){

  PlotCalc <- FALSE

  sel1 <- sel[,2:3];
  ## Assign constant values
  cT <- 0.88;   air_rh <- 1.225;   k = 0.075;
  ## Extract values from windraster, which will be 1 in this case, as they will get multiplied
  ## by the incoming wind speed.
  windpo <- raster::extract(x= windraster, y = as.matrix((sel1)), 
                            buffer=resol*1, small=T,fun= mean,na.rm=T);


  ## Terrain Effect Model:
  if (topograp == TRUE) {
    ## Calculates Wind multiplier. Hills will get higher values, valleys will get lower values.
    orogr1 <- raster::calc(srtm_crop, function(x) {x/(raster::cellStats(srtm_crop,mean,na.rm=T))})
    orogrnum <- raster::extract(x= orogr1, y = as.matrix((sel1)), buffer=resol*2, small=T,fun= mean,na.rm=T);
    windpo <- windpo * orogrnum

    ## Get Elevation of Turbine Locations to estimate the air density at the resulting height
    heightWind <- raster::extract(x= srtm_crop, y = as.matrix((sel1)), small=T,fun= max,na.rm=T);
    ## Plot the elevation and the wind speed multiplier rasters
    if (PlotCalc==TRUE){
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
    if (PlotCalc==TRUE){
      par(mfrow=c(1,1))
      plot(srtm_crop, main="Normal Air Density",col=topo.colors(10));points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = rep(1.225,nrow(sel1)),cex=0.8);plot(polygon1,add=T)
      raster::plot(srtm_crop, main="Corrected Air Density",col=topo.colors(10));points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round(air_dt$rh,2),cex=0.8);plot(polygon1,add=T)
    }

    ## Corine Land Cover Surface Roughness values and Elevation Roughness
    SurfaceRoughness0 <- raster::extract(x= cclRaster, y = as.matrix((sel1)),buffer=resol*2,
                                         small=T,fun= mean,na.rm=T);
    SurfaceRoughness1 <- raster::extract(x=raster::terrain(srtm_crop,"roughness"), y = as.matrix((sel1)),
                                         buffer=resol*2, small=T,fun= mean,na.rm=T);
    SurfaceRoughness <-SurfaceRoughness*(1+(SurfaceRoughness1/max(raster::res(srtm_crop))));
    elrouind <- raster::terrain(srtm_crop,"roughness")
    elrouindn <- raster::resample(elrouind,cclRaster,method="ngb")
    modSurf <- raster::overlay(x = cclRaster,y = elrouindn, fun=function(x,y){return(x*(1+y/max(raster::res(srtm_crop))))})
    ## Plot the different Surface Roughness Values
    if (PlotCalc==TRUE){
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
    if (PlotCalc==TRUE){
      graphics::par(mfrow=c(1,1)); cexa=0.9
      plot(x= raster::terrain(srtm_crop,"roughness",neighbors = 4),
                     main="Adapted Wake Decay Values - K");
      graphics::points(sel1$X,sel1$Y,pch=20);
      calibrate::textxy(sel1$X,sel1$Y,labs = round((k),3),cex=cexa);
      plot(polygon1,add=T)
    }

  }

  
  if (missing(weibull)) {
    weibull=F
  }
  if (weibull==T){
    if (missing(weibullsrc) | is.null(weibullsrc)) {
      cat("\nWeibull Informations from package will be used.\n")
      path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
      k_param = ""
      a_param = ""
      k_weibull <- readRDS(file = paste0(path, "k_weibull.RDS"))
      a_weibull <- readRDS(file = paste0(path, "a_weibull.RDS"))
      weibullsrc = list(k_weibull, a_weibull)
      }
  }

  ## For every wind direction, calculate the energy output. Do so by rotating Polygon for all angles and
  ## analyze, which turbine is affected by another one to calculate total energy output.
  ## Save Output in a list.
  alllist <- vector("list",nrow(dirSpeed))
  for (index in 1:nrow(dirSpeed)) {
    ## Get the Coordinates of the individual / wind farm
    xyBgldMa <- as.matrix((sel1));

    ## Get mean windspeed for every turbine location from windraster
    pointWind <- windpo * dirSpeed$ws[index]

    if (weibull==T){
      k_param <- weibullsrc[[1]]
      a_param <- weibullsrc[[2]]
    
      Erwartungswert <- a_param * (gamma(1 + (1/k_param)))
      Erwartungswert <- projectRaster(Erwartungswert, crs = proj4string(polygon1))

      if (PlotCalc == TRUE){
          weibl_k <- projectRaster(k_param, crs = proj4string(polygon1))
          weibl_a <- projectRaster(a_param, crs = proj4string(polygon1))
          par(mfrow=c(3,1), ask=F)
          plot(Erwartungswert, main="Mean Weibull"); plot(polygon1, add=T)
          plot(weibl_a, main="A Weibull Parameter"); plot(polygon1, add=T)
          plot(weibl_k, main="K Weibull Parameter"); plot(polygon1, add=T)
      }

      Erwartungswertxy <- raster::extract(x= Erwartungswert, y = as.matrix((sel1)),
                                          buffer=resol*1, small=T,fun= mean,na.rm=T);
      pointWind <- windpo * Erwartungswertxy

      # cat(pointWind)
    }

    ## Calculate Windspeed according to Rotor Height using wind profile law. Other law possible with
    ## log MISSING:
    pointWind <- pointWind*((RotorHeight/referenceHeight)^SurfaceRoughness);
    pointWind[is.na(pointWind)] <- 0;

    ## Get the current incoming wind direction and assign to "angle"
    angle <- -dirSpeed$wd[index];

    ## If activated, plots the turbine locations with angle 0 and opens a second frame for rotated
    ## turbine lovations
    if (PlotCalc == TRUE){
      par(mfrow=c(1,2))
      plot(polygon1, main="Shape at angle 0");
      graphics::points(xyBgldMa[,1],xyBgldMa[,2],pch=20)
      calibrate::textxy(xyBgldMa[,1],xyBgldMa[,2], labs = dimnames(xyBgldMa)[[1]],cex=0.8)
      Polygon3 = maptools::elide(polygon1, rotate=angle, center=apply(sp::bbox(polygon1), 1, mean));
      plot(Polygon3, main=c("Shape at angle:", (-1*angle)))
      graphics::mtext(paste("Direction: ", index, "\nfrom total: ", nrow(dirSpeed)), side = 1)
    }

    ## Change Coordinates to Spatial Points and rotate them by the incoming wind direction
    ## and rearrange as coordinates again
    xyBgldMa <- sp::SpatialPoints(sp::coordinates(xyBgldMa))
    xyBgldMa <- maptools::elide(xyBgldMa, rotate=angle, center=apply(sp::bbox(polygon1), 1, mean));
    xyBgldMa <- sp::coordinates(xyBgldMa)

    ## If activated, plots the rotated turbines in red.
    if (PlotCalc == TRUE){
      graphics::points(xyBgldMa, col="red",pch=20)
    }

    ## If Height is taken into account. 3D Modelling of Wake and Overlapping Areas
    DatFram <- as.data.frame(cbind(pointWind,xyBgldMa)); colnames(DatFram)=c("Windmittel","X","Y");

    ## Get the influecing points given with incoming wind direction angle and reduce then to data frame
    BgleInf <- InfluPoints(t = xyBgldMa,wnkl =  wnkl, dist = distanz, polYgon = polygon1, dirct = angle)
    dfAll <- do.call("rbind",BgleInf) ;

    ## Create a list for every turbine
    windlist <- vector("list",length(sel1[,1]))

    ## Assign Windspeed to a filtered list with all turbines and add the desired rotor radius to the
    ## data frame
    maxpo <- max(dfAll$Punkt_id)
    for (i in 1:maxpo){
      windlist[[i]] <- dplyr::filter(dplyr::select(dplyr::tbl_df(dfAll), Punkt_id,
                                                   Ax,Ay,Bx,By,Laenge_B,Laenge_A,alpha,
                                                   Windrichtung), Punkt_id==i)
      windlist[[i]]$Windmean <- DatFram[[1]][i];
    }
    windlist <- do.call("rbind", windlist);
    windlist$RotorR <- as.numeric(RotorR);

    ## Calculate the wake Radius and the rotor area for every turbine.
    lnro <- nrow(windlist); windlist$WakeR <- 0; windlist$Rotorflaeche <- 0
    for (i in 1:lnro){
      RotD <- as.numeric(windlist[i,]$RotorR)
      if (windlist[i,]$Laenge_B != 0) {

        if (topograp==TRUE){
          windlist[i,]$WakeR <- (((RotD * 2) + (2*k[windlist[i,]$Punkt_id]*
                                                 (as.numeric(windlist[i,]$Laenge_B))))/2)[1]
        } else {
          windlist[i,]$WakeR <- (((RotD * 2) + (2*k*(as.numeric(windlist[i,]$Laenge_B))))/2)[1]
        }
      } else {
        windlist[i,]$WakeR = 0
      }
      windlist[i,]$Rotorflaeche <- (RotD^2) *pi
    };

    ## Calculate the overlapping area and the overlapping percentage.
    windlist$A_ov <- 0; windlist$AbschatInProz <- 0
    for (o in 1:lnro){
      Rotorf <- as.numeric(windlist[o,]$RotorR)
      leA <- windlist[o,]$Laenge_A
      wakr <- windlist[o,]$WakeR;
      if (windlist[o,]$Laenge_B == 0) {
        windlist[o,]$A_ov <- 0;
      } else {
        if ((wakr - Rotorf) >= leA && leA >= 0) {
          windlist[o,]$A_ov <- as.numeric(windlist[o,]$RotorR^2)*pi;
        }
        if (round((wakr + Rotorf),2) <= round(leA,2)) {
          windlist[o,]$A_ov <- 0
        }
        if ((wakr - Rotorf) <= leA && leA <= (wakr+Rotorf))  {
          windlist[o,]$A_ov <- (Rotorf^2 * round(acos((Rotorf^2 - wakr^2 + leA^2) / (2*leA * Rotorf)),4))+
            (wakr^2 * round(acos((wakr^2 - Rotorf^2 + leA^2) / (2*leA * wakr)),4)) -
            ((1/2)*sqrt( round((Rotorf + wakr + leA),6) * round((-Rotorf + wakr + leA ),6)*
                           round((Rotorf - wakr + leA),6) * round((Rotorf + wakr - leA),6)))
        }
      }
      if (windlist[o,]$A_ov != 0) {
        windlist[o,]$AbschatInProz <- round(((as.numeric(windlist[o,]$A_ov)/
                                                windlist[o,]$Rotorflaeche)*100), 2);
      } else {
        windlist[o,]$AbschatInProz <- 0;
      }

    }

    ## Calculate the wind velocity reduction.
    windlist$V_red <- 0
    for (p in 1:lnro) {
      RotrR <- windlist[p,]$RotorR
      a <- 1- sqrt(1-cT)
      s <- (windlist[p,]$Laenge_B/RotrR);
      if (topograp==TRUE){
        b <- (1 + (k[windlist[p,]$Punkt_id]*s))^2;
      } else {
        b <- (1 + (k*s))^2;
      }
      aov <- (windlist[p,]$A_ov / windlist[p,]$Rotorflaeche);
      windlist[p,]$V_red <- (aov *(a / b))
      ve <- windlist[p,]$Windmean * windlist[p,]$V_red;
      windlist[p,]$V_red <- ve
    }

    ## Calculate multiple wake effects, total wake influence, the new resulting wind velocity
    ## and add the Grid IDs.
    maPi <- max(windlist$Punkt_id)
    windlist$V_i <- 0; windlist$TotAbschProz <- 0; windlist$V_New <- 0; windlist$Rect_ID <- 0
    for (z in 1:maPi) {
      windlist[windlist$Punkt_id==z,]$V_i <-  sqrt(sum(windlist[windlist$Punkt_id==z,]$V_red^2))
      windlist[windlist$Punkt_id==z,]$TotAbschProz <-  sum(windlist[windlist$Punkt_id==z,]$AbschatInProz)
      windlist[windlist$Punkt_id==z,]$V_New <-  windlist[windlist$Punkt_id==z,]$Windmean -
        windlist[windlist$Punkt_id==z,]$V_i
      windlist[windlist$Punkt_id==z,]$Rect_ID <-  sel[z,1]
    }

    ## Get a reduced dataframe and split duplicated Point_id, since a turbine with fixed Point_id,
    ## can have several influencing turbines and therefore several data frame elements
    windlist2 <- dplyr::select(windlist,-alpha,-AbschatInProz,-Rotorflaeche,-V_red,-V_i)
    windlist1 <- split(windlist2, duplicated(windlist2$Punkt_id))$'FALSE'

    ## Calculate Full and reduced Energy Outputs in kW and Park Efficienca in %. Assign the values to
    ## the list
    EneOutRed <- sum(0.593 * (1/2) * air_rh * (windlist1$V_New ^ 3) *
                       ((as.numeric(windlist1$RotorR)^2)*pi),na.rm=T)/1000;
    EneOutFul <- sum(0.593 * (1/2) * air_rh * (windlist1$Windmean^3)*
                       ((as.numeric(windlist1$RotorR)^2)*pi),na.rm=T)/1000;
    Effic <- (EneOutRed*100)/EneOutFul;
    windlist2$Energy_Output_Red <- EneOutRed; windlist2$Energy_Output_Voll <- EneOutFul;
    windlist2$Parkwirkungsgrad <- Effic;
    windlist2$Windrichtung <- as.numeric(windlist2$Windrichtung) * (-1)
    alllist[[index]] <- windlist2
  }

  ## Return the list with all relevant information
  invisible(alllist)
}
