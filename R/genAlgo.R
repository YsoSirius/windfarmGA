#' @title Start The Genetic Algorithm
#' @name genAlgo
#' @description  The method which coordinates all other elements of the
#' genetic algorithm. To initiate an optimization run, this method has to
#' be called with the desired inputs. To activate the terrain effect
#' model, the sources of the Corine Land cover raster and the adapted
#' legend csv file have to be assigned.
#'
#' @export
#'
#' @importFrom raster crs getData crop mask projectRaster reclassify
#' @importFrom sp spTransform
#' @importFrom utils read.csv
#' @importFrom grDevices colorRampPalette
#' @importFrom dplyr select group_by summarise_each %>% funs
#'
#'
#' @param Polygon1 The considered area as shapefile (SpatialPolygons)
#' @param Rotor A numeric value that gives the rotor radius in meter
#' (numeric)
#' @param n A numeric value indicating the required amount of turbines
#' (numeric)
#' @param fcrR A numeric value, that is used for grid spacing (numeric)
#' @param referenceHeight The height at which the incoming
#' wind speeds were measured. Default is 50m (numeric)
#' @param RotorHeight The desired height of the turbine.
#' Default is 100m (numeric)
#' @param SurfaceRoughness A surface roughness length of the
#' considered area in m. (numeric)
#' @param Proportionality A numeric factor used for grid calculation.
#' Determines the percentage a grid has to overlay (numeric)
#' @param iteration A numeric value indicating the desired amount
#' of iterations of the algorithm (numeric)
#' @param mutr A numeric mutation rate, with low default value of 0.008
#' (numeric)
#' @param vdirspe A data.frame containing the incoming wind speeds,
#' wind directions and probabilities (data.frame)
#' @param topograp Boolean Value, which indicates if the terrain effect model
#'  should be activated or not. (character)
#' @param elitism Boolean Value, which indicates whether elitism should
#' be included or not. (character)
#' @param nelit If \code{elitism} is "TRUE", then this input variable
#' determines the amount of individuals in the elite group. (numeric)
#' @param selstate Determines which selection method is used,
#' "FIX" selects a constant percentage and "VAR" selects a variable percentage,
#' depending on the development of the fitness values. (character)
#' @param crossPart1 Determines which crossover method is used,
#' "EQU" divides the genetic code at equal intervals and
#' "RAN" divides the genetic code at random locations. (character)
#' @param trimForce If activated (\code{trimForce}=="TRUE"),
#' the algorithm will take a probabilistic approach to trim the windfarms
#' to the desired amount of turbines. If \code{trimForce}=="FALSE" the
#' adjustment will be random. Default is "TRUE". (character)
#'
#' @return The result of this run is a matrix of all relevant output
#' parameters. This output can be used for several plotting functions.
#' (matrix)
#'
#' @examples \donttest{
#' ## Create a random rectangular shapefile
#' library(sp)
#' Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' plot(Polygon1,axes=T)
#'
#' ## Create a uniform and unidirectional wind data.frame and plots the
#' ## resulting wind rose
#' ## Uniform wind speed and single wind direction
#' data.in <- as.data.frame(cbind(ws=12,wd=0))
#' # windrosePlot <- plotWindrose(data = data.in, spd = data.in$ws,
#' #                dir = data.in$wd, dirres=10, spdmax=20)
#'
#' ## Runs an optimization run for 10 iterations (iteration) with the
#' ## given shapefile (Polygon1), the wind data.frame (data.in),
#' ## 12 turbines (n) with rotor radii of 30m (Rotor) and a grid spacing
#' ## factor of 3 (fcrR)
#' # result <- genAlgo(Polygon1 = Polygon1, n=12, Rotor=30,fcrR=3,iteration=10,
#' #          vdirspe = data.in)
#'}
#' @author Sebastian Gatscha

genAlgo           <- function(Polygon1, Rotor, n, fcrR,referenceHeight, RotorHeight,SurfaceRoughness,
                              Proportionality, iteration, mutr, vdirspe, topograp,
                              elitism, nelit, selstate, crossPart1,trimForce){
  oldpar <- graphics::par(no.readonly = T)
  #plot.new();
  graphics::par(ask=F);

  resol2 <- fcrR*Rotor
  CrossUpLimit = 300

  inputData <- list(Input_Data=rbind("Rotorradius"=Rotor,"Number of turbines"=n,"Grid Shape Factor"= fcrR,
                                     "Iterations"=iteration,"Mutation Rate"=mutr,
                                     "Percentage of Polygon"=Proportionality,"Topographie"=topograp,
                                     "Elitarism"=elitism, "Selection Method"=selstate,
                                     "Trim Force Method Used"=trimForce,"Crossover Method Used"=crossPart1,
                                     "Reference Height"= referenceHeight, "Rotor Height"=RotorHeight,
                                     "Resolution" = resol2));
  inputWind <- list(Windspeed_Data=vdirspe)

  print(inputData);print(inputWind)

  ProjLAEA = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
              +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  if (as.character(raster::crs(Polygon1)) != ProjLAEA) {
    Polygon1 <- sp::spTransform(Polygon1, CRSobj = ProjLAEA)
  }
  if  (crossPart1!= "EQU" & crossPart1 !="RAN") {
    crossPart1 <- readinteger()
  }

  Grid1 <- GridFilter(shape = Polygon1,resol = resol2,prop = Proportionality);
  Grid <- Grid1[[1]]
  dry.grid.filtered <- Grid1[[2]]
  AmountGrids <- nrow(Grid)

  nStart = (AmountGrids*n)/iteration;   if (nStart < 100) {nStart = 100};   if (nStart > 300) {nStart = 300}
  nStart<- ceiling(nStart);
  startsel <- StartGA(Grid,n,nStart);

  maxParkwirkungsg = 0; allparkcoeff <- vector("list",iteration);
  bestPaEn <- vector("list",iteration);
  bestPaEf <- vector("list",iteration); fuzzycontr <- vector("list",iteration);
  fitnessValues <- vector("list",iteration);
  nindiv <- vector("list",iteration); clouddata <- vector("list",iteration);
  selcross <- vector("list",iteration);
  beorwor <- vector("list",iteration); mut_rate <- vector("list",iteration);
  allCoords <- vector("list",iteration);
  if (topograp == "FALSE"){
    print("Topography and orography are not taken into account.")
  } else if (topograp == "TRUE"){
    print("Topography and orography are taken into account.")

    par(mfrow=c(3,1))
    ## SRTM Daten
    Polygon1 <-  sp::spTransform(Polygon1, CRSobj =
                                   raster::crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"));
    extpol <- round(Polygon1@bbox,0)[,2]
    srtm <- raster::getData('SRTM', lon=extpol[1], lat=extpol[2]);
    srtm_crop <- raster::crop(srtm, Polygon1);
    srtm_crop <- raster::mask(srtm_crop, Polygon1)

    # Get the estimated mean wind speed value for Austria only
    #     rassou <- "C:/Users/Bobo/Documents/STUDIUM/_____WS_2015_16/int_SeminarWind/INT_Seminar/windatlas_nachbauen/Erwartungswert.gri"
    #     windraster <- raster::raster(rassou)
    #     Polygon12 <-  sp::spTransform(Polygon1, CRSobj = raster::crs(windraster));
    #     windcrop <- raster::crop(windraster, Polygon12);   windcrop <- raster::mask(windcrop, Polygon12);
    #     windraster <- raster::projectRaster(windcrop, raster::crs = crs(ProjLAEA));
    #     plot(windraster, main="Mean Wind Speed in m/s"):
    #

    Polygon1 <-  sp::spTransform(Polygon1, CRSobj = raster::crs(ProjLAEA));
    srtm_crop <- raster::projectRaster(srtm_crop, crs = raster::crs(ProjLAEA));
    plot(srtm_crop, main="Elevation from SRTM");
    plot(Polygon1,add=T); plot(dry.grid.filtered,add=T)

    # INclude Corine Land Cover Raster to get an estimation of Surface Roughness
    ccl <- raster::raster(sourceCCL)
    cclPoly <- raster::crop(ccl,Polygon1)
    cclPoly1 <- raster::mask(cclPoly,Polygon1)
    rauhigkeitz <- utils::read.csv(sourceCCLRoughness,header = T,sep = ";");
    cclRaster <- raster::reclassify(cclPoly1, matrix(c(rauhigkeitz$GRID_CODE,rauhigkeitz$Rauhigkeit_z),ncol = 2))
    plot(cclRaster, main="Surface Roughness from Corine Land Cover")

  }


  rbPal <- grDevices::colorRampPalette(c('red','green'))
  i=1
  while (i <= iteration) {
    if (i==1) {
      fit <- fitness(selection = startsel,referenceHeight, RotorHeight,SurfaceRoughness,
                     Polygon = Polygon1,resol1 = resol2,rot=Rotor, dirspeed = vdirspe,
                     srtm_crop,topograp,cclRaster)

    } else {
      getRectV <- getRects(mut1, Grid)
      fit <- fitness(selection = getRectV,referenceHeight, RotorHeight,SurfaceRoughness,
                     Polygon = Polygon1,resol1 = resol2,rot = Rotor, dirspeed = vdirspe,
                     srtm_crop,topograp,cclRaster)

    }

    allparks <- do.call("rbind",fit);
    allparksUni <- split(allparks, duplicated(allparks$Run))$'FALSE';
    maxparkfitness <-  round(max(allparksUni$Parkfitness),4);
    meanparkfitness <- round(mean(allparksUni$Parkfitness),3);
    minparkfitness <- round(min(allparksUni$Parkfitness),3);
    MaxEnergyRedu <-  round(max(allparksUni$EnergyOverall),2);
    MeanEnergyRedu <- round(mean(allparksUni$EnergyOverall),2);
    MinEnergyRedu <- round(min(allparksUni$EnergyOverall),2);
    allCoords[[i]] <- allparks
    maxParkwirkungsg <- round(max(allparksUni$EfficAllDir),2);
    meanParkwirkungsg <- round(mean(allparksUni$EfficAllDir),2);
    minParkwirkungsg <- round(min(allparksUni$EfficAllDir),2);
    allparkcoeff[[i]] <- cbind(maxparkfitness,meanparkfitness,minparkfitness, MaxEnergyRedu,
                               MeanEnergyRedu,MinEnergyRedu,maxParkwirkungsg,meanParkwirkungsg,minParkwirkungsg)
    clouddata[[i]] <- dplyr::select(allparksUni,EfficAllDir,EnergyOverall,Parkfitness);
    cat(c("\n\n", i, ": Round with coefficients ", allparkcoeff[[i]], "\n"));

    ## Highest Energy Output
    xd <- allparks[allparks$EnergyOverall==max(allparks$EnergyOverall),]$EnergyOverall[1];
    ind <- allparks$EnergyOverall == xd;     bestPaEn[[i]] <- allparks[ind,][1:n,]
    ## Highest Efficiency
    xd1 <- allparks[allparks$EfficAllDir==max(allparks$EfficAllDir),]$EfficAllDir[1];
    ind1 <- allparks$EfficAllDir == xd1;     bestPaEf[[i]] <- allparks[ind1,][1:n,]
    # Print out most relevant information on Generation i
    afvs <- allparks[allparks$EnergyOverall==max(allparks$EnergyOverall),];
    cat(paste("How many individuals exist: ",  length(fit) ), "\n");
    cat(paste("How many parks are in local Optimum: ",  (length(afvs[,1])/n) ), "\n")
    nindivfit <- length(fit)

    lebre <- length(unique(bestPaEn[[i]]$AbschGesamt))
    if (lebre < 2){
      Col <- "green";      Col1 <- "green"
    } else {
      Col <- rbPal(lebre)[as.numeric(cut(-bestPaEn[[i]]$AbschGesamt,breaks = lebre))];
      Col1 <- rbPal(lebre)[as.numeric(cut(-bestPaEf[[i]]$AbschGesamt,breaks = lebre))]
    }

    x = round(bestPaEn[[i]]$EnergyOverall[[1]],2);y = round(bestPaEn[[i]]$EfficAllDir[[1]],2);
    e = bestPaEn[[i]]$EfficAllDir;
    x1 = round(bestPaEf[[i]]$EnergyOverall[[1]],2);y1 = round(bestPaEf[[i]]$EfficAllDir[[1]],2);
    e1 = bestPaEf[[i]]$EfficAllDir

    ## ALLPARKS RECT ID NOT CORRECT
    allparksNewplot <- dplyr::select(allparks,AbschGesamt,Rect_ID,Parkfitness);
    allparksNewplot <- allparksNewplot %>% dplyr::group_by(Rect_ID) %>%
                      dplyr::summarise_each(dplyr::funs(mean));
    if(any(allparksNewplot$Rect_ID %in% Grid$ID == F)){
      print(paste("Index of Grid not correct. Bigger than maximum Grid? Fix BUG"))
      break()
    }

    graphics::par(mfrow=c(1,2))
    plot(Polygon1, main=paste(i, "Round \n Best Energy Output: ", x,"\n Efficiency: ", y ),
         sub =paste("\n Number of turbines: ", length(e)));    plot(dry.grid.filtered, add=T)
    graphics::points(bestPaEn[[i]]$X,bestPaEn[[i]]$Y,col=Col,pch=20,cex=1.5);
    plot(Polygon1, main=paste(i, "Round \n Best Efficiency Output: ", x1, "\n Efficiency: ", y1 ),
         sub =paste("\n Number of turbines: ", length(e1)));  plot(dry.grid.filtered, add=T)
    graphics::points(bestPaEf[[i]]$X,bestPaEf[[i]]$Y,col=Col1,pch=20,cex=1.5)


    if (i > 20) {
      besPE <- do.call("rbind",lapply(bestPaEn[1:i], function(x) max(x$EnergyOverall)))
      maxBisher <- max(besPE); WhichMaxBs <- which(besPE==max(besPE))

      if (length(WhichMaxBs) >= 2) {
        BestForNo <- bestPaEn[sample(WhichMaxBs,2)]
        BestForNo[[1]]$Run <- length(fit)+1
        BestForNo[[2]]$Run <- length(fit)+2
      } else {
        BestForNo <- bestPaEn[WhichMaxBs]
        BestForNo <- append(BestForNo, BestForNo)
        BestForNo[[1]]$Run <- length(fit)+1
        BestForNo[[2]]$Run <- length(fit)+2
      }

      last7 <- besPE[i:(i-5)]
      if (!any(last7==maxBisher)){
        cat(paste("Park with highest Fitness level to date is replaced in the list.", "\n\n"))
        fit <- append(fit, BestForNo)
      }
    }

    if (i==1) {
      t0 <- split(allparks, duplicated(allparks$Run))$'FALSE'
      t0 <- t0$Parkfitness;       fitnessValues[[i]] <- t0
      rangeFitnessVt0 <- range(t0);rangeFitnessVt0
      maxt0 <- max(t0);maxt0;       meant0 <- mean(t0);
      allcoef0 <- c(rangeFitnessVt0, meant0); cat(c( i, ": Round: ", allcoef0, "\n"));
      fuzzycontr[[i]] <- rbind(allcoef0); colnames(fuzzycontr[[i]]) <- c("Min","Max","Mean")
      teil=2
      if (selstate=="VAR"){
        teil=1.35
      }
      u = 1.1
      beorwor[[i]] <- cbind(0,0)
    }
    if (i>=2 && i <= iteration) {
      t0 <- split(allparks, duplicated(allparks$Run))$'FALSE';       t0 <- t0$Parkfitness;t0
      fitnessValues[[i]] <- t0;       rangeFitnessVt0 <- range(t0);rangeFitnessVt0
      maxt0 <- max(t0);      meant0 <- mean(t0); mint0 <- min(t0)
      t1 <- fitnessValues[[i-1]];t1;       rangeFitnessVt1 <- range(t1);rangeFitnessVt1
      maxt1 <- max(t1);     meant1 <- mean(t1); mint1 <- min(t1)
      maxDif <- maxt0 - maxt1; meanDif <- meant0 - meant1; minDif = mint0 - mint1
      WeightDif <- c(0.80,0.2,0.0)
      maxunt <- (maxDif*WeightDif[1])+(meanDif*WeightDif[2])+(minDif*WeightDif[3])
      allcoef1 <- c(rangeFitnessVt0, meant0); allcoef2 <- c(rangeFitnessVt1, meant1);
      fuzzycontr[[i]] <- rbind(allcoef1,allcoef2); colnames(fuzzycontr[[i]]) <- c("Min","Max","Mean")

      if(maxunt<0) {
        pri="declined";teil=teil-0.02; u=u-0.06} else if (maxunt==0) {
          pri="not changed"; teil=teil; u=u} else {
            pri="enhanced"; teil=teil+0.017; u=u+0.03}

      if (teil > 5){teil=5; u=u+0.09; print("Min 20% Selected");print(paste("u is increased! u:",u,
                                                                            "teil: ",teil))}
      if (trunc(u) < 0){u = 0.5;teil=teil-0.4; print(paste("Min 1 CrossPoints. Selection decreased. u:",
                                                           u,"teil: ",teil))}
      if (u >= 4){u=4;teil=4; print(paste("Max 5 CrossPoints. Select fittest 25%. teil: ",teil))}
      if (teil <= 4/3){teil = 4/3; print(paste("Max 75% selected. teil: ",teil))}
      if (length(fit) <= 20) {teil=1;u=u+0.07; print(paste("Less than 20 individuals. Select all and
                                                increase Crossover-point rate. u: ", u,"teil", teil))}
      if (teil > 5){teil=5; print(paste("Teil is bigger than 5. Set to max 5. teil:",teil))}


      u = round(u,2); teil=round(teil,3);

      print(paste("Fitness of this population (", i,") compared to the prior population: ",pri , maxunt))
      meanunt <- meant0-meant1; if(meanunt<0) {pri="declined"} else if (meanunt==0) {pri="not changed"}
      else {pri="enhanced"}
      beorwor[[i]] <- cbind(maxunt, meanunt)
    }

    if (selstate=="FIX"){
      if (teil==1){teil=1} else {teil=2}
    }
    if (crossPart1=="EQU"){
      u=round(u,2)
    }

    ## How many are selected and how much crossover points are used?
    selcross[[i]] <- cbind(cross=trunc(u+1),teil)

    ## SELECTION
    ## print the amount of Individuals selected. Check if the amount of Turbines is as requested.
    selec6best <- selection1(fit, Grid,teil, elitism, nelit, selstate);
    selec6best_bin <- selec6best[[1]]
    print(paste("Selection. Amount of Individuals: ",length(selec6best_bin[1,-1])))
    Trus1 <- colSums(selec6best_bin)[-1] == n
    if (any(Trus1 == FALSE)){
      print("Number of turbines is not as required. Trus1. Fix BUG")
      break()
    }
    nindivsel<- length(selec6best_bin[1,-1]);

    ## CROSSOVER
    ## u determines the amount of crossover points, crossPart det. the method used (Equal/Random),
    ## uplimit is the maximum allowed permutations
    crossOut <- crossover1(selec6best, u, uplimit = CrossUpLimit, crossPart=crossPart1) ;
    print(paste("Crossover. Amount of Individuals: ",length(crossOut[1,])));
    nindivcros<- length(crossOut[1,]);

    ## MUTATION
    ## Variable Mutation Rate is activated if more than 2 individuals represent the current best solution.
    loOp <- (length(afvs[,1])/n)
    if (loOp > 2) {
      mutrn <- round(runif(1, 0.03, 0.1),2);
      t1 <- (loOp*1.25)/42
      mutrn <- mutrn * (1+(t1));
      mutrn <- round(mutrn +((i)/(20*iteration)),5);
      mut <- mutation(a = crossOut, p = mutrn);
      mut_rat <- mutrn
      cat(paste("1. Mutation Rate is", mutrn, "\n\n"))
    } else {
      mut <- mutation(a = crossOut, p = mutr);
      mut_rat <- mutr
    }
    mut_rate[[i]] <- mut_rat
    print(paste("Mutation. Amount of Individuals: ",length(mut[1,])));
    nindivmut <- length(mut[1,]);nindivmut

    ## TRIMTON
    ## After Crossover and Mutation, the amount of turbines in a windpark change and have to be
    ## corrected to the required amount of turbines.
    mut1 <- trimton(mut = mut, nturb = n, allparks = allparks, nGrids = AmountGrids,trimForce=trimForce)
    print(paste("TrimToN. Amount of Individuals: ",length(mut1[1,])))
    Trus3 <- colSums(mut1) == n
    if (any(Trus3 == FALSE)){
      print(paste("Number of turbines is not as required. Trus3. Fix Bug. Amount:",
                  length(Trus3[Trus3==FALSE])))
      break()
    }

    nindiv[[i]] <- cbind(nindivfit,nindivsel,nindivcros,nindivmut)
    if (maxParkwirkungsg == 100) {
      i = iteration + 1
    } else {
      i = i+1
    }
  }

  mut_rate <- mut_rate[lapply(mut_rate,length)!=0];
  beorwor <- beorwor[lapply(beorwor,length)!=0] ;
  selcross <- selcross[lapply(selcross,length)!=0] ;
  clouddata <- clouddata[lapply(clouddata,length)!=0];
  allparkcoeff <- allparkcoeff[lapply(allparkcoeff,length)!=0]
  bestPaEn <- bestPaEn[lapply(bestPaEn,length)!=0] ;
  bestPaEf <- bestPaEf[lapply(bestPaEf,length)!=0] ;
  fuzzycontr <- fuzzycontr[lapply(fuzzycontr,length)!=0];
  fitnessValues <- fitnessValues[lapply(fitnessValues,length)!=0];
  nindiv <- nindiv[lapply(nindiv,length)!=0]
  allCoords <- allCoords[lapply(allCoords,length)!=0] ;

  alldata <- cbind(allparkcoeff,bestPaEn,bestPaEf,fuzzycontr,fitnessValues,nindiv,
                   clouddata,selcross,beorwor,inputData,inputWind,mut_rate,allCoords)
  graphics::par(oldpar)
  return(alldata)
}


