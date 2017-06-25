#' @title Start The Genetic Algorithm for a wind Farm Layout
#' @name genAlgo
#' @description  This function coordinates all other elements of the
#' genetic algorithm. To initiate an optimization run, this method has to
#' be called with the desired inputs. To be able to include the terrain effect
#' model, the source of the Corine Land cover raster has to be given.
#' This function will not control user inputs before an optimization process.
#' It is therefore recommended to start an optimization run with
#' the \code{\link{windfarmGA}} function.
#'
#' @export
#'
#'
#' @importFrom raster crs getData crop mask projectRaster reclassify
#' @importFrom sp spTransform
#' @importFrom utils read.csv
#' @importFrom grDevices colorRampPalette
#' @importFrom dplyr select group_by summarise_each %>% funs
#' @importFrom graphics plot.new
#' @importFrom stats runif
#'
#' @param Polygon1 The considered area as shapefile (SpatialPolygons)
#' @param Rotor A numeric value that gives the rotor radius in meter
#' (numeric)
#' @param n A numeric value indicating the required amount of turbines
#' (numeric)
#' @param fcrR A numeric value, that is used for grid spacing (numeric)
#' @param referenceHeight The height at which the incoming
#' wind speeds were measured. (numeric)
#' @param RotorHeight The desired height of the turbine.
#' (numeric)
#' @param SurfaceRoughness A surface roughness length of the
#' considered area in m.  If the terrain effect model is activated, a
#' surface roughness will be calculated for every grid cell with the
#' elevation and land cover information. (numeric)
#' @param sourceCCL The source to the Corine Land Cover raster (.tif). Only
#' required, when the terrain effect model is activated. (character)
#' @param sourceCCLRoughness The source to the adapted
#' Corine Land Cover legend as .csv file. Only required, when terrain
#' effect model is activated. As default a .csv file within this
#' package (\file{~/extdata}) is taken that was already adapted
#' manually. To use your own .csv legend this variable has to be assigned.
#' See Details. (character)
#' @param Proportionality A numeric factor used for grid calculation.
#' Determines the percentage a grid has to overlay (numeric)
#' @param iteration A numeric value indicating the desired amount
#' of iterations of the algorithm (numeric)
#' @param mutr A numeric mutation rate, with low default value of 0.008
#' (numeric)
#' @param vdirspe A data.frame containing the incoming wind speeds,
#' wind directions and probabilities (data.frame)
#' @param topograp Logical Value, which indicates if the terrain effect model
#'  should be activated or not. (logical)
#' @param elitism Boolean Value, which indicates whether elitism should
#' be included or not. (logical)
#' @param nelit If \code{elitism} is TRUE, then this input variable
#' determines the amount of individuals in the elite group. (numeric)
#' @param selstate Determines which selection method is used,
#' "FIX" selects a constant percentage and "VAR" selects a variable percentage,
#' depending on the development of the fitness values. (character)
#' @param crossPart1 Determines which crossover method is used,
#' "EQU" divides the genetic code at equal intervals and
#' "RAN" divides the genetic code at random locations. (character)
#' @param trimForce If activated (\code{trimForce}==TRUE),
#' the algorithm will take a probabilistic approach to trim the windfarms
#' to the desired amount of turbines. If deactivated
#' (\code{trimForce}==FALSE) the adjustment will be random.
#' (logical)
#' @param Projection A desired Projection can be used instead
#' of the default Lambert Azimuthal Equal Area Projection. (character)
#'
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
#' plot(Polygon1,axes=TRUE)
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
#' ## factor of 3 (fcrR) and other required inputs.
#' result <- genAlgo(Polygon1 = Polygon1, n=12, Rotor=20,fcrR=3,iteration=10,
#'              vdirspe = data.in,crossPart1 = "EQU",selstate="FIX",mutr=0.8,
#'             Proportionality = 1, SurfaceRoughness = 0.3, topograp = FALSE,
#'             elitism=TRUE, nelit = 7, trimForce = TRUE,
#'             referenceHeight = 50,RotorHeight = 100)
#'}
#' @author Sebastian Gatscha
genAlgo           <- function(Polygon1, Rotor, n, fcrR, referenceHeight,
                              RotorHeight,SurfaceRoughness, Proportionality,
                              iteration, mutr,vdirspe, topograp, elitism, nelit,
                              selstate,crossPart1,trimForce, Projection,
                              sourceCCL,sourceCCLRoughness){
  oldpar <- graphics::par(no.readonly = T)
  plot.new();
  graphics::par(ask=F);

  resol2 <- fcrR*Rotor
  CrossUpLimit <- 300

  ## Check if Input Data is correct and prints it out.
  if  (crossPart1!= "EQU" & crossPart1 !="RAN") {
    crossPart1 <- readinteger()
  }
  if  (selstate!= "FIX" & selstate !="VAR") {
    selstate <- readintegerSel()
  }
  inputData <- list(Input_Data=rbind("Rotorradius"=Rotor,"Number of turbines"=n,"Grid Shape Factor"= fcrR,
                                     "Iterations"=iteration,"Mutation Rate"=mutr,
                                     "Percentage of Polygon"=Proportionality,"Topographie"=topograp,
                                     "Elitarism"=elitism, "Selection Method"=selstate,
                                     "Trim Force Method Used"=trimForce,"Crossover Method Used"=crossPart1,
                                     "Reference Height"= referenceHeight, "Rotor Height"=RotorHeight,
                                     "Resolution" = resol2));
  inputWind <- list(Windspeed_Data=vdirspe)
  print(inputData);print(inputWind)
  readline(prompt = "Check Inputs one last time. Press <ENTER> and lets go!")


  ##  Project the Polygon to LAEA if it is not already.
  if (missing(Projection)) {
    ProjLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
              +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  } else {
    ProjLAEA <- Projection;
  }

  if (as.character(raster::crs(Polygon1)) != ProjLAEA) {
    Polygon1 <- sp::spTransform(Polygon1, CRSobj = ProjLAEA)
  }

  ## Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
  Grid1 <- GridFilter(shape = Polygon1,resol = resol2,prop = Proportionality);
  Grid <- Grid1[[1]]
  dry.grid.filtered <- Grid1[[2]]
  AmountGrids <- nrow(Grid)

  ## Determine the amount of initial individuals and create initial population.
  nStart <- (AmountGrids*n)/iteration;   if (nStart < 100) {nStart <- 100};   if (nStart > 300) {nStart <- 300}
  nStart<- ceiling(nStart);
  startsel <- StartGA(Grid,n,nStart);

  ## Initialize all needed variables as list.
  maxParkwirkungsg <- 0; allparkcoeff <- vector("list",iteration);
  bestPaEn <- vector("list",iteration);
  bestPaEf <- vector("list",iteration); fuzzycontr <- vector("list",iteration);
  fitnessValues <- vector("list",iteration);
  nindiv <- vector("list",iteration); clouddata <- vector("list",iteration);
  selcross <- vector("list",iteration);
  beorwor <- vector("list",iteration); mut_rate <- vector("list",iteration);
  allCoords <- vector("list",iteration);

  ## Checks if terrain effect model is activated, and makes necessary caluclations.
  if (topograp == FALSE){
    cat("Topography and orography are not taken into account.")
  } else if (topograp == TRUE){
    cat("Topography and orography are taken into account.")

    par(mfrow=c(3,1))
    ## SRTM Daten
    Polygon1 <-  sp::spTransform(Polygon1, CRSobj =
                                   raster::crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"));
    extpol <- round(Polygon1@bbox,0)[,2]
    srtm <- raster::getData('SRTM', lon=extpol[1], lat=extpol[2]);
    srtm_crop <- raster::crop(srtm, Polygon1);
    srtm_crop <- raster::mask(srtm_crop, Polygon1)

    Polygon1 <-  sp::spTransform(Polygon1, CRSobj = raster::crs(ProjLAEA));
    srtm_crop <- raster::projectRaster(srtm_crop, crs = raster::crs(ProjLAEA));
    plot(srtm_crop, main="Elevation from SRTM");
    plot(Polygon1,add=T); plot(dry.grid.filtered,add=T)


    # INclude Corine Land Cover Raster to get an estimation of Surface Roughness
    if (missing(sourceCCLRoughness)) {
      path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
      sourceCCLRoughness <- paste0(path, "clc_legend.csv")
      # sourceCCL <- paste0(path,"g100_06.tif")
    } else {
      print("You are using your own Corine Land Cover legend.")
      readline(prompt = "\nPress <ENTER> if you want to continue")
      sourceCCLRoughness <- sourceCCLRoughness
    }

    ccl <- raster::raster(sourceCCL)
    cclPoly <- raster::crop(ccl,Polygon1)
    cclPoly1 <- raster::mask(cclPoly,Polygon1)
    rauhigkeitz <- utils::read.csv(sourceCCLRoughness,header = T,sep = ";");
    cclRaster <- raster::reclassify(cclPoly1, matrix(c(rauhigkeitz$GRID_CODE,rauhigkeitz$Rauhigkeit_z),ncol = 2))
    plot(cclRaster, main="Surface Roughness from Corine Land Cover")

  }

  ## Start the GA
  cat("\nStart Genetic Algorithm ...")
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

    x <- round(bestPaEn[[i]]$EnergyOverall[[1]],2);y <- round(bestPaEn[[i]]$EfficAllDir[[1]],2);
    e <- bestPaEn[[i]]$EfficAllDir;
    x1 <- round(bestPaEf[[i]]$EnergyOverall[[1]],2);y1 <- round(bestPaEf[[i]]$EfficAllDir[[1]],2);
    e1 <- bestPaEf[[i]]$EfficAllDir

    ## ALLPARKS RECT ID NOT CORRECT
    allparksNewplot <- dplyr::select(allparks,AbschGesamt,Rect_ID,Parkfitness);
    allparksNewplot <- allparksNewplot %>% dplyr::group_by(Rect_ID) %>%
                      dplyr::summarise_each(dplyr::funs(mean));
    if(any(allparksNewplot$Rect_ID %in% Grid$ID == F)){
      cat(paste("Index of Grid not correct. Bigger than maximum Grid? Fix BUG"))
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
      rangeFitnessVt0 <- range(t0);
      maxt0 <- max(t0);
      meant0 <- mean(t0);
      allcoef0 <- c(rangeFitnessVt0, meant0);
      fuzzycontr[[i]] <- rbind(allcoef0); colnames(fuzzycontr[[i]]) <- c("Min","Max","Mean")
      teil <- 2
      if (selstate=="VAR"){
        teil <- 1.35
      }
      u <- 1.1
      beorwor[[i]] <- cbind(0,0)
    }
    if (i>=2 && i <= iteration) {
      t0 <- split(allparks, duplicated(allparks$Run))$'FALSE';       t0 <- t0$Parkfitness;
      fitnessValues[[i]] <- t0;       rangeFitnessVt0 <- range(t0);
      maxt0 <- max(t0);      meant0 <- mean(t0); mint0 <- min(t0)
      t1 <- fitnessValues[[i-1]];t1;       rangeFitnessVt1 <- range(t1);
      maxt1 <- max(t1);     meant1 <- mean(t1); mint1 <- min(t1)
      maxDif <- maxt0 - maxt1; meanDif <- meant0 - meant1; minDif = mint0 - mint1
      WeightDif <- c(0.80,0.2,0.0)
      maxunt <- (maxDif*WeightDif[1])+(meanDif*WeightDif[2])+(minDif*WeightDif[3])
      allcoef1 <- c(rangeFitnessVt0, meant0); allcoef2 <- c(rangeFitnessVt1, meant1);
      fuzzycontr[[i]] <- rbind(allcoef1,allcoef2); colnames(fuzzycontr[[i]]) <- c("Min","Max","Mean")

      if(maxunt<0) {
        pri<-"deteriorated";teil<-teil-0.02; u<-u-0.06} else if (maxunt==0) {
          pri<-"not changed"; teil<-teil; u<-u} else {
            pri<-"improved"; teil<-teil+0.017; u<-u+0.03}

      if (teil > 5){teil<-5; u<-u+0.09; cat("Min 20% Selected");cat(paste("CPR is increased! CPR:",u,
                                                                            "SP: ",teil,"\n"))}
      if (trunc(u) < 0){u <- 0.5;teil<-teil-0.4;
      cat(paste("Min 1 CrossPoints. Selection decreased. CPR:",u,"SP: ",teil,"\n"))}
      if (u >= 4){u<-4;teil<-4; cat(paste("Max 5 CrossPoints. Select fittest 25%. SP: ",teil,"\n"))}
      if (teil <= 4/3){teil <- 4/3; cat(paste("Max 75% selected. SP: ", teil, "\n"))}
      if (length(fit) <= 20) {teil<-1;u<-u+0.07;
      cat(paste("Less than 20 individuals. Select all and increase Crossover-point rate. CPR: ",
                u,"SP: ", teil,"\n"))}
      if (teil > 5){teil<-5; cat(paste("Teil is bigger than 5. Set to max 5. SP:",teil,"\n"))}

      u <- round(u,2); teil<-round(teil,3);

      cat(paste("Fitness of this population (",i,"), compared to the prior,",pri,"by", round(maxunt,2),"\n"))
      meanunt <- meant0-meant1;
      beorwor[[i]] <- cbind(maxunt, meanunt)
    }

    if (selstate=="FIX"){
      if (teil==1){teil<-1} else {teil<-2}
    }
    if (crossPart1=="EQU"){
      u <- round(u,2)
    }

    ## How many are selected and how much crossover points are used?
    selcross[[i]] <- cbind(cross=trunc(u+1),teil)

    ## SELECTION
    ## print the amount of Individuals selected. Check if the amount of Turbines is as requested.
    selec6best <- selection1(fit, Grid,teil, elitism, nelit, selstate);
    selec6best_bin <- selec6best[[1]]
    cat(paste("Selection  -  Amount of Individuals: ",length(selec6best_bin[1,-1]),"\n"))
    Trus1 <- colSums(selec6best_bin)[-1] == n
    if (any(Trus1 == FALSE)){
      print("Number of turbines is not as required. Trus1. Fix BUG")
      break()
    }
    nindivsel <- length(selec6best_bin[1,-1]);

    ## CROSSOVER
    ## u determines the amount of crossover points, crossPart det. the method used (Equal/Random),
    ## uplimit is the maximum allowed permutations
    crossOut <- crossover1(selec6best, u, uplimit = CrossUpLimit, crossPart=crossPart1) ;
    cat(paste("Crossover  -  Amount of Individuals: ",length(crossOut[1,])));
    nindivcros <- length(crossOut[1,]);

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
      print(paste("1. Mutation Rate is", mutrn, "\n\n"))
    } else {
      mut <- mutation(a = crossOut, p = mutr);
      mut_rat <- mutr
    }
    mut_rate[[i]] <- mut_rat
    cat(paste("\nMutation   -  Amount of Individuals: ",length(mut[1,])));
    nindivmut <- length(mut[1,]);nindivmut

    ## TRIMTON
    ## After Crossover and Mutation, the amount of turbines in a windpark change and have to be
    ## corrected to the required amount of turbines.
    mut1 <- trimton(mut = mut, nturb = n, allparks = allparks, nGrids = AmountGrids,trimForce=trimForce)
    cat(paste("\nTrimToN    -  Amount of Individuals: ",length(mut1[1,])))
    Trus3 <- colSums(mut1) == n
    if (any(Trus3 == FALSE)){
      print(paste("Number of turbines is not as required. Trus3. Fix Bug. Amount:",
                  length(Trus3[Trus3==FALSE])))
      break()
    }

    nindiv[[i]] <- cbind(nindivfit,nindivsel,nindivcros,nindivmut)
    if (maxParkwirkungsg == 100) {
      i <- iteration + 1
    } else {
      i <- i+1
    }
  }

  ## Reduce the results, if a slution was found prior to the iend of the iterations
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

  ## Bind the results together and Output them.
  alldata <- cbind(allparkcoeff,bestPaEn,bestPaEf,fuzzycontr,fitnessValues,nindiv,
                   clouddata,selcross,beorwor,inputData,inputWind,mut_rate,allCoords)
  graphics::par(oldpar)
  return(alldata)
}


