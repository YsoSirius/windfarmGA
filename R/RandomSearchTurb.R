#' @title RandomSearchTurb Randomise the location of a single turbine.
#' @name RandomSearchTurb
#' @description Perform a random search for a single turbine, to
#' further optimize the output of the wind farm layout.
#' 
#' @export
#'
#' @importFrom sp SpatialPoints
#' @importFrom dplyr arrange select desc
#' @importFrom calibrate textxy
#' @importFrom raster rasterize raster extent
#' 
#'
#' @param result The resulting matrix of the function 'genAlgo' or
#' 'windfarmGA'. (matrix)
#' @param Polygon1 The Polygon for the wind farm area. (SpatialPolygons)
#' @param n The number of random searches to be perfomed. Default is 20.
#' (numeric)
#' @param Plot Should the random serach be plotted? Default is TRUE
#' (boolean)
#' @param GridMethod Should the polygon be divided into rectangular or
#' hexagonal grid cells? The default is rectangular grid cells and hexagonal
#' grid cells are computed when assigning "h" or "hexagon" to this input
#' variable. The randomly generated points may also be paced outside of 
#' their hexagon. (character)
#'
#' @return Returns a list.
#'
#' @examples \donttest{
#' load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
#' 
#' Res = RandomSearchTurb(result = resultrect, Polygon1 = polygon, n=10)
#' RandomSearchPlot(resultRS = Res, resultGA = resultrect, Polygon1 = polygon, best =2)
#'
#' }
#' @author Sebastian Gatscha
RandomSearchTurb <- function(result, Polygon1, n, Plot, GridMethod){
  ## Missing attrs, Data Config ############################
  # Order the resulting layouts with highest Energy output
  resldat <- do.call("rbind", result[,"bestPaEn"])
  maxDist <- as.numeric(result[,"inputData"][[1]]['Rotorradius',])*2
  if (missing(Plot)) {
    Plot = T
  }
  if (missing(n)) {
    n <- 20
  }
  if (missing(GridMethod)){
    GridMethod <- "Rectangular"
  }
  GridMethod <- toupper(GridMethod)
  if (Plot == T) {
    opar <- par(no.readonly = T)
    par(mfrow=c(1,1))
  }

  ## Remove duplicated "Runs", assign do resldat and sort by Energy
  resldat <- resldat[!duplicated(resldat$Run),]
  resldat$GARun <- 1:nrow(resldat)
  resldat <- dplyr::arrange(resldat, desc(EnergyOverall))
  
  # if (best > nrow(resldat)) { best = nrow(resldat)}
  bestGARunIn <- resldat$GARun[1:1]
  
  bestGARun <- bestGARunIn[1]
  resolu <- as.numeric(result[bestGARun,]$inputData["Resolution",][1])
  rotRad <- as.numeric(result[bestGARun,]$inputData["Rotorradius",][1])
  propu  <- as.numeric(result[bestGARun,]$inputData["Percentage of Polygon",][1])
  winddata <- result[bestGARun,]$inputWind
  
  ## Decide if the space division should be rectangular or in hexagons.
  if (GridMethod != "HEXAGON" & GridMethod != "H") {
    # Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
    Grid <- GridFilter(shape = Polygon1,resol = resolu, prop = propu, plotGrid = F);
  } else {
    # Calculate a Grid with hexagonal grid cells
    Grid <- HexaTex(Polygon1, resolu/2)
  }
  
  ## Get max factor for alteration of coordination
  maxFac <- rotRad * (resolu/(rotRad*2))
  ##############################
  
  
  ## Windata Formatting / Raster ###################
  winddata$wd <- round(winddata$wd,0)
  winddata$wd <-  round(winddata$wd/100,1)*100;
  ## If no probabilites are given, assign uniform distributed ones.
  if (any(names(winddata) == "probab") == FALSE) {
    winddata$probab <- 100/nrow(winddata)
  }
  ## Checks if all wind directions/speeds have a possibility greater than 0.
  winddata$probab <- round(winddata$probab,0)
  if (sum(winddata$probab) != 100) {
    winddata$probab <- winddata$probab*(100/sum(winddata$probab))
  }
  ## Checks if duplicated wind directions are at hand
  if   (any(duplicated(winddata$wd)==TRUE)) {
    for (i in 1:nrow(winddata[duplicated(winddata$wd)==F,])){
      temp <- winddata[winddata$wd ==  winddata[duplicated(
        winddata$wd)==F,][i,'wd'],];
      temp$ws < -with(temp, sum(ws * (probab/sum(probab))));
      temp$probab <- with(temp, sum(probab * (probab/sum(probab))));
      
      winddata[winddata$wd ==  winddata[duplicated(
        winddata$wd)==F,][i,'wd'],]$ws <- round(temp$ws,2)[1]
      winddata[winddata$wd ==  winddata[duplicated(
        winddata$wd)==F,][i,'wd'],]$probab <- round(temp$probab,2)[1]
    }
  }
  winddata <- winddata[!duplicated(winddata$wd)==TRUE,];
  winddata <- winddata[with(winddata, order(wd)), ]
  if (sum(winddata$probab) != 100) {
    winddata$probab <- winddata$probab*(100/sum(winddata$probab))
  }
  probabDir <- winddata$probab;
  pp <- sum(probabDir)/100;   probabDir <- probabDir/pp;
  windraster <-raster::rasterize(Polygon1, raster::raster(raster::extent(Polygon1),
                                                          ncol=180, nrow=180),field=1)
  ###################
  
  ## Get the starting layout of windfarm[o]
  layout_start <- result[bestGARun,]$bestPaEn    
  
  ## todo #################
  # exit = 0
  # while (toupper(exit) != toupper("x")) {
  #   print("blabla")
  #   exit <- readline(prompt = "Enter an 'x' if you want to stop:")
  # }
  #################
  
  ### Turbine Indexing ################
  raster::plot(Grid[[2]])
  points(x = layout_start$X, y = layout_start$Y, pch=15)
  calibrate::textxy(X = layout_start$X, Y = layout_start$Y, 
                    labs = layout_start$Rect_ID, cex = 1.5, offset = 0.75)
  turbInx <- ""
  
  while (!turbInx %in% layout_start$Rect_ID) {
    cat("Enter the number of the turbine you want to optimize.")
    turbInx <- readline(prompt = "Please enter the corresponding number: ")
  }
  turbInx <- which(layout_start$Rect_ID == as.numeric(turbInx))
  ################
  
  ## Run Random Search  ################
  coordLay <- layout_start[,c(1:2)]
  if (Plot == T) {
    raster::plot(Grid[[2]])
    points(coordLay, pch=15, col="black")
    points(coordLay[as.numeric(turbInx),], pch=15, col="red")
    
    legend( x="bottom", 
            legend=c("Starting Location", "Selected Turbine", "Randomly generated Location",  
                     "Suitable Location", "Relocated due to Turbine Collision"),
            col=c("black","red" ,"blue","green", "purple"), lwd=1, lty=c(0,0), 
            pch=c(15,15,3,1,20))
  }

  ## Run n random searches on windfarm[o]
  RandResult <- vector(mode = "list", length = n)
  for (i in 1:n){
    coordLayRnd <- coordLay
    maxAlterX <- runif(1, min = -maxFac, max = maxFac)
    maxAlterY <- runif(1, min = -maxFac, max = maxFac)
    cordNew <- coordLayRnd[as.numeric(turbInx),]
    cordNew$X <- cordNew$X+maxAlterX
    cordNew$Y <- cordNew$Y+maxAlterY
    if (Plot == T) {
      points(cordNew, col="blue", pch=3)
    }
    
    coordLayRnd[as.numeric(turbInx),] <- cordNew
    
    ## Check if turbines are not colliding #####################
    pointsDistBl <- sp::SpatialPoints(coordLayRnd)
    pointsDist <- sp::spDists(sp::SpatialPoints(coordLayRnd))
    distMin <- pointsDist[which(pointsDist<maxDist & pointsDist!=0)]
    
    while (length(distMin)>0) {
      # length(distMin)>0
      pointsDistBl <- sp::SpatialPoints(coordLay)
      pointsDist <- sp::spDists(sp::SpatialPoints(coordLay))
      distMin <- pointsDist[which(pointsDist<maxDist & pointsDist!=0)]
      distMin
      if (length(distMin)==0) {
        # print("Relocation of turbines due to fasablity checks done.")
        # break()
      } else {
        pointsDist <- data.frame(pointsDist)
        colnames(pointsDist) <- 1:length(pointsDist)
        
        pointsDist <- round(pointsDist,2); distMin <- round(distMin,2)
        distMin <- distMin[duplicated(distMin)]
        
        ColRowMin <- which(pointsDist == distMin, useNames=T, arr.ind = T)
        if (length(ColRowMin)==0) {
          break("Something is wrong")
        }
        CoordsWrongOrigin <- coordLay[ColRowMin[1,2],]
        
        maxAlterX <- runif(1, min = -maxFac, max = maxFac)
        maxAlterY <- runif(1, min = -maxFac, max = maxFac)
        cordNew <- CoordsWrongOrigin
        cordNew$X <- cordNew$X+maxAlterX; cordNew$Y <- cordNew$Y+maxAlterY
        
        points(coordsj[ColRowMin[1,2],], col="purple", pch=20)
        points(coordsj[ColRowMin[1,2],], col="purple", pch=20)
        
        coordsj[ColRowMin[1,2],] <- cordNew
      }
      
      
    }
    if (Plot == T) {
      points(coordLay, col="green")
    }
    #####################
    
    ## Arrange random points to input for calculateEn
    coordLayRnd$ID <- 1; coordLayRnd$bin <- 1; coordLayRnd <- dplyr::select(coordLayRnd, ID,X,Y,bin)
    
    # Calculate energy and save in list with length n ################
    resCalcen <- calculateEn(sel=coordLayRnd,referenceHeight= 50,
                             RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
                             distanz = 100000, resol = 200,dirSpeed = winddata,
                             RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
                             windraster = windraster, 
                             srtm_crop, cclRaster, weibull=F, weibullsrc)
    ee  <- lapply(resCalcen, function(x){split(x, duplicated(x$Punkt_id))$'FALSE'})
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
    xundyOrig <- coordLayRnd[,2:3];
    # Add the Efficieny and the Energy Output of all wind directions and add the total Wake Effect of every Point Location
    xundyOrig$EfficAllDir <- enOut$Efficalldire[1];xundyOrig$EnergyOverall <- enOut$EnergyOverall[1];
    xundyOrig$AbschGesamt <- AbschGesamt
    # Include the Run of the genertion to the data frame
    xundyOrig$Run <- i
    
    # Get the Rotor Radius and the Rect_IDs of the park configuration
    dt <-  ee[[1]]; dt <- dplyr::select(dt,RotorR, Rect_ID);
    # Bind the Efficiency,Energy,WakeEffect,Run to the Radius and Rect_IDs
    dt <- cbind(xundyOrig,dt)
    dt$bestGARun <- bestGARun
    ################
    
    RandResult[[i]] <- dt
  }
  if (Plot == T) {
    par(opar)
  }
  ################
  
  
  
  
  
  return(RandResult)
}


