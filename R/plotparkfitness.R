#' @title Plot the genetic algorithm results
#' @name plotparkfitness
#' @description Plot the evolution of fitness values with the influences of
#' selection, crossover and mutation.
#' @export
#'
#' @importFrom graphics par layout lines grid plot points axis legend abline
#' mtext title
#' @importFrom grDevices colorRampPalette
#' @importFrom stats smooth.spline median
#' @importFrom calibrate textxy
#' @importFrom dplyr select
#'
#'
#' @param result An output matrix of \code{\link{windfarmGA}} or
#' \code{\link{genAlgo}}, which has stored all relevant information. (matrix)
#' @param spar A numeric value determining how exact a spline should
#' be drawn. Default is 0.1 (numeric)
#'
#' @return NULL
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
#'             vdirspe = data.in,crossPart1 = "EQU",selstate="FIX",mutr=0.8,
#'             Proportionality = 1, SurfaceRoughness = 0.3, topograp = FALSE,
#'             elitism=TRUE, nelit = 7, trimForce = TRUE,
#'             referenceHeight = 50,RotorHeight = 100)
#' plotparkfitness(result, 0.1)
#'}
#' @author Sebastian Gatscha
plotparkfitness <- function(result,spar=0.1){
  rslt <- as.data.frame(do.call("rbind", result[,'allparkcoeff']))
  mutres <- as.data.frame(do.call("rbind", result[,'mut_rate']))
  nindiv1 <- as.data.frame(do.call("cbind", result[,'nindiv']))
  nindiv1 <- nindiv1[-seq(4,length(nindiv1),4)]

  opar <- graphics::par(no.readonly = T)
  selcross <- unlist(result[,'selcross'])
  selteil <- selcross[seq(2,length(selcross),2)]
  crossteil <- selcross[seq(1,length(selcross),2)]


  graphics::layout(matrix(c(1,1,1,1,2,3,4,5),2,4, byrow = TRUE));
  rbPal <- grDevices::colorRampPalette(c('red','green'));
  Col <- rbPal(4)[as.numeric(cut(as.numeric(rslt$maxparkfitness),breaks = 4))]

  plot(rslt$minparkfitness, xaxt='n', main="Parkfitness per Generation",
                 ylab="Parkfitness in %", cex=1.2,col="red", pch=20,
                 ylim= c(min(rslt$minparkfitness),max(rslt$maxparkfitness)));
  graphics::axis(1,at = 1:nrow(rslt),tick=T)
  graphics::points(rslt$meanparkfitness,ylab="MeanParkF", cex=1.2,col="blue", pch=20);
  graphics::points(rslt$maxparkfitness,ylab="maxParkF", cex=1.2,col="green", pch=20)
  x <- 1:length(rslt$maxparkfitness)

  if (nrow(result)>=4){
    lmin <- stats::smooth.spline(x,rslt$minparkfitness, spar=spar); graphics::lines(lmin, col='red', lwd=1.2);
    lmea <- stats::smooth.spline(x,rslt$meanparkfitness, spar=spar); graphics::lines(lmea, col='blue', lwd=1.2);
    lmax <- stats::smooth.spline(x,rslt$maxparkfitness, spar=spar); graphics::lines(lmax, col='green', lwd=1.2)
    graphics::grid(col = "gray")
  }


  graphics::par(mar=c(5,5,3,2))
  farbe <- rep(seq(1,3,1),length(nindiv1)/3);   ndindiplot <- as.integer(nindiv1)
  plot(ndindiplot,type="b",col=farbe,cex=2,pch=20, main="N-Individuen",axes = FALSE,
       ylab="N",ylim=c(0,max(ndindiplot)+100))
  graphics::axis(side = 2,tick = TRUE); axis(side = 1,tick = TRUE,at =seq(1,length(ndindiplot),3),
                                             labels =(1:(length(ndindiplot)/3)))
  graphics::legend("topleft",title="Amount of Individuals in: ",lty = c(1,1,1),cex=0.5,inset = c(0.01,0.01),
         box.lty=0,box.lwd=0,c("Fitness","Selection","Crossover"),col=farbe[1:3],xjust = 0)

  plot(1*100/selteil,ylim=c(20,110),type="b",cex=2,col="green",pch=20,main="Selection percentage",
                 ylab="Percentage",xlab="Generation")
  graphics::grid(col = "gray")
  selrpl <- 1*100/selteil;
  timeticksel <- which(selrpl>75);
  selrplval <- selrpl[selrpl>75]
  calibrate::textxy(timeticksel,selrplval,labs = timeticksel,cex = 0.7)


  plot(crossteil,col=crossteil,main="n Crossoverparts",xlab="Generation", ylab="Crossover Points",
                 ylim=c(1,8),cex=1,pch=15); graphics::grid(col = "gray")
  timetickcro <- which(crossteil>median(crossteil));
  crorplval <- crossteil[crossteil>median(crossteil)]
  calibrate::textxy(timetickcro,crorplval,labs = timetickcro,cex = 0.5)

  plot(as.numeric(t(mutres)),type="b",main="Mutation Rate",xlab="Generation",
                 ylab="Crossover Points",cex=1,pch=15)
  mutrpl <- as.numeric(t(mutres));
  timetick <- which(mutrpl>median(mutrpl));
  mutrplval <- mutrpl[mutrpl>median(mutrpl)]
  calibrate::textxy(timetick,mutrplval,labs = timetick,cex = 0.7)
  graphics::grid(col = "gray")


  op1 <- graphics::par(ask=T)
  on.exit(par(op1))
  graphics::par(mfrow=c(1,1))
  plot(ndindiplot,type="b",col=farbe,cex=2,pch=20, main="N-Individuen",axes = FALSE,
       ylab="N",ylim=c(0,max(ndindiplot)+100))
  graphics::axis(side = 2,tick = TRUE); axis(side = 1,tick = TRUE,at =seq(1,length(ndindiplot),3),
                                             labels =(1:(length(ndindiplot)/3)))
  graphics::legend("topleft",title="Amount of Individuals in: ",pch = c(20,20,20),cex=1,inset = c(0.01,0.01),
         box.lty=0,box.lwd=0,c("Fitness","Selection","Crossover"),text.col=farbe[1:3],col=farbe[1:3],xjust = 0)


  op2 <- graphics::par(ask=T)
  on.exit(par(op2))
  graphics::par(mfrow=c(2,1))
  plot(1*100/selteil,ylim=c(20,110),type="b",cex=2,col="green",pch=20,main="Selection percentage",
                 ylab="Percentage",xlab="Generation")
  graphics::grid(col = "gray")
  selrpl <- 1*100/selteil;timeticksel <- which(selrpl>75);   selrplval <- selrpl[selrpl>75]
  calibrate::textxy(timeticksel,selrplval,labs = timeticksel,cex = 0.5)
  plot(crossteil,col=crossteil,main="n Crossoverparts",xlab="Generation",
                 ylab="Crossover Points",ylim=c(1,8),cex=1,pch=15); grid(col = "gray")
  timetickcro <- which(crossteil>median(crossteil));   crorplval <- crossteil[crossteil>median(crossteil)]
  calibrate::textxy(timetickcro,crorplval,labs = timetickcro,cex = 0.5)


  if (length(timetick)!=0) {
    op3 <- graphics::par(ask=T)
    on.exit(par(op3))
    graphics::par(mfrow=c(1,1))
    rbPal <- grDevices::colorRampPalette(c('red','green'));
    Col <- rbPal(4)[as.numeric(cut(as.numeric(rslt$maxparkfitness),breaks = 4))]
    plot(rslt$minParkwirkungsg, xaxt='n', main="Mutation Influence", ylab=" in %", cex=1.2,col="red",
         pch=20, ylim= c(min(rslt$minParkwirkungsg),max(rslt$maxParkwirkungsg)));
    graphics::axis(1,at = 1:nrow(rslt),tick=T)
    graphics::points(rslt$meanParkwirkungsg,ylab="MeanParkEff", cex=1.2,col="blue", pch=20);
    graphics::points(rslt$maxParkwirkungsg,ylab="maxParkEff", cex=1.2,col="green", pch=20)
    x <- 1:length(rslt$maxparkfitness);
    if (nrow(result)>=4){
      lmin <- stats::smooth.spline(x,rslt$minParkwirkungsg, spar=spar); graphics::lines(lmin, col='red', lwd=1.2);
      lmea <- stats::smooth.spline(x,rslt$meanParkwirkungsg, spar=spar); graphics::lines(lmea, col='blue', lwd=1.2)
      lmax <- stats::smooth.spline(x,rslt$maxParkwirkungsg, spar=spar); graphics::lines(lmax, col='green', lwd=1.2);
      graphics::grid(col = "gray")
    }
      graphics::abline(v = timetick,col="black");
      graphics::mtext(mutrplval,side = 3,at = timetick,cex = 0.8)
  }
  if (length(timeticksel)!=0){
    op4 <- graphics::par(ask=T)
    on.exit(par(op4))
    graphics::par(mfrow=c(1,1))
    rbPal <- grDevices::colorRampPalette(c('red','green'));
    Col <- rbPal(4)[as.numeric(cut(as.numeric(rslt$maxparkfitness),breaks = 4))]
    graphics:: plot(rslt$minParkwirkungsg, xaxt='n', main="Selection Influence", ylab=" in %", cex=1.2,col="red",
         pch=20, ylim= c(min(rslt$minParkwirkungsg),max(rslt$maxParkwirkungsg)));
    graphics::axis(1,at = 1:nrow(rslt),tick=T)
    graphics::points(rslt$meanParkwirkungsg,ylab="MeanParkEff", cex=1.2,col="blue", pch=20);
    graphics::points(rslt$maxParkwirkungsg,ylab="maxParkEff", cex=1.2,col="green", pch=20)
    x <- 1:length(rslt$maxparkfitness);
    if (nrow(result)>=4){
      lmin <- stats::smooth.spline(x,rslt$minParkwirkungsg, spar=spar); graphics::lines(lmin, col='red', lwd=1.2);
      lmea <- stats::smooth.spline(x,rslt$meanParkwirkungsg, spar=spar); graphics::lines(lmea, col='blue', lwd=1.2)
      lmax <- stats::smooth.spline(x,rslt$maxParkwirkungsg, spar=spar); graphics::lines(lmax, col='green', lwd=1.2);
      graphics::grid(col = "gray")
  }
      graphics::abline(v = timeticksel,col="green");
      graphics::mtext(selrplval,side = 3,at = timeticksel,col="green",cex = 0.8)
  }
  if (length(timetickcro)!=0){
    op5 <- graphics::par(ask=T)
    on.exit(par(op5))
    graphics::par(mfrow=c(1,1))
    rbPal <- colorRampPalette(c('red','green'));
    Col <- rbPal(4)[as.numeric(cut(as.numeric(rslt$maxparkfitness),breaks = 4))]
    plot(rslt$minParkwirkungsg, xaxt='n', main="Crossover Influence", ylab=" in %", cex=1.2,col="red",
         pch=20, ylim= c(min(rslt$minParkwirkungsg),max(rslt$maxParkwirkungsg)));graphics::axis(1,at = 1:nrow(rslt),tick=T)
    graphics::points(rslt$meanParkwirkungsg,ylab="MeanParkEff", cex=1.2,col="blue", pch=20);
    graphics::points(rslt$maxParkwirkungsg,ylab="maxParkEff", cex=1.2,col="green", pch=20)
    x <- 1:length(rslt$maxparkfitness);
    if (nrow(result)>=4){
      lmin <- stats::smooth.spline(x,rslt$minParkwirkungsg, spar=spar); graphics::lines(lmin, col='red', lwd=1.2);
      lmea <- stats::smooth.spline(x,rslt$meanParkwirkungsg, spar=spar); graphics::lines(lmea, col='blue', lwd=1.2)
      lmax <- stats::smooth.spline(x,rslt$maxParkwirkungsg, spar=spar); graphics::lines(lmax, col='green', lwd=1.2);
      graphics::grid(col = "gray")
    }
      graphics::abline(v = timetickcro,col="red");
      graphics::mtext(crorplval,side = 3,at = timetickcro,col="red",cex = 0.8)
  }


  sddata <- plotCloud(result);
  fitsd <- dplyr::select(sddata,dplyr::contains("fit"));
  effsd <- dplyr::select(sddata,dplyr::contains("eff"));
  enesd <- dplyr::select(sddata,dplyr::contains("ene"));
  op6 <- graphics::par(ask=T)
  on.exit(par(op6))
  graphics::par(mfrow=c(4,1))
  plot(rslt$minparkfitness, xaxt='n', main="Parkfitness per Generation", ylab="Parkfitness in %",
       cex=1.2,col="red", pch=20, ylim= c(min(rslt$minparkfitness),max(rslt$maxparkfitness)))
  graphics::axis(1,at = 1:nrow(rslt),tick=T)
  graphics::grid(col = "black")
  graphics::points(rslt$meanparkfitness,ylab="MeanParkF", cex=1.2,col="blue", pch=20)
  graphics::points(rslt$maxparkfitness,ylab="maxParkF", cex=1.2,col="green", pch=20)
  x <- 1:length(rslt$maxparkfitness)
  if (nrow(result)>=4){
    lmin <- smooth.spline(x,rslt$minparkfitness, spar=spar); graphics::lines(lmin, col='red', lwd=1.2)
    lmea <- smooth.spline(x,rslt$meanparkfitness, spar=spar); graphics::lines(lmea, col='blue', lwd=1.2)
    lmax <- smooth.spline(x,rslt$maxparkfitness, spar=spar); graphics::lines(lmax, col='green', lwd=1.2)
    graphics::grid(col = "gray")
  }


  plot(1*100/selteil,ylim=c(20,110),type="b",lwd=2,col="green",pch=20,main="Selection percentage",
       ylab="Percentage",xlab="Generation");graphics::grid(lty = 2)
  plot(crossteil,col=crossteil,pch=20,cex=2,main="n Crossoverparts",xlab="Generation",ylab="Crossover Points",
       ylim=c(1,6));graphics::grid(lty = 2)
  plot(enesd$Ene.sd, type="b",col="blue",pch=20,lwd=2,main="Standard Deviation");graphics::grid(lty = 2)
  graphics::par(new = TRUE)
  plot(effsd$Eff.sd, type="b",col="orange",lwd=2,axes = FALSE, bty = "n", xlab = "", ylab = "",pch=20)
  graphics::par(new = TRUE)
  plot(fitsd$Fitn.sd, type="b",col="red",lwd=2,axes = FALSE, bty = "n", xlab = "", ylab = "",pch=20)

  timeticksd <- which(mutrpl>median(mutrpl));   sdrplval <- fitsd$Fitn.sd[timeticksd]
  if (length(timeticksd) != 0){
    calibrate::textxy(timeticksd,sdrplval,labs = timeticksd,cex = 0.5)
    graphics::abline(v = timeticksd)
    graphics::mtext(mutrplval,side = 3,at = timetick,cex = 0.8)
  }


  op7 <- graphics::par(ask=T)
  on.exit(par(op7))
  graphics::par(mfrow=c(1,1))
  plot(fitsd$Fitn.sd, type="b",col="red",lwd=2,axes = TRUE, bty = "n", xlab = "", ylab = "",
                 pch=20, main="Mutation Rate influence on Standard Deviation")
  if (length(timeticksd) != 0){
    calibrate::textxy(timeticksd,sdrplval,labs = timeticksd,cex = 0.7)
    graphics::abline(v = timeticksd)
    graphics::mtext(mutrplval,side = 3,at = timetick,cex = 0.8)
  }


  graphics::par(opar)
  return()
}

