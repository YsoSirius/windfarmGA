#' @title Plot all outputs of all generations with coresponding
#' standard deviation
#' @name plotCloud
#' @description  Plot all the fitness, efficiency and energy outputs of
#' all generations. Plot the coresponding standard deviation below.
#'
#' @export
#'

#' @importFrom dplyr group_by summarise %>%
#' @importFrom stats smooth.spline sd
#' @importFrom graphics lines par plot points
#'
#' @param result The output matrix of \code{\link{genAlgo}}, which has
#' stored all relevant information. (matrix)
#' @param pl Should the results be plotted? Default is "FALSE" (character)
#'
#' @return Returns a data.frame with the values for fitness, efficiency
#' and energy for all evaluated individuals. (data.frame)
#'
#' @author Sebastian Gatscha
plotCloud <- function(result,pl="FALSE"){
  #result = result2
  # library(dplyr)
  opar <- graphics::par(no.readonly = T)
  clouddata <- result[,7]
  EffCloud <- lapply(clouddata, function(x) x = x[[1]]);
  EneCloud <- lapply(clouddata, function(x) x = x[[2]]);
  FitCloud <- lapply(clouddata, function(x) x = x[[3]]);

  EffCldInd <- list();EneCldInd <- list();FitCldInd <- list();
  for (i in 1:length(clouddata)){
    l <- length(clouddata[[i]]$EfficAllDir); l
    EffCldInd[[i]] <- t(as.matrix(rbind(rep(i,l),EffCloud[[i]])));
    EneCldInd[[i]] <- t(as.matrix(rbind(rep(i,l),EneCloud[[i]])));
    FitCldInd[[i]] <- t(as.matrix(rbind(rep(i,l),FitCloud[[i]])));
  }
  EffCldInd <- do.call("rbind",EffCldInd)
  EffCldIndmax <- data.frame(EffCldInd)
  EffCldIndmax <- dplyr::group_by(EffCldIndmax,X1) %>%
    dplyr::summarise(max=max(X2),mean=mean(X2),min=min(X2),sd=sd(X2))

  EneCldInd <- do.call("rbind",EneCldInd)
  EneCldIndmax <- data.frame(EneCldInd)
  EneCldIndmax <- dplyr::group_by(EneCldIndmax,X1) %>%
    dplyr::summarise(max=max(X2),mean=mean(X2),min=min(X2),sd=sd(X2))

  FitCldInd <- do.call("rbind",FitCldInd)
  FitCldIndmax <- data.frame(FitCldInd)
  FitCldIndmax <- dplyr::group_by(FitCldIndmax,X1) %>%
    dplyr::summarise(max=max(X2),mean=mean(X2),min=min(X2),sd=sd(X2))

  if (pl=="TRUE"){
    par(mfrow=c(2,3))
    graphics::plot(FitCldInd, main="Fitness",xlab="Generation",ylab="Fitnessvalue",pch=20,col="red",cex=1.3);
    lf <- stats::smooth.spline(x=FitCldInd[,1],y=FitCldInd[,2], spar=0.1);
    graphics::lines(lf, col='red', lwd=1.2)
    graphics::points(x=FitCldIndmax$X1,y=FitCldIndmax$max,type="l",col="red")
    graphics::points(x=FitCldIndmax$X1,y=FitCldIndmax$min,type="l",col="red")

    graphics::plot(EffCldInd, main="Efficiency",xlab="Generation",ylab="Efficiency in %",pch=20,col="orange",cex=1.3);
    le <- stats::smooth.spline(x=EffCldInd[,1],y=EffCldInd[,2], spar=0.1); graphics::lines(le, col='orange', lwd=1.2)
    graphics::points(x=EffCldIndmax$X1,y=EffCldIndmax$max,type="l",col="orange")
    graphics:: points(x=EffCldIndmax$X1,y=EffCldIndmax$min,type="l",col="orange")

    graphics::plot(EneCldInd, main="Energy",xlab="Generation",ylab="Energy in kW",pch=20,col="blue",cex=1.3);
    len <- stats::smooth.spline(x=EneCldInd[,1],y=EneCldInd[,2], spar=0.1); graphics::lines(len, col='blue', lwd=1.2)
    graphics::points(x=EneCldIndmax$X1,y=EneCldIndmax$max,type="l",col="blue")
    graphics::points(x=EneCldIndmax$X1,y=EneCldIndmax$min,type="l",col="blue")

    graphics::plot(x=FitCldIndmax$X1,y=FitCldIndmax$sd, main="Standard Deviation Fitness",xlab="Generation",ylab="Standard Deviation of Population", col="red",type="b")
    graphics::plot(x=EffCldIndmax$X1,y=EffCldIndmax$sd, main="Standard Deviation Efficiency",xlab="Generation",ylab="Standard Deviation of Population", col="orange",type="b")
    graphics::plot(x=EneCldIndmax$X1,y=EneCldIndmax$sd, main="Standard Deviation Energy",xlab="Generation",ylab="Standard Deviation of Population", col="blue",type="b")

  }

  clouddatafull <- cbind(Fitn=FitCldIndmax,Eff=EffCldIndmax,Ene=EneCldIndmax)
  par(opar)
  invisible(clouddatafull)

}


