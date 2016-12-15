#' @title Distances between right triangle points
#' @name PointToLine2
#' @description  Takes two input coordinates, from the current turbine and
#' from the potentially influencing turbine, as it stands in front of the
#' current turbine. The algorithm draws an imaginary right triangle between
#' the two input points and a point C, which is calculated by the algorithm
#' itself. Wind will always seem to come from north, as the input area will
#' be rotated accordingly if the incoming wind direction does not come from
#' north. For further calculations only the distance perpendicular to the
#' wind direction and the sidewise distance to the potentially influencing
#' turbine is required.
#'
#' @export
#'
#' @importFrom sp Line Lines SpatialLines
#' @importFrom raster plot
#' @importFrom graphics text points
#'
#' @param x Coordinates of the current turbine. (numeric)
#' @param y Coordinates of the turbine, that could potentially influence
#' the current turbine. (numeric)
#' @param plotAngles A logical variable, which is used to plot the distances
#' and angles. Default is FALSE. (logical)
#' @return Returns a matrix with the resulting distances and the coordinates
#' of 3 points. (matrix)
#'
#' @details Assume two points located at P1=c(x=1,y=1) and P2=c(x=10,y=15)
#' and wind coming from north. If P1 is the current turbine and P2 the
#' potentially influencing turbine, then C will be located at C=c(x=10,y=1)
#' where it will build a right triangle. plot(rbind(P1,P2,C,P1),type="l")
#' See Examples.
#'
#' @examples \donttest{
#' ## For further calculations only the distances between B-C and A-C
#' ## and the angle at A will be needed. B represents the current turbine
#' ## and A represents a turbine, that could potentially influence turbine B.
#' x = c(100,100); y=c(500,500);
#' plot(rbind(x,y),col=c("red","blue"),cex=2,pch=20);
#' PointToLine2(x,y,TRUE)
#' }
#' @author Sebastian Gatscha
PointToLine2      <- function(x,y,plotAngles) {
  # x=WKA_akt;y= xynew1[i,]
  # x=as.matrix(cbind(000,000));y=as.matrix(cbind(100,000)); c=as.matrix(cbind(y[1,1],x[1,2])); plot(rbind(x,y,c,x),type="l")
#   if (x[1]-y[1] == 0 | x[2]-y[2]==0) {
#     distc <- euc.dist(x,y)
#     distIndiv <- rbind(c(y,x,x,distc,distc,0));colnames(distIndiv) <- c("Ax","Ay","Bx","By","Cx","Cy","Laenge_C","Laenge_B", "Laenge_A");
#     return(distIndiv)
#   }
  if (is.matrix(y) == FALSE) {y <- as.matrix(rbind(y))};  if (is.matrix(x) == FALSE) {x<- as.matrix(rbind(x))};
  C1 <- as.matrix(cbind(y[1,1],x[1,2]));
  c <- euc.dist(x,y);  a<- euc.dist(x,C1); b<- euc.dist(C1,y);  dist1 <- c(c,b,a)

  if (plotAngles==TRUE){
    # require(sp)
    # ABC <- rbind(as.numeric(x),as.numeric(y),as.numeric(C1))
    # plot(ABC, xlim=c(min(ABC[,1])-10,max(ABC[,1])+10), ylim=c(min(ABC[,2])-10,max(ABC[,2])+10),main= "Right Triangle", xlab="X",ylab="Y")
    ## Plot the Points of the Triangle
    graphics::points(x, cex=2, col="red", pch=20);
    graphics::points(y, cex=2, col="green", pch=20);
    graphics::points(C1, col="blue", cex=2, pch=20)
    ## Create and Plot the Lines between Points
    sidec <-sp::Line(rbind(x, y));   Cl<-sp::Lines(list(sidec), ID="a");
    Csl<-sp::SpatialLines(list(Cl));  raster::plot(Csl,add=TRUE,col="green");
    sidea <-sp::Line(rbind(x, C1));  Al<-sp::Lines(list(sidea), ID="b");
    Asl<-sp::SpatialLines(list(Al));  raster::plot(Asl,add=TRUE,col="red");
    sideb <-sp::Line(rbind(y, C1));  Bl<-sp::Lines(list(sideb), ID="c");
    Bsl<-sp::SpatialLines(list(Bl));  raster::plot(Bsl,add=TRUE,col="blue")
    ## Calculate Distance between the points and save Numerical;
    c <- euc.dist(x,y);  a<- euc.dist(x,C1); b<- euc.dist(C1,y);  dist1 <- c(c,b,a)
    ## Add Text: The Labels of the Points A,B,C, and the distances at the half of the
    ## size of the vector length
    graphics::text(x = cbind(c(x[1],y[1],C1[1]),c(x[2],y[2],C1[2])),
                   labels = c("B","A","C"), pos=4, col=c("red","green","blue"),cex=1);
  }

  distIndiv <- rbind(c(y,x,C1,dist1));     colnames(distIndiv) <- c("Ax","Ay","Bx","By","Cx","Cy","Laenge_C","Laenge_B", "Laenge_A");
  return(distIndiv)
}

##importFrom GenAlgo euc.dist

