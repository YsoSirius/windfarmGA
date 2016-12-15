#' @title Calculates Angles between 3 Points
#' @name WinkelCalc
#' @description  Calculates all three angles for an imaginary right triangle
#' between the actual turbine, the possible influencing turbine and a
#' right angle. The function works as well with non-right triangles, although
#' it is not needed for the genetic algorithm, as only the distance
#' perpendicular to the wind direction and the sidewise distance to the
#' potentially influencing turbine is required for further calculations.
#' Point C, where the right angle is located will therefore be calculated
#' by the algorithm itself. See also \code{\link{PointToLine2}}.
#' @export
#' @param Aa A numeric value with the x and y coordinates of a
#' potentially influencing turbine (numeric)
#' @param Bb A numeric value with the x and y coordinates of the
#' current turbine (numeric)
#' @param Cc A numeric value with the x and y coordinates of the
#' imaginary right angle (numeric)
#'
#' @return Returns a matrix with the alpha, betha and gamma angles of the
#' imaginary right triangle (matrix)
#' @examples \donttest{
#'   Aa= as.numeric(cbind(1,1))
#'   Bb= as.numeric(cbind(10,3))
#'   Cc= as.numeric(cbind(10,1))
#'   plot(rbind(Aa,Bb,Cc,Aa), type="b", xlab="x",
#'        ylab="y", ylim=c(0,4), xlim=c(0,11));
#'   points(x=Aa[1],y=Aa[2],col="green",pch=20);
#'   points(x=Bb[1],y=Bb[2],col="red",pch=20);
#'   points(x=Cc[1],y=Cc[2],col="blue",pch=20)
#'   Angles <- WinkelCalc(Aa,Bb,Cc); Angles
#'   text(rbind(Aa,Bb,Cc),labels=round(Angles,2),pos=1)
#' }
#' @author Sebastian Gatscha
WinkelCalc        <- function(Aa,Bb,Cc) {
  # Aa=xynew1[i,]; Bb=WKA_akt; Cc= P2LFu[5:6]
  AB <- Bb-Aa; AC <- Cc-Aa;  BA <- Aa-Bb;  BC <- Cc-Bb; CA <- Aa-Cc; CB <- Bb-Cc;
  # Calculate all 3 angles
  alpha <- acos(sum(AB*AC) / (sqrt(sum(AB * AB)) * sqrt(sum(AC * AC))))*(180/pi);
  betha <- acos(sum(BA*BC)/(sqrt(sum(BA*BA))*sqrt(sum(BC*BC))))*(180/pi);
  gamma <- acos(sum(CA*CB) / (sqrt(sum(CA * CA)) * sqrt(sum(CB * CB))))*(180/pi)

  if (any(is.na(alpha),is.na(betha),is.na(gamma))) {
    alpha=0;betha=0;gamma=0;
  }

  if (trunc(alpha+betha+gamma) >= 179 | ceiling(alpha+betha+gamma) <= 180) {
    winkel <- rbind(alpha,betha,gamma)
    invisible(winkel)
  }
}

