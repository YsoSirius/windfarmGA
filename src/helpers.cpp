#include <Rcpp.h>
using namespace Rcpp;

// Calculate the overlapping area of wake and rotor area
// [[Rcpp::export]]
double wake(double Rotorf, double wakr, double leA) {
  double aov = pow(Rotorf,2) * acos((pow(Rotorf,2) - pow(wakr,2) + (pow(leA,2))) / (2 * leA * Rotorf)) + 
       (pow(wakr,2) * acos((pow(wakr,2) - pow(Rotorf,2) + pow(leA,2)) / (2 * leA * wakr))) -
       0.5 * sqrt((Rotorf + wakr + leA) * (-Rotorf + wakr + leA ) * 
       (Rotorf - wakr + leA) * (Rotorf + wakr - leA));
  return aov;
}

// Calculate the overlapping area of wake and rotor area (FULL)
// [[Rcpp::export]]
NumericVector aovCPP(double lenght_b, double wakr, double leA, double Rotorf, double rotor_rad) {
  NumericVector aov; 
  if (lenght_b == 0) {
	aov = 0;
  } else {
	if ((wakr - Rotorf) >= leA && leA >= 0) {
	  aov = pow(rotor_rad, 2) * 3.14159265;
	}
	if ((wakr + Rotorf) <= leA) {
	  aov = 0;
	}
	if ((wakr - Rotorf) <= leA && leA <= (wakr + Rotorf))  {
	  aov = wake(Rotorf = 50,  wakr = 106.25, leA = 150);
	}
  }
  return aov;
}

// Rotate a set of coordinates around a given center point (P)
// [[Rcpp::export]]
NumericMatrix rotatePP(NumericVector X1, NumericVector Y1, double Px, double Py, float angle) {
  double d1 = -angle * 0.01745329;  // PI / 180
  int n = X1.size();
  NumericVector x(n);
  NumericVector y(n);
  for (int i = 0; i < n; i++) {
    x[i] = Px + cos(d1) * (X1[i] - Px) - sin(d1) * (Y1[i] - Py);
    y[i] = Py + sin(d1) * (X1[i] - Px) + cos(d1) * (Y1[i] - Py);
  }
  return cbind(x, y);
}

// Calculate the wake radius. /Not used, less performant
// [[Rcpp::export]]
double wakeCPP(double lenght_b, bool topograp, double RotD, double k) {
  double wakR; 
  if (lenght_b) {
    if (topograp){
      wakR = (RotD * 2 + 2 * k * lenght_b) / 2;
    } else {
      wakR = (RotD * 2 + 2 * k * lenght_b) / 2;
    }
  } else {
    wakR = 0;
  }
  return wakR;
}
	
// Calculate euclidian distance between two coordinates. /Not used, less performant
// 
//double eucdist(double x1, double x2, double y1, double y2) {
//	double dist = sqrt(pow(x1 - y1, 2) + pow(x2 - y2, 2));
//	return dist;
// }
