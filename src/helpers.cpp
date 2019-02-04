#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double wake(double Rotorf, double wakr, double leA) {
  double aov = pow(Rotorf,2) * acos((pow(Rotorf,2) - pow(wakr,2) + (pow(leA,2))) / (2 * leA * Rotorf)) + 
       (pow(wakr,2) * acos((pow(wakr,2) - pow(Rotorf,2) + pow(leA,2)) / (2 * leA * wakr))) -
       0.5 * sqrt((Rotorf + wakr + leA) * (-Rotorf + wakr + leA ) * 
       (Rotorf - wakr + leA) * (Rotorf + wakr - leA));
  return aov;
}

// [[Rcpp::export]]
double wakeRcpp(double lenght_b, bool topograp, double RotD, double k) {
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

// Calculate euclidian distance between two coordinates. /Not faster
// 
//double eucdist(double x1, double x2, double y1, double y2) {
//	double dist = sqrt(pow(x1 - y1, 2) + pow(x2 - y2, 2));
//	return dist;
// }
