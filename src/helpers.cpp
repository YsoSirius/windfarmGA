#include <Rcpp.h>
using namespace Rcpp;

// Calculate the overlapping area of wake and rotor area
// [[Rcpp::export]]
double wake_CPP(double Rotorf, double wakr, double leA) {
  double aov = pow(Rotorf,2) * acos((pow(Rotorf,2) - pow(wakr,2) + (pow(leA,2))) / (2 * leA * Rotorf)) + 
       (pow(wakr,2) * acos((pow(wakr,2) - pow(Rotorf,2) + pow(leA,2)) / (2 * leA * wakr))) -
       0.5 * sqrt((Rotorf + wakr + leA) * (-Rotorf + wakr + leA ) * 
       (Rotorf - wakr + leA) * (Rotorf + wakr - leA));
  return aov;
}

// Calculate the overlapping area of wake and rotor area (FULL) ////// Not used yet... testing
// [[Rcpp::export]]
NumericMatrix aov_CPP(double lenght_b, double wakr, double leA, double Rotorf, double rotor_rad) {  
  double aov = 0.1;
  double absch = 0.1;
  if (lenght_b == 0) {
    aov = 0;
    absch = 0;
  } else {

	Rcout << "wakr" << std::endl << wakr << std::endl;
	Rcout << "leA" << std::endl << leA << std::endl;
	Rcout << "Rotorf" << std::endl << Rotorf << std::endl;
	
	if ((wakr - Rotorf) >= leA && leA >= 0) {
	  aov = pow(rotor_rad, 2) * 3.14159265;
      absch = ((aov / Rotorf) * 100);
	}
	if ((wakr + Rotorf) <= leA) {
	  aov = 0;
      absch = 0;
	}
	if ((wakr - Rotorf) <= leA && leA <= (wakr + Rotorf))  {
	  //aov = wake_CPP(Rotorf, wakr, leA); // some problem still
	  aov = pow(Rotorf,2) * acos((pow(Rotorf,2) - pow(wakr,2) + (pow(leA,2))) / (2 * leA * Rotorf)) + 
            (pow(wakr,2) * acos((pow(wakr,2) - pow(Rotorf,2) + pow(leA,2)) / (2 * leA * wakr))) -
            0.5 * sqrt((Rotorf + wakr + leA) * (-Rotorf + wakr + leA ) * 
            (Rotorf - wakr + leA) * (Rotorf + wakr - leA));
      absch = ((aov / Rotorf) * 100);
	}
	Rcout << "wakr" << std::endl << wakr << std::endl;
	Rcout << "leA" << std::endl << leA << std::endl;
	Rcout << "Rotorf" << std::endl << Rotorf << std::endl;
	Rcout << "aov" << std::endl << aov << std::endl;
	Rcout << "absch" << std::endl << absch << std::endl;

  }
  return cbind(aov, absch);
}

// Rotate a set of coordinates around a given center point (P)
// [[Rcpp::export]]
NumericMatrix rotate_CPP(NumericVector X1, NumericVector Y1, double Px, double Py, float angle) {
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

// Calculates all 3 angles (instead of WinkelCalc.R
// [[Rcpp::export]]
NumericVector angles_CPP(NumericVector Aa, NumericVector Bb, NumericVector Cc) {
  NumericVector AB = Bb - Aa;
  NumericVector AC = Cc - Aa;  
  NumericVector BA = Aa - Bb;  
  NumericVector BC = Cc - Bb; 
  NumericVector CA = Aa - Cc; 
  NumericVector CB = Bb - Cc;
  double alpha = acos(sum(AB * AC) / (sqrt(sum(AB * AB)) * sqrt(sum(AC * AC)))) * 57.29578;
  double betha = acos(sum(BA * BC) / (sqrt(sum(BA * BA)) * sqrt(sum(BC * BC)))) * 57.29578;
  double gamma = acos(sum(CA * CB) / (sqrt(sum(CA * CA)) * sqrt(sum(CB * CB)))) * 57.29578;
  return NumericVector::create(alpha, betha, gamma);
}

// Calculates the energy output. (This is used for the reduced energy output with wake effects and for the full output)
// NOTE: 0.2965 = 0.593 * 0.5
// [[Rcpp::export]]
double energy_calc_CPP(NumericVector wind_speed, NumericVector rotor_radius, NumericVector air_rh) {
  return sum(0.2965 * air_rh * pow(wind_speed, 3) * (pow(rotor_radius, 2) * 3.141593)) / 1000;
}


// Calculate euclidean distance between two coordinates.
// [[Rcpp::export]]
float euc_CPP(float x1, float x2, float y1, float y2) {
  return sqrt(pow(x1 - y1, 2) + pow(x2 - y2, 2));
}

// Replacement of PointToLine.R
// [[Rcpp::export]]
NumericVector point_2_line_CPP(NumericVector x, NumericVector y) {
  double lc = euc_CPP(x[0], x[1], y[0], y[1]);
  double lb = euc_CPP(y[0], x[1], y[0], y[1]);
  double la = euc_CPP(x[0], x[1], y[0], x[1]);
  return NumericVector::create(y[0], y[1], x[0], x[1], y[0], x[1], lc, lb, la);
}


// Calculate the wake radius. /Not used, less performant --------------------------
// [[Rcpp::export]]
NumericVector wakeradius_CPP(double lenght_b, double RotD, double k) {
  double wakR; 
  if (lenght_b > 0) {
    wakR = (RotD * 2 + 2 * k * lenght_b) / 2;
  } else {
    wakR = 0.;
  }
  double rotar = pow(RotD, 2) * 3.141593;
  return NumericVector::create(wakR, rotar);
}

