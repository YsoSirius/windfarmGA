#include <Rcpp.h>
using namespace Rcpp;

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

// Calculates all 3 angles of 3 pair of coordinates
// [[Rcpp::export]]
NumericVector angles_CPP(NumericVector Aa, NumericVector Bb, NumericVector Cc) {
  // Calculate the side lengths of the triangle
  double a = sqrt(pow(Bb[0] - Cc[0], 2) + pow(Bb[1] - Cc[1], 2));
  double b = sqrt(pow(Aa[0] - Cc[0], 2) + pow(Aa[1] - Cc[1], 2));
  double c = sqrt(pow(Aa[0] - Bb[0], 2) + pow(Aa[1] - Bb[1], 2));
  
  // Calculate the three angles using the law of cosines and Convert the angles from radians to degrees
  double alpha = acos((b*b + c*c - a*a) / (2*b*c)) * 57.29578;
  double betha = acos((a*a + c*c - b*b) / (2*a*c)) * 57.29578;
  double gamma = acos((a*a + b*b - c*c) / (2*a*b)) * 57.29578;
  
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

// Create Triangle between influencing turbine, actual turbine and imaginary point with 
// right angle. Return all coordinates and their distances to each other.
// [[Rcpp::export]]
NumericVector point_2_line_CPP(NumericVector x, NumericVector y) {
  double lc = euc_CPP(x[0], x[1], y[0], y[1]);
  double lb = euc_CPP(y[0], x[1], y[0], y[1]);
  double la = euc_CPP(x[0], x[1], y[0], x[1]);
  return NumericVector::create(y[0], y[1], x[0], x[1], y[0], x[1], lc, lb, la);
}
