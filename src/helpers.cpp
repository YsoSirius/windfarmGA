#include <Rcpp.h>
#include <cmath>
#include <vector>
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

// Calculates the energy output. (This is used for the reduced energy output with wake effects and for the full output)
// NOTE: 0.2965 = 0.593 * 0.5  (Betz Cp). Callers scale by Cp / 0.593.
// [[Rcpp::export]]
double energy_calc_CPP(NumericVector wind_speed, NumericVector rotor_radius, NumericVector air_rh) {
  return sum(0.2965 * air_rh * pow(wind_speed, 3) * (pow(rotor_radius, 2) * 3.141593)) / 1000;
}


static double circle_intersection_one(
    double r1, double r2, double h1, double h2, double dx
) {
  const double rr1 = r1 * r1;
  const double rr2 = r2 * r2;
  const double d = std::sqrt(dx * dx + (h1 - h2) * (h1 - h2));
  if (d >= r2 + r1) {
    return 0;
  }
  if (d <= std::fabs(r1 - r2) && r1 >= r2) {
    return M_PI * rr2;
  }
  if (d <= std::fabs(r1 - r2) && r1 < r2) {
    return M_PI * rr1;
  }
  double cphi = (rr1 + d * d - rr2) / (2 * r1 * d);
  double cth = (rr2 + d * d - rr1) / (2 * r2 * d);
  if (cphi > 1) cphi = 1;
  if (cphi < -1) cphi = -1;
  if (cth > 1) cth = 1;
  if (cth < -1) cth = -1;
  const double phi = std::acos(cphi) * 2;
  const double theta = std::acos(cth) * 2;
  return 0.5 * phi * rr1 - 0.5 * rr1 * std::sin(phi) +
    0.5 * theta * rr2 - 0.5 * rr2 * std::sin(theta);
}

// [[Rcpp::export]]
NumericVector circle_intersection_CPP(
    NumericVector r1,
    NumericVector r2,
    NumericVector h1,
    NumericVector h2,
    NumericVector dx
) {
  const int n1 = r1.size();
  const int n2 = r2.size();
  const int n3 = h1.size();
  const int n4 = h2.size();
  const int n5 = dx.size();
  if (n1 == 0 || n2 == 0 || n3 == 0 || n4 == 0 || n5 == 0) {
    stop("circle_intersection() needs non-empty arguments.");
  }
  int n = n1;
  if (n2 > n) n = n2;
  if (n3 > n) n = n3;
  if (n4 > n) n = n4;
  if (n5 > n) n = n5;
  NumericVector out(n);
  for (int i = 0; i < n; ++i) {
    out[i] = circle_intersection_one(
      r1[i % n1], r2[i % n2], h1[i % n3], h2[i % n4], dx[i % n5]
    );
  }
  return out;
}

static const double kRad2Deg = 57.29578;

static CharacterVector dist_angle_names() {
  return CharacterVector::create(
    "Ax", "Ay", "Bx", "By", "Cx", "Cy",
    "Laenge_C", "Laenge_B", "Laenge_A",
    "alpha", "betha", "gamma",
    "height1", "height2"
  );
}

static void write_dummy_row(NumericMatrix m, int row, double bx, double by) {
  m(row, 0) = 0;
  m(row, 1) = 0;
  m(row, 2) = bx;
  m(row, 3) = by;
  for (int j = 4; j < 14; ++j) {
    m(row, j) = 0;
  }
}

static bool pair_in_wake(
    double ax, double ay, double az,
    double bx, double by, double bz,
    double wnkl, double dist,
    double* out
) {
  double cx = ax;
  double cy = by;
  double dx = bx - ax;
  double dy = by - ay;
  double lc = std::sqrt(dx * dx + dy * dy);
  double lb = std::fabs(dy);
  double la = std::fabs(dx);
  if (!(R_FINITE(lb) && R_FINITE(la) && R_FINITE(lc)) || lb <= 0 || lb >= dist) {
    return false;
  }
  if (la <= 1e-6) {
    out[0] = ax;
    out[1] = ay;
    out[2] = bx;
    out[3] = by;
    out[4] = cx;
    out[5] = cy;
    out[6] = lb;
    out[7] = lb;
    out[8] = 0;
    out[9] = 0;
    out[10] = 90;
    out[11] = 90;
    out[12] = bz;
    out[13] = az;
    return true;
  }
  double a = la;
  double b = lb;
  double c = lc;
  double ca = (b * b + c * c - a * a) / (2 * b * c);
  double cb = (a * a + c * c - b * b) / (2 * a * c);
  double cc = (a * a + b * b - c * c) / (2 * a * b);
  if (ca > 1) ca = 1;
  if (ca < -1) ca = -1;
  if (cb > 1) cb = 1;
  if (cb < -1) cb = -1;
  if (cc > 1) cc = 1;
  if (cc < -1) cc = -1;
  double alpha = std::acos(ca) * kRad2Deg;
  if (!(R_FINITE(alpha) && alpha < wnkl && lb < dist)) {
    return false;
  }
  out[0] = ax;
  out[1] = ay;
  out[2] = bx;
  out[3] = by;
  out[4] = cx;
  out[5] = cy;
  out[6] = lc;
  out[7] = lb;
  out[8] = la;
  out[9] = alpha;
  out[10] = std::acos(cb) * kRad2Deg;
  out[11] = std::acos(cc) * kRad2Deg;
  out[12] = bz;
  out[13] = az;
  return true;
}

static NumericMatrix dist_angles_one(
    const NumericMatrix& t, int o0, double wnkl, double dist
) {
  const int n = t.nrow();
  const int p = t.ncol();
  const double bx = t(o0, 0);
  const double by = t(o0, 1);
  const double bz = (p >= 3) ? t(o0, 2) : 1.0;
  std::vector<double> buf;
  buf.reserve(static_cast<size_t>(n) * 14);
  int kept = 0;
  for (int j = 0; j < n; ++j) {
    if (!(by < t(j, 1))) {
      continue;
    }
    double row[14];
    const double az = (p >= 3) ? t(j, 2) : 1.0;
    if (pair_in_wake(t(j, 0), t(j, 1), az, bx, by, bz, wnkl, dist, row)) {
      buf.insert(buf.end(), row, row + 14);
      ++kept;
    }
  }
  NumericMatrix out(kept == 0 ? 1 : kept, 14);
  colnames(out) = dist_angle_names();
  if (kept == 0) {
    write_dummy_row(out, 0, bx, by);
    return out;
  }
  for (int r = 0; r < kept; ++r) {
    for (int c = 0; c < 14; ++c) {
      out(r, c) = buf[static_cast<size_t>(r * 14 + c)];
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix get_dist_angles_CPP(NumericMatrix t, int o, double wnkl, double dist) {
  if (t.nrow() < 1 || t.ncol() < 2) {
    stop("t must have X/Y columns.");
  }
  if (o < 1 || o > t.nrow()) {
    stop("o must be a turbine index in 1..nrow(t).");
  }
  return dist_angles_one(t, o - 1, wnkl, dist);
}

// [[Rcpp::export]]
List turbine_influences_CPP(NumericMatrix t, double wnkl, double dist, double dirct) {
  if (t.nrow() < 1 || t.ncol() < 2) {
    stop("t must have X/Y columns.");
  }
  const int n = t.nrow();
  List out(n);
  CharacterVector extra = CharacterVector::create("Windrichtung", "Punkt_id");
  for (int i = 0; i < n; ++i) {
    NumericMatrix ee = dist_angles_one(t, i, wnkl, dist);
    NumericMatrix full(ee.nrow(), 16);
    for (int r = 0; r < ee.nrow(); ++r) {
      for (int c = 0; c < 14; ++c) {
        full(r, c) = ee(r, c);
      }
      full(r, 14) = dirct;
      full(r, 15) = i + 1;
    }
    CharacterVector nm(16);
    CharacterVector base = dist_angle_names();
    for (int c = 0; c < 14; ++c) {
      nm[c] = base[c];
    }
    nm[14] = extra[0];
    nm[15] = extra[1];
    colnames(full) = nm;
    out[i] = full;
  }
  return out;
}
