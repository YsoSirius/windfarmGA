# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

rotate_CPP <- function(X1, Y1, Px, Py, angle) {
    .Call(`_windfarmGA_rotate_CPP`, X1, Y1, Px, Py, angle)
}

angles_CPP <- function(Aa, Bb, Cc) {
    .Call(`_windfarmGA_angles_CPP`, Aa, Bb, Cc)
}

energy_calc_CPP <- function(wind_speed, rotor_radius, air_rh) {
    .Call(`_windfarmGA_energy_calc_CPP`, wind_speed, rotor_radius, air_rh)
}

euc_CPP <- function(x1, x2, y1, y2) {
    .Call(`_windfarmGA_euc_CPP`, x1, x2, y1, y2)
}

point_2_line_CPP <- function(x, y) {
    .Call(`_windfarmGA_point_2_line_CPP`, x, y)
}

