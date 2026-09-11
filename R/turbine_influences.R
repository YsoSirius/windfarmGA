#' @title Find potentially influencing turbines
#' @name turbine_influences
#' @description Find all turbines that could potentially influence another
#'   turbine and save them to a list.
#'
#' @export
#'
#' @inheritParams calculate_energy
#' @param t A data.frame of the current individual with X and Y coordinates
#' @param dist A numeric value indicating the distance, after which the wake
#'   effects are considered to be eliminated.
#' @param area Site polygon
#' @param dirct Current wind direction
#' @param plot_angles Plot distances and angles
#'
#' @family Wind Energy Calculation Functions
#' @return Returns a list of all individuals of the current generation which
#'   could potentially influence other turbines. List includes the relevant
#'   coordinates, the distances and angles in between and assigns the Point ID.
#'
#' @examples
#' ## Exemplary input Polygon with 2km x 2km:
#' library(sf)
#'
#' area <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(0, 0, 2000, 2000, 0),
#'     c(0, 2000, 2000, 0, 0)
#'   ))),
#'   crs = 3035
#' ))
#'
#' t <- st_coordinates(st_sample(area, 10))
#' t <- cbind(t, "Z" = 1)
#' wnkl <- 20
#' dist <- 100000
#' dirct <- 0
#'
#' res <- turbine_influences(t, wnkl, dist, area, dirct, plot_angles = TRUE)
#'
turbine_influences <- function(t, wnkl, dist, area, dirct,
                               plot_angles = FALSE) {
  lapply(seq_along(t[, 1]), function(i) {
    ee <- get_dist_angles(
      t = t, o = i, wnkl = wnkl, dist = dist,
      area = area, plot_angles = plot_angles
    )
    cbind(ee, "Windrichtung" = dirct, "Punkt_id" = i)
  })
}


#' @title Calculate distances and angles of possibly influencing turbines
#' @name get_dist_angles
#' @description Calculate distances and angles for a turbine and all it's
#'   potentially influencing turbines.
#' @export
#'
#' @param o A numeric value indicating the index of the current turbine
#' @inheritParams turbine_influences
#'
#' @family Wind Energy Calculation Functions
#' @return Returns a matrix with the distances, angles and heights of potentially
#'   influencing turbines
#' @examples
#' library(sf)
#'
#' ## Exemplary input Polygon with 2km x 2km:
#' area <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(cbind(
#'     c(4498482, 4498482, 4499991, 4499991, 4498482),
#'     c(2668272, 2669343, 2669343, 2668272, 2668272)
#'   ))),
#'   crs = 3035
#' ))
#'
#' ## Create a random windfarm with 10 turbines
#' t <- st_coordinates(st_sample(area, 10))
#' t <- cbind(t, "Z" = 1)
#' wnkl <- 20
#' dist <- 100000
#'
#' ## Evaluate and plot for every turbine all other potentially influencing turbines
#' potInfTur <- list()
#' for (i in 1:(length(t[, 1]))) {
#'   potInfTur[[i]] <- get_dist_angles(
#'     t = t, o = i, wnkl = wnkl,
#'     dist = dist, area = area, plot_angles = TRUE
#'   )
#' }
#' potInfTur
#'
get_dist_angles <- function(t, o, wnkl, dist, area, plot_angles = FALSE) {
  col_names <- c(
    "Ax", "Ay", "Bx", "By", "Cx", "Cy",
    "Laenge_C", "Laenge_B", "Laenge_A",
    "alpha", "betha", "gamma",
    "height1", "height2"
  )
  turbine_loc <- c(x = t[o, 1L], y = t[o, 2L], z = t[o, 3L])
  turbines_ahead <- subset.matrix(x = t, subset = (t[o, 2L] < t[, 2L]))

  if (plot_angles) {
    graphics::plot(t[, 1], t[, 2],
      col.axis = "darkblue",
      xlab = "X-Coordinates", ylab = "Y-Coordinates"
    )
    title(
      main = "Potentially Influential Turbines",
      sub = paste(
        "PointNr: ", o, ";", "Distance: ",
        dist, "Meter", ";", "Angle: ", wnkl, "Degrees"
      ),
      outer = FALSE, cex.main = 1, cex.sub = 1
    )
    plot(st_geometry(area), add = TRUE)
    points(x = turbine_loc[1], y = turbine_loc[2], col = "green", pch = 20, cex = 2)
    calibrate::textxy(turbine_loc[1], turbine_loc[2], "Point B")
    angles_min <- rbind(
      turbine_loc[1:2],
      cbind(
        turbine_loc[1] + dist * cos((90 - wnkl) * pi / 180),
        turbine_loc[2] + dist * sin((90 - wnkl) * pi / 180)
      )
    )
    angles_plu <- rbind(
      turbine_loc[1:2],
      cbind(
        turbine_loc[1] + dist * cos((90 + wnkl) * pi / 180),
        turbine_loc[2] + dist * sin((90 + wnkl) * pi / 180)
      )
    )
    lines(angles_min[, 1], angles_min[, 2])
    lines(angles_plu[, 1], angles_plu[, 2])
    points(x = turbines_ahead[, 1], y = turbines_ahead[, 2], col = "red", pch = 20)
  }

  len2 <- length(turbines_ahead[, 1L])
  if (len2 != 0L) {
    datalist <- lapply(1:len2, function(i) {
      turb_tmp <- turbines_ahead[i, ]
      P2LFu <- point_2_line_CPP(turbine_loc, turb_tmp)
      winkel <- angles_CPP(turb_tmp, turbine_loc, P2LFu[5:6])
      c(P2LFu, winkel, "Height1" = turbine_loc[[3]], "Height2" = turb_tmp[["Z"]])
    })
    res <- matrix(unlist(datalist), ncol = 14, byrow = TRUE)
    colnames(res) <- col_names

    dl <- subset.matrix(res,
      subset = res[, "alpha"] < wnkl & res[, "Laenge_B"] < dist
    )

    if (plot_angles) {
      points(dl[, "Ax"], dl[, "Ay"], col = "orange", pch = 20, cex = 2)
      calibrate::textxy(dl[, "Ax"], dl[, "Ay"], rep("Points A", nrow(dl)))
    }

    if (nrow(dl) != 0) {
      DataLun3 <- dl
    } else {
      DataLun3 <- matrix(data = c(0, 0, t[o, 1], t[o, 2], rep(0, 10)), nrow = 1, ncol = 14)
      colnames(DataLun3) <- col_names
    }
  } else {
    DataLun3 <- matrix(data = c(0, 0, t[o, 1], t[o, 2], rep(0, 10)), nrow = 1, ncol = 14)
    colnames(DataLun3) <- col_names
  }

  return(DataLun3)
}
