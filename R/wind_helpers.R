#' @title Wind rose from u/v components
#' @name wind_from_uv
#' @description Bin ERA5-style eastward (`u`) and northward (`v`) wind
#'   components into the `ws` / `wd` / `probab` table that
#'   [windata_format()] and [genetic_algorithm()] expect.
#'   Direction is meteorological (where the wind comes from; 0 = north).
#'   Each direction bin gets the mean speed and the hour share.
#' @export
#'
#' @param u Eastward wind component (m/s). Same length as `v`.
#' @param v Northward wind component (m/s).
#' @param dir_width Width of direction bins in degrees. Default is 30.
#' @return A data.frame with `ws`, `wd`, `probab` (probabilities sum to 100).
#'
#' @examples
#' set.seed(1)
#' u <- rnorm(200, 2)
#' v <- rnorm(200, -3)
#' wind_from_uv(u, v, dir_width = 30)
#'
#' @family Helper Functions
wind_from_uv <- function(u, v, dir_width = 30) {
  u <- as.numeric(u)
  v <- as.numeric(v)
  if (length(u) != length(v)) {
    stop("u and v must have the same length.")
  }
  ok <- is.finite(u) & is.finite(v)
  u <- u[ok]
  v <- v[ok]
  if (!length(u)) {
    stop("No finite u/v values.")
  }
  ws <- sqrt(u * u + v * v)
  wd <- (atan2(-u, -v) * (180 / pi)) %% 360
  wind_from_series(ws, wd, dir_width = dir_width)
}

#' @title Wind rose from speed and direction series
#' @name wind_from_series
#' @description Bin a time series of hub-height speed and meteorological
#'   direction into `ws` / `wd` / `probab`.
#' @export
#'
#' @param ws Wind speed (m/s).
#' @param wd Direction in degrees (0 = north, clockwise, where the wind
#'   comes from).
#' @param dir_width Width of direction bins in degrees. Default is 30.
#' @return A data.frame with `ws`, `wd`, `probab`.
#'
#' @family Helper Functions
wind_from_series <- function(ws, wd, dir_width = 30) {
  ws <- as.numeric(ws)
  wd <- as.numeric(wd) %% 360
  if (length(ws) != length(wd)) {
    stop("ws and wd must have the same length.")
  }
  dir_width <- as.numeric(dir_width)[1]
  if (!is.finite(dir_width) || dir_width <= 0 || dir_width > 360) {
    stop("dir_width must be in (0, 360].")
  }
  ok <- is.finite(ws) & is.finite(wd) & ws >= 0
  ws <- ws[ok]
  wd <- wd[ok]
  if (!length(ws)) {
    stop("No finite wind observations.")
  }
  n_bin <- as.integer(round(360 / dir_width))
  width <- 360 / n_bin
  idx <- floor(wd / width)
  idx[idx >= n_bin] <- n_bin - 1L
  mid <- (idx + 0.5) * width
  mid[mid >= 360] <- mid[mid >= 360] - 360
  bins <- sort(unique(idx))
  out <- do.call(rbind, lapply(bins, function(b) {
    take <- idx == b
    data.frame(
      ws = mean(ws[take]),
      wd = mid[take][1],
      probab = 100 * sum(take) / length(idx)
    )
  }))
  out[order(out$wd), , drop = FALSE]
}

#' @title Read a manufacturer or NREL/IEA power-curve table
#' @name read_power_curve
#' @description Parse a CSV (or data.frame) with wind speed and power
#'   in kW. Understands NREL/IEA headers such as `Wind Speed [m/s]`
#'   and `Power [kW]`. Optional `Ct` is returned as an attribute.
#'   Does not ship manufacturer curves; pass your own file or an open
#'   IEA/NREL reference CSV. See `experimental/climate_helpers.R`.
#' @export
#'
#' @param file Path to a CSV, or a data.frame.
#' @return A data.frame with `ws` and `power`. Attribute `ct` is a
#'   matching data.frame when a thrust column is present.
#'
#' @examples
#' curve <- data.frame(
#'   `Wind Speed [m/s]` = c(3, 8, 12, 25),
#'   `Power [kW]` = c(0, 1200, 2000, 2000),
#'   check.names = FALSE
#' )
#' read_power_curve(curve)
#'
#' @family Helper Functions
read_power_curve <- function(file) {
  raw <- if (is.data.frame(file)) {
    as.data.frame(file, stringsAsFactors = FALSE)
  } else {
    utils::read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  }
  if (!ncol(raw)) {
    stop("Empty power-curve table.")
  }
  key <- gsub("[^a-z0-9]+", "", tolower(names(raw)))
  ws_i <- power_curve_match_col(key, c("windspeedms", "windspeed", "speed", "ws"))
  pw_i <- power_curve_match_col(key, c("powerkw", "powerw", "power"))
  if (!length(ws_i) || !length(pw_i)) {
    stop("Need a wind-speed column and a power column (kW).")
  }
  curve <- data.frame(
    ws = as.numeric(raw[[ws_i[1]]]),
    power = as.numeric(raw[[pw_i[1]]])
  )
  curve <- curve[is.finite(curve$ws) & is.finite(curve$power), , drop = FALSE]
  curve <- curve[order(curve$ws), , drop = FALSE]
  ct_i <- power_curve_match_col(key, c("ct", "thrustcoefficient", "thrustcoef"))
  if (length(ct_i)) {
    attr(curve, "ct") <- data.frame(
      ws = curve$ws,
      ct = as.numeric(raw[[ct_i[1]]])[order(as.numeric(raw[[ws_i[1]]]))]
    )
  }
  curve
}

power_curve_match_col <- function(key, candidates) {
  for (cand in candidates) {
    hit <- which(key == cand)
    if (length(hit)) {
      return(hit)
    }
  }
  for (cand in candidates) {
    hit <- which(startsWith(key, cand))
    if (length(hit)) {
      return(hit)
    }
  }
  integer()
}
