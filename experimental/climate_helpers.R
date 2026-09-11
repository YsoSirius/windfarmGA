## Wind climate + open reference turbines (GitHub only, not on CRAN).
## source("experimental/climate_helpers.R")
##
## bReeze / ecmwfr / mcera5 are optional. Do not add them to Suggests.
## Manufacturer PDFs stay out of the package; only IEA/NREL reference CSVs.

nrel_curve_catalog <- function() {
  base <- "https://raw.githubusercontent.com/NREL/turbine-models/main/turbine_models/data"
  data.frame(
    name = c(
      "IEA_3.4MW_130",
      "IEA_10MW_198",
      "IEA_15MW_240",
      "NREL_5MW_126"
    ),
    folder = c("Onshore", "Offshore", "Offshore", "Offshore"),
    file = c(
      "IEA_Reference_3.4MW_130.csv",
      "IEA_Reference_10MW_198.csv",
      "IEA_Reference_15MW_240.csv",
      "NREL_Reference_5MW_126.csv"
    ),
    rotor_m = c(130, 198, 240, 126),
    hub_m = c(110, 119, 150, 90),
    rated_kw = c(3370, 10000, 15000, 5000),
    url = c(
      paste0(base, "/Onshore/IEA_Reference_3.4MW_130.csv"),
      paste0(base, "/Offshore/IEA_Reference_10MW_198.csv"),
      paste0(base, "/Offshore/IEA_Reference_15MW_240.csv"),
      paste0(base, "/Offshore/NREL_Reference_5MW_126.csv")
    ),
    stringsAsFactors = FALSE
  )
}

nrel_fetch_curve <- function(name = "IEA_3.4MW_130", destfile = tempfile(fileext = ".csv")) {
  catlg <- nrel_curve_catalog()
  row <- catlg[catlg$name == name, , drop = FALSE]
  if (!nrow(row)) {
    stop(
      "Unknown turbine '", name, "'. Choose one of: ",
      paste(catlg$name, collapse = ", ")
    )
  }
  utils::download.file(row$url[[1]], destfile, mode = "wb", quiet = TRUE)
  curve <- windfarmGA::read_power_curve(destfile)
  attr(curve, "rotor") <- row$rotor_m[[1]] / 2
  attr(curve, "rotor_height") <- row$hub_m[[1]]
  attr(curve, "rated_kw") <- row$rated_kw[[1]]
  attr(curve, "source") <- row$url[[1]]
  curve
}

wind_from_breeze <- function(x, dir_width = 30, set = 1L) {
  if (!requireNamespace("bReeze", quietly = TRUE)) {
    stop("install.packages('bReeze')")
  }
  dat <- breeze_speed_dir(x, set = set)
  windfarmGA::wind_from_series(dat$ws, dat$wd, dir_width = dir_width)
}

breeze_speed_dir <- function(x, set = 1L) {
  if (inherits(x, "mast") || (is.list(x) && !is.null(x$sets))) {
    return(breeze_speed_dir(x$sets[[set]], set = 1L))
  }
  dat <- if (is.list(x) && !is.null(x$data)) {
    as.data.frame(x$data)
  } else {
    as.data.frame(x)
  }
  nms <- gsub("[^a-z0-9]+", "", tolower(names(dat)))
  ws_i <- match(c("vavg", "speed", "ws", "v"), nms)
  wd_i <- match(c("diravg", "dir", "wd", "direction"), nms)
  ws_i <- ws_i[!is.na(ws_i)][1]
  wd_i <- wd_i[!is.na(wd_i)][1]
  if (is.na(ws_i) || is.na(wd_i)) {
    stop("bReeze object needs speed (v.avg/ws) and direction (dir.avg/wd).")
  }
  data.frame(ws = as.numeric(dat[[ws_i]]), wd = as.numeric(dat[[wd_i]]))
}

## get_era5_wind() from download_ERA5_historic.R -> rose:
##   source("experimental/download_ERA5_historic.R")
##   source("experimental/climate_helpers.R")
##   era5 <- get_era5_wind(area, "2021-01-01", "2025-12-31")  # NetCDF in tempdir()
##   wind <- wind_from_era5(era5)                 # uses era5$hourly$u100/v100
##   genetic_algorithm(..., wind = wind, reference_height = 100)
##
## bReeze mast (install.packages("bReeze")):
##   library(bReeze)
##   data(winddata)
##   s40 <- set(height = 40, v.avg = winddata[, 2], dir.avg = winddata[, 14])
##   mast <- mast(timestamp = timestamp(timestamp = winddata[, 1]), s40)
##   wind <- wind_from_breeze(mast)               # set = 1 is the first height
## Or skip bReeze if the CSV already has speed + direction:
##   wind <- wind_from_series(dat$ws, dat$wd)

wind_from_era5 <- function(x, u = NULL, v = NULL, dir_width = 30) {
  x <- era5_unwrap(x)
  uv <- tryCatch(era5_uv(x, u = u, v = v), error = function(e) NULL)
  if (!is.null(uv)) {
    return(windfarmGA::wind_from_uv(uv$u, uv$v, dir_width = dir_width))
  }
  series <- era5_speed_dir(x)
  if (!is.null(series)) {
    return(windfarmGA::wind_from_series(series$ws, series$wd, dir_width = dir_width))
  }
  stop(
    "Need u/v (u100/v100 or u10/v10) or wind_speed + wind_direction. ",
    "Pass the list from get_era5_wind() or its $hourly table."
  )
}

era5_unwrap <- function(x) {
  if (is.list(x) && !is.data.frame(x) && !inherits(x, "SpatRaster") &&
    !is.null(x$hourly)) {
    return(x$hourly)
  }
  x
}

era5_uv <- function(x, u = NULL, v = NULL) {
  u_names <- c(
    "u100", "100u", "u10",
    "u_component_of_wind_10m", "100m_u_component_of_wind", "u"
  )
  v_names <- c(
    "v100", "100v", "v10",
    "v_component_of_wind_10m", "100m_v_component_of_wind", "v"
  )
  if (inherits(x, "SpatRaster") && requireNamespace("terra", quietly = TRUE)) {
    nms <- names(x)
    u_layer <- era5_pick_name(nms, u, u_names)
    v_layer <- era5_pick_name(nms, v, v_names)
    return(data.frame(
      u = as.numeric(terra::values(x[[u_layer]])),
      v = as.numeric(terra::values(x[[v_layer]]))
    ))
  }
  dat <- as.data.frame(x)
  nms <- names(dat)
  u_col <- era5_pick_name(nms, u, u_names)
  v_col <- era5_pick_name(nms, v, v_names)
  data.frame(u = as.numeric(dat[[u_col]]), v = as.numeric(dat[[v_col]]))
}

era5_speed_dir <- function(x) {
  if (inherits(x, "SpatRaster")) {
    return(NULL)
  }
  dat <- as.data.frame(x)
  nms <- gsub("[^a-z0-9]+", "", tolower(names(dat)))
  ws_i <- match(c("windspeed100m", "windspeed", "ws"), nms)
  wd_i <- match(c("winddirectiondeg", "winddirection", "wd"), nms)
  ws_i <- ws_i[!is.na(ws_i)][1]
  wd_i <- wd_i[!is.na(wd_i)][1]
  if (is.na(ws_i) || is.na(wd_i)) {
    return(NULL)
  }
  data.frame(ws = as.numeric(dat[[ws_i]]), wd = as.numeric(dat[[wd_i]]))
}

era5_pick_name <- function(nms, explicit, candidates) {
  if (!is.null(explicit) && explicit %in% nms) {
    return(explicit)
  }
  low <- gsub("[^a-z0-9]+", "", tolower(nms))
  for (cand in candidates) {
    hit <- which(low == tolower(gsub("[^a-z0-9]+", "", cand)))
    if (length(hit)) {
      return(nms[hit[1]])
    }
  }
  stop(
    "Could not find a u/v column. Names were: ",
    paste(nms, collapse = ", ")
  )
}

## Other GWA GIS layers (same URL pattern): capacity-factor, elevation,
## roughness-length, combined-Weibull-A/k per height 10/50/100/150/200.
gwa_layers <- function() {
  c(
    "wind-speed",
    "air-density",
    "combined-Weibull-A",
    "combined-Weibull-k",
    "power-density"
  )
}

gwa_country_url <- function(iso3, layer, height = 100) {
  sprintf(
    "https://globalwindatlas.info/api/gis/country/%s/%s/%d",
    toupper(iso3), layer, as.integer(height)
  )
}

## Global Wind Atlas country GeoTIFFs (~250 m). No API key.
## ERA5 (COPERNICUS_CLIMATE_DATA) is ~31 km: good for a directional rose
## over time, weak as a spatial wind field. Prefer GWA Weibull A/k here.
##
## gwa <- gwa_download_country("AUT", height = 100)
## genetic_algorithm(..., weibull = TRUE, weibull_src = gwa$weibull_src)
gwa_download_country <- function(iso3 = "AUT",
                                 height = 100,
                                 layers = gwa_layers(),
                                 dest_dir = file.path("_experiment", "gwa", toupper(iso3))) {
  iso3 <- toupper(iso3)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(length(layers))
  names(paths) <- layers
  for (layer in layers) {
    dest <- file.path(dest_dir, sprintf("%s_%s_%dm.tif", iso3, layer, height))
    if (!file.exists(dest) || !file.info(dest)$size) {
      url <- gwa_country_url(iso3, layer, height)
      message("GET ", url)
      utils::download.file(url, dest, mode = "wb", quiet = TRUE)
      if (!file.exists(dest) || file.info(dest)$size < 1000) {
        unlink(dest)
        stop(
          "GWA download failed for ", layer, ".\n",
          "Open the URL in a browser and save the GeoTIFF:\n  ", url
        )
      }
    }
    paths[[layer]] <- normalizePath(dest, winslash = "/", mustWork = TRUE)
  }
  weibull_src <- NULL
  if (all(c("combined-Weibull-k", "combined-Weibull-A") %in% names(paths))) {
    weibull_src <- list(
      paths[["combined-Weibull-k"]],
      paths[["combined-Weibull-A"]]
    )
  }
  list(iso3 = iso3, height = height, files = paths, weibull_src = weibull_src)
}

copernicus_cds_key <- function() {
  key <- Sys.getenv("COPERNICUS_CLIMATE_DATA", unset = "")
  if (!nzchar(key)) {
    stop(
      "Set the CDS key, e.g. Sys.setenv(COPERNICUS_CLIMATE_DATA = \"...\") ",
      "or add it to ~/.Renviron."
    )
  }
  key
}

# Example after you downloaded ERA5 or have a bReeze mast:
# wind <- wind_from_era5(era5_df)
# curve <- nrel_fetch_curve("IEA_3.4MW_130")
# ga_options(power_curve = curve)
# genetic_algorithm(
#   area = polygon, wind = wind, n = 15,
#   rotor = attr(curve, "rotor"),
#   rotor_height = attr(curve, "rotor_height")
# )
#
# GWA Austria at 100 m (spatial field + Weibull for genetic_algorithm):
# gwa <- gwa_download_country("AUT", 100)
# genetic_algorithm(..., weibull = TRUE, weibull_src = gwa$weibull_src)


