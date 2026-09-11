## CDS time series at the nearest 0.25° point (100 m u/v).
## Needs ecmwfr + a CDS key, e.g. Sys.getenv("COPERNICUS_CLIMATE_DATA").
##
##   source("experimental/download_ERA5_historic.R")
##   source("experimental/climate_helpers.R")
##   era5 <- get_era5_wind(area, "2021-01-01", "2025-12-31")  # NetCDF in tempdir()
##   wind <- wind_from_era5(era5)
##   genetic_algorithm(..., wind = wind, reference_height = 100)
##
## Do not source this file just to get a rose — that would re-request CDS.
## If you already have era5$hourly, only wind_from_era5() is needed.

library(sf)
library(ecmwfr)
library(ncdf4)
library(dplyr)
library(lubridate)
library(windfarmGA)

# 1. Polygon ------------------------------------------------------------
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669781, 2669781, 2668272, 2668272)
  ))),
  crs = 3035
))

# 2. Hilfsfunktion: ERA5 Winddaten für ein Polygon laden ------------------------------------------------------------
get_era5_wind <- function(
    polygon,
    start_date = "2022-01-01",
    end_date   = "2026-09-10",
    out_dir    = tempfile("windfarmGA_era5_")
) {

  key <- Sys.getenv("COPERNICUS_CLIMATE_DATA", unset = "")
  if (nzchar(key)) {
    try(ecmwfr::wf_set_key(key = key), silent = TRUE)
  }

  # ----------------------------------------------------------
  # Mittelpunkt des Polygons bestimmen
  centroid <- polygon |>
    sf::st_union() |>
    sf::st_centroid() |>
    sf::st_transform(4326)

  coords <- sf::st_coordinates(centroid)
  lon <- as.numeric(coords[1, "X"])
  lat <- as.numeric(coords[1, "Y"])


  # ----------------------------------------------------------
  # Nächsten ERA5 0.25° Grid Point bestimmen
  era5_lon <- round(lon / 0.25) * 0.25
  era5_lat <- round(lat / 0.25) * 0.25

  message("Polygon Mittelpunkt: ", round(lat, 5), ", ", round(lon, 5))
  message("ERA5 Grid Point: ", era5_lat, ", ", era5_lon)


  # ----------------------------------------------------------
  # Download-Datei
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  message("ERA5 download directory: ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))
  target <- paste0("era5_wind_100m_", start_date, "_", end_date, ".nc")


  # ----------------------------------------------------------
  # ERA5 Time Series Request
  request <- list(
    dataset_short_name = "reanalysis-era5-single-levels-timeseries",
    variable = c("100m_u_component_of_wind", "100m_v_component_of_wind"),
    location = list(longitude = era5_lon, latitude  = era5_lat),
    date = paste0(start_date, "/", end_date),
    data_format = "netcdf",
    target = target
  )


  # ----------------------------------------------------------
  # Request an CDS senden
  nc_file <- ecmwfr::wf_request(
    request  = request,
    transfer = TRUE,
    path     = out_dir,
    verbose  = TRUE
  )
  # ----------------------------------------------------------
  # CDS kann NetCDF als ZIP zurückgeben
  if (grepl("\\.zip$", nc_file, ignore.case = TRUE)) {
    message("ZIP-Datei erkannt: ", nc_file)
    extract_dir <- file.path(
      out_dir,
      tools::file_path_sans_ext(basename(nc_file))
    )

    dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

    unzip(nc_file, exdir = extract_dir)

    nc_files <- list.files(
      extract_dir,
      pattern = "\\.nc$",
      full.names = TRUE,
      recursive = TRUE
    )

    if (length(nc_files) == 0) {
      stop("ZIP wurde entpackt, aber keine .nc-Datei gefunden.")
    }
    if (length(nc_files) > 1) {
      message("Mehrere NetCDF-Dateien gefunden:\n",
              paste(nc_files, collapse = "\n")
      )
    }

    nc_file <- nc_files[1]
  }


  # ----------------------------------------------------------
  # NetCDF öffnen
  nc <- ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)


  # ----------------------------------------------------------
  # Variablennamen automatisch finden
  #
  # Typischerweise:
  # u100 = 100 m U-Komponente
  # v100 = 100 m V-Komponente
  # ----------------------------------------------------------
  variable_names <- names(nc$var)
  find_variable <- function(
    direct_names,
    long_name_pattern
  ) {

    hit <- intersect(
      direct_names,
      variable_names
    )

    if (length(hit) > 0) {
      return(hit[1])
    }

    long_names <- sapply(
      variable_names,
      function(x) {

        att <- ncdf4::ncatt_get(
          nc,
          x,
          "long_name"
        )

        if (isTRUE(att$hasatt)) {
          att$value
        } else {
          ""
        }
      }
    )

    hit <- grep(
      long_name_pattern,
      long_names,
      ignore.case = TRUE
    )

    if (length(hit) == 0) {
      stop(
        "ERA5 Windvariable konnte nicht gefunden werden. ",
        "Vorhandene Variablen: ",
        paste(variable_names, collapse = ", ")
      )
    }

    variable_names[hit[1]]
  }


  u_name <- find_variable(
    c("u100", "100u"),
    "100.*U.*wind"
  )

  v_name <- find_variable(
    c("v100", "100v"),
    "100.*V.*wind"
  )


  message("U variable: ", u_name)
  message("V variable: ", v_name)


  # ----------------------------------------------------------
  # Windkomponenten auslesen
  # ----------------------------------------------------------

  u100 <- as.numeric(
    ncdf4::ncvar_get(
      nc,
      u_name
    )
  )

  v100 <- as.numeric(
    ncdf4::ncvar_get(
      nc,
      v_name
    )
  )


  # ----------------------------------------------------------
  # Zeitvariable finden
  # ----------------------------------------------------------

  all_time_names <- unique(
    c(
      names(nc$dim),
      names(nc$var)
    )
  )

  time_name <- intersect(
    c("valid_time", "time"),
    all_time_names
  )

  if (length(time_name) == 0) {
    stop("Keine Zeitvariable gefunden.")
  }

  time_name <- time_name[1]


  # ----------------------------------------------------------
  # Zeitwerte + Units lesen
  # ----------------------------------------------------------

  if (time_name %in% names(nc$dim)) {

    time_values <- nc$dim[[time_name]]$vals
    time_units  <- nc$dim[[time_name]]$units

  } else {

    time_values <- ncdf4::ncvar_get(
      nc,
      time_name
    )

    time_units <- ncdf4::ncatt_get(
      nc,
      time_name,
      "units"
    )$value
  }


  # ----------------------------------------------------------
  # CF-NetCDF-Zeit in POSIXct umwandeln
  # ----------------------------------------------------------

  parse_cf_time <- function(
    values,
    units
  ) {

    match <- regexec(
      "^(seconds|minutes|hours|days) since (.+)$",
      units,
      ignore.case = TRUE
    )

    parts <- regmatches(
      units,
      match
    )[[1]]

    if (length(parts) < 3) {
      stop(
        "Unbekanntes Zeitformat: ",
        units
      )
    }

    unit <- tolower(parts[2])

    origin_string <- parts[3]

    origin_string <- sub(
      " UTC$",
      "",
      origin_string
    )

    origin <- as.POSIXct(
      origin_string,
      tz = "UTC"
    )

    factor <- switch(
      unit,
      seconds = 1,
      minutes = 60,
      hours   = 3600,
      days    = 86400
    )

    origin + values * factor
  }


  timestamp <- parse_cf_time(
    time_values,
    time_units
  )


  # ----------------------------------------------------------
  # Tatsächlichen ERA5 Punkt aus NetCDF holen
  # ----------------------------------------------------------

  read_coord <- function(
    name,
    fallback
  ) {

    if (name %in% names(nc$dim)) {

      return(
        as.numeric(
          nc$dim[[name]]$vals[1]
        )
      )
    }

    if (name %in% names(nc$var)) {

      return(
        as.numeric(
          ncdf4::ncvar_get(
            nc,
            name
          )[1]
        )
      )
    }

    fallback
  }


  actual_lat <- read_coord(
    "latitude",
    era5_lat
  )

  actual_lon <- read_coord(
    "longitude",
    era5_lon
  )


  # ----------------------------------------------------------
  # Data Frame erstellen
  # ----------------------------------------------------------

  hourly <- tibble::tibble(

    timestamp = timestamp,

    latitude = actual_lat,
    longitude = actual_lon,

    u100 = u100,
    v100 = v100

  ) |>

    mutate(

      # Windgeschwindigkeit in m/s
      wind_speed_100m = sqrt(
        u100^2 + v100^2
      ),

      # meteorologische Windrichtung:
      # 0   = Nord
      # 90  = Ost
      # 180 = Süd
      # 270 = West
      wind_direction_deg =
        (
          atan2(
            -u100,
            -v100
          ) * 180 / pi
        ) %% 360
    )


  # ----------------------------------------------------------
  # Monatliche Aggregation
  # ----------------------------------------------------------

  monthly <- hourly |>

    mutate(
      year  = lubridate::year(timestamp),
      month = lubridate::month(timestamp),
      year_month = floor_date(
        timestamp,
        "month"
      )
    ) |>

    group_by(
      year,
      month,
      year_month
    ) |>

    summarise(

      n_hours = n(),

      wind_mean = mean(
        wind_speed_100m,
        na.rm = TRUE
      ),

      wind_median = median(
        wind_speed_100m,
        na.rm = TRUE
      ),

      wind_sd = sd(
        wind_speed_100m,
        na.rm = TRUE
      ),

      wind_p10 = quantile(
        wind_speed_100m,
        0.10,
        na.rm = TRUE
      ),

      wind_p50 = quantile(
        wind_speed_100m,
        0.50,
        na.rm = TRUE
      ),

      wind_p90 = quantile(
        wind_speed_100m,
        0.90,
        na.rm = TRUE
      ),

      wind_max = max(
        wind_speed_100m,
        na.rm = TRUE
      ),

      .groups = "drop"
    )


  # ----------------------------------------------------------
  # Jährliche Aggregation
  # ----------------------------------------------------------

  yearly <- hourly |>

    mutate(
      year = lubridate::year(timestamp)
    ) |>

    group_by(year) |>

    summarise(

      n_hours = n(),

      wind_mean = mean(
        wind_speed_100m,
        na.rm = TRUE
      ),

      wind_median = median(
        wind_speed_100m,
        na.rm = TRUE
      ),

      wind_sd = sd(
        wind_speed_100m,
        na.rm = TRUE
      ),

      wind_p10 = quantile(
        wind_speed_100m,
        0.10,
        na.rm = TRUE
      ),

      wind_p50 = quantile(
        wind_speed_100m,
        0.50,
        na.rm = TRUE
      ),

      wind_p90 = quantile(
        wind_speed_100m,
        0.90,
        na.rm = TRUE
      ),

      wind_max = max(
        wind_speed_100m,
        na.rm = TRUE
      ),

      .groups = "drop"
    )


  # ----------------------------------------------------------
  # Ergebnis
  # ----------------------------------------------------------

  list(

    polygon_center = data.frame(
      latitude = lat,
      longitude = lon
    ),

    era5_point = data.frame(
      latitude = actual_lat,
      longitude = actual_lon
    ),

    hourly = hourly,
    monthly = monthly,
    yearly = yearly,

    nc_file = nc_file
  )
}



if (FALSE) {

source("experimental/climate_helpers.R")
era5 <- get_era5_wind(area, "2021-01-01", "2025-12-31")
wind <- wind_from_era5(era5)   # nimmt era5$hourly
plot_windrose(wind, spd = "ws", dir = "wd")
genetic_algorithm(
  area = area, wind = wind, n = 12,
  rotor = 65, rotor_height = 110,
  reference_height = 100        # Download ist 100 m, nicht 10 m
)

wind_summary <- era5$hourly |>
  summarise(
    mean_wind = mean(
      wind_speed_100m,
      na.rm = TRUE
    ),
    median_wind = median(
      wind_speed_100m,
      na.rm = TRUE
    ),
    p90_wind = quantile(
      wind_speed_100m,
      0.90,
      na.rm = TRUE
    ),
    pct_over_5ms =
      mean(
        wind_speed_100m >= 5,
        na.rm = TRUE
      ) * 100,
    pct_over_6ms =
      mean(
        wind_speed_100m >= 6,
        na.rm = TRUE
      ) * 100,

    pct_over_7ms =
      mean(
        wind_speed_100m >= 7,
        na.rm = TRUE
      ) * 100,
    pct_over_10ms =
      mean(
        wind_speed_100m >= 10,
        na.rm = TRUE
      ) * 100
  )
wind_summary

library(ggplot2)
ggplot(era5$monthly, aes(x = year_month, y = wind_mean)) +
  geom_line() +
  geom_point() +
  labs(
    x = NULL,
    y = "Windgeschwindigkeit 100 m [m/s]",
    title = "ERA5 – monatliche mittlere Windgeschwindigkeit"
  )
}
