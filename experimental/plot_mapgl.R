## 3D MapLibre map of a GA result on our own DEM (experimental).
##
##   source("experimental/plot_mapgl.R")
##   plot_mapgl_from_result(result, area, buffer = 8000)
##   plot_mapgl_from_result(result, area, basemap = "satellite")
##   # OBJ is experimental/wind_turbine_v1.obj (same as rayshader).
##
## Close old RStudio Viewer tabs before re-plotting (stale Three.js
## hooks keep loading three@0.160 + OBJLoader). Hook version: v4.
## Existing terrain_tiles/ + terrain.mbtiles are reused. Set
## rebuild = TRUE to remake. Port 8000 leftover servers are stopped.
## Layers control: Satellite, Topo, Site, Wake cones (green = 0% wake),
## Turbines (OBJ only). No HTML pins, no rotor-radius pads.
## Enlarges the DEM (buffer), fills NA, encodes Terrarium PNG tiles,
## writes MBTiles, serves them locally, then mapgl::set_terrain().
## Not in Suggests. Needs mapgl, terra, sf, elevatr, png; RSQLite
## for the .mbtiles file. Tiles/json/mbtiles are gitignored.

.mg_script_dir <- local({
  ofile <- NULL
  for (i in sys.nframe():1) {
    f <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(f) && nzchar(f)) {
      ofile <- f
      break
    }
  }
  if (!is.null(ofile)) {
    dirname(normalizePath(ofile, winslash = "/", mustWork = FALSE))
  } else {
    normalizePath("experimental", winslash = "/", mustWork = FALSE)
  }
})

mg_need <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(TRUE)
  }
  stop("install.packages(\"", pkg, "\")", call. = FALSE)
}

mg_fill_na <- function(dem, passes = 16L, w = 5L) {
  if (!anyNA(terra::values(dem))) {
    return(dem)
  }
  message("DEM has NA cells; filling by focal mean (", passes, " passes).")
  for (i in seq_len(passes)) {
    if (!anyNA(terra::values(dem))) {
      break
    }
    dem <- terra::focal(
      dem, w = w, fun = "mean",
      na.policy = "only", na.rm = TRUE
    )
  }
  leftover <- anyNA(terra::values(dem))
  if (leftover) {
    m <- mean(terra::values(dem), na.rm = TRUE)
    fill <- if (is.finite(m)) m else 0
    message("Remaining NA set to ", round(fill, 1), " m.")
    dem <- terra::subst(dem, NA, fill)
  }
  dem
}

mg_prepare_dem <- function(area, dem = NULL, result = NULL,
                           buffer = 8000, z = 12) {
  mg_need("terra")
  mg_need("sf")
  area <- windfarmGA::isSpatial(area)
  pad <- sf::st_buffer(area, dist = buffer)
  stored <- NULL
  if (is.null(dem) && !is.null(result)) {
    tm <- tryCatch(windfarmGA:::ga_result_terrain(result), error = function(e) NULL)
    if (!is.null(tm) && !is.null(tm$srtm_crop)) {
      stored <- tm$srtm_crop[[1]]
    }
  }
  covers <- function(r, poly) {
    if (is.null(r) || !inherits(r, "SpatRaster")) {
      return(FALSE)
    }
    poly <- sf::st_transform(poly, terra::crs(r))
    e <- terra::ext(r)
    b <- sf::st_bbox(poly)
    as.numeric(e[1]) <= b[["xmin"]] &&
      as.numeric(e[2]) >= b[["xmax"]] &&
      as.numeric(e[3]) <= b[["ymin"]] &&
      as.numeric(e[4]) >= b[["ymax"]]
  }
  if (is.null(dem) && covers(stored, pad)) {
    dem <- stored
  } else if (is.null(dem)) {
    mg_need("elevatr")
    message("Downloading DEM for a ", buffer, " m buffer (elevatr z = ", z, ").")
    wgs <- sf::st_transform(pad, 4326)
    dem <- terra::rast(elevatr::get_elev_raster(
      locations = wgs, z = z, clip = "bbox"
    ))
  } else if (!inherits(dem, "SpatRaster")) {
    dem <- terra::rast(dem)
  }
  dem <- terra::project(dem, terra::crs(area))
  dem <- terra::crop(dem, terra::vect(pad), snap = "out")
  mg_fill_na(dem)
}

mg_lonlat_tile <- function(lon, lat, zoom) {
  n <- 2^zoom
  x <- as.integer(floor((lon + 180) / 360 * n))
  lat <- pmin(pmax(lat, -85.0511), 85.0511)
  lat_rad <- lat * pi / 180
  y <- as.integer(floor((1 - log(tan(lat_rad) + 1 / cos(lat_rad)) / pi) / 2 * n))
  list(x = x, y = y)
}

mg_tile_bbox <- function(x, y, zoom) {
  n <- 2^zoom
  lon1 <- x / n * 360 - 180
  lon2 <- (x + 1) / n * 360 - 180
  merc <- function(ty) {
    atan(sinh(pi * (1 - 2 * ty / n))) * 180 / pi
  }
  c(xmin = lon1, xmax = lon2, ymin = merc(y + 1), ymax = merc(y))
}

mg_write_terrarium_png <- function(tile_r, path, size = 256L) {
  if (terra::nrow(tile_r) != size || terra::ncol(tile_r) != size) {
    tmpl <- terra::rast(
      terra::ext(tile_r),
      ncols = size, nrows = size, crs = terra::crs(tile_r)
    )
    tile_r <- terra::resample(tile_r, tmpl, method = "bilinear")
  }
  v <- terra::as.matrix(tile_r, wide = TRUE)
  if (nrow(v) != size || ncol(v) != size) {
    stop("DEM tile is ", nrow(v), "x", ncol(v), ", expected ", size, "x", size)
  }
  val <- v + 32768
  val[!is.finite(val)] <- 32768
  val <- pmin(pmax(val, 0), 65535.996)
  rr <- floor(val / 256)
  gg <- floor(val - rr * 256)
  bb <- floor((val - floor(val)) * 256)
  arr <- array(0, dim = c(size, size, 3))
  arr[, , 1] <- rr / 255
  arr[, , 2] <- gg / 255
  arr[, , 3] <- bb / 255
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  png::writePNG(arr, path)
}

mg_existing_tiles <- function(out_dir, mbtiles = NULL) {
  json_path <- file.path(out_dir, "terrain_tiles.json")
  pngs <- list.files(out_dir, pattern = "\\.png$", recursive = TRUE)
  if (!file.exists(json_path) || !length(pngs)) {
    return(NULL)
  }
  tj <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  mb_path <- NULL
  if (!is.null(mbtiles) && file.exists(mbtiles)) {
    mb_path <- normalizePath(mbtiles, winslash = "/", mustWork = FALSE)
  }
  list(
    dir = normalizePath(out_dir, winslash = "/", mustWork = TRUE),
    json = normalizePath(json_path, winslash = "/", mustWork = TRUE),
    mbtiles = mb_path,
    bounds = unlist(tj$bounds),
    min_zoom = as.integer(tj$minzoom),
    max_zoom = as.integer(tj$maxzoom),
    n_tiles = length(pngs)
  )
}

mg_dem_to_mbtiles <- function(dem, out_dir = "terrain_tiles",
                              mbtiles = "terrain.mbtiles",
                              min_zoom = 10L, max_zoom = 15L) {
  mg_need("terra")
  mg_need("sf")
  mg_need("png")
  mg_need("jsonlite")
  out_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dem4326 <- terra::project(dem, "EPSG:4326")
  e <- terra::ext(dem4326)
  bounds <- c(
    as.numeric(e[1]), as.numeric(e[3]),
    as.numeric(e[2]), as.numeric(e[4])
  )
  tiles_written <- 0L
  for (z in min_zoom:max_zoom) {
    sw <- mg_lonlat_tile(bounds[1], bounds[2], z)
    ne <- mg_lonlat_tile(bounds[3], bounds[4], z)
    xs <- seq.int(max(0L, sw$x), min(2^z - 1L, ne$x))
    ys <- seq.int(max(0L, ne$y), min(2^z - 1L, sw$y))
    message("Terrarium tiles z", z, ": ", length(xs) * length(ys))
    for (x in xs) {
      for (y in ys) {
        bb <- mg_tile_bbox(x, y, z)
        tmpl <- terra::rast(
          xmin = bb[["xmin"]], xmax = bb[["xmax"]],
          ymin = bb[["ymin"]], ymax = bb[["ymax"]],
          ncols = 256, nrows = 256, crs = "EPSG:4326"
        )
        one <- tryCatch(
          terra::resample(dem4326, tmpl, method = "bilinear"),
          error = function(e) NULL
        )
        if (is.null(one)) {
          next
        }
        png_path <- file.path(out_dir, as.character(z), as.character(x), paste0(y, ".png"))
        mg_write_terrarium_png(one, png_path)
        tiles_written <- tiles_written + 1L
      }
    }
  }
  tilejson <- list(
    tilejson = "3.0.0",
    name = "windfarmGA terrain",
    tiles = list("http://127.0.0.1:8000/tiles/{z}/{x}/{y}.png"),
    bounds = bounds,
    minzoom = min_zoom,
    maxzoom = max_zoom,
    scheme = "xyz",
    encoding = "terrarium"
  )
  json_path <- file.path(out_dir, "terrain_tiles.json")
  jsonlite::write_json(tilejson, json_path, auto_unbox = TRUE, pretty = TRUE)
  mb_path <- NULL
  if (!is.null(mbtiles) && requireNamespace("RSQLite", quietly = TRUE)) {
    mb_path <- mg_pack_mbtiles(out_dir, mbtiles, tilejson)
  } else if (!is.null(mbtiles)) {
    message("RSQLite not installed; skipped ", mbtiles)
  }
  list(
    dir = out_dir,
    json = json_path,
    mbtiles = mb_path,
    bounds = bounds,
    min_zoom = min_zoom,
    max_zoom = max_zoom,
    n_tiles = tiles_written
  )
}

mg_pack_mbtiles <- function(tile_dir, mbtiles, tilejson) {
  mg_need("RSQLite")
  mbtiles <- normalizePath(mbtiles, winslash = "/", mustWork = FALSE)
  if (file.exists(mbtiles)) {
    unlink(mbtiles)
  }
  con <- RSQLite::dbConnect(RSQLite::SQLite(), mbtiles)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  RSQLite::dbExecute(con, "CREATE TABLE metadata (name TEXT, value TEXT);")
  RSQLite::dbExecute(
    con,
    "CREATE TABLE tiles (zoom_level INTEGER, tile_column INTEGER, tile_row INTEGER, tile_data BLOB);"
  )
  meta <- list(
    name = "windfarmGA terrain",
    format = "png",
    type = "overlay",
    bounds = paste(tilejson$bounds, collapse = ","),
    minzoom = as.character(tilejson$minzoom),
    maxzoom = as.character(tilejson$maxzoom),
    scheme = "tms",
    encoding = "terrarium"
  )
  for (nm in names(meta)) {
    RSQLite::dbExecute(
      con, "INSERT INTO metadata (name, value) VALUES (?, ?);",
      params = list(nm, as.character(meta[[nm]]))
    )
  }
  pngs <- list.files(tile_dir, pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
  for (p in pngs) {
    parts <- strsplit(gsub("\\\\", "/", p), "/")[[1]]
    n <- length(parts)
    z <- as.integer(parts[n - 2L])
    x <- as.integer(parts[n - 1L])
    y_xyz <- as.integer(sub("\\.png$", "", parts[n]))
    y_tms <- as.integer(2^z - 1L - y_xyz)
    blob <- readBin(p, "raw", file.info(p)$size)
    RSQLite::dbExecute(
      con,
      "INSERT INTO tiles (zoom_level, tile_column, tile_row, tile_data) VALUES (?, ?, ?, ?);",
      params = list(z, x, y_tms, I(list(blob)))
    )
  }
  message("Wrote ", mbtiles, " (", length(pngs), " tiles).")
  mbtiles
}

mg_stop_tile_server <- function() {
  if (exists(".mg_tile_server", envir = .GlobalEnv, inherits = FALSE)) {
    try(httpuv::stopServer(get(".mg_tile_server", envir = .GlobalEnv)), silent = TRUE)
    rm(".mg_tile_server", envir = .GlobalEnv)
  }
}

mg_tile_maxzoom <- function(tile_dir, fallback = 15L) {
  zs <- list.dirs(tile_dir, recursive = FALSE, full.names = FALSE)
  zs <- suppressWarnings(as.integer(zs))
  zs <- zs[is.finite(zs)]
  if (!length(zs)) fallback else as.integer(max(zs))
}

mg_write_tilejson <- function(tile_dir, port, bounds = NULL,
                              min_zoom = 10L, max_zoom = 15L) {
  json_path <- file.path(tile_dir, "terrain_tiles.json")
  old <- if (file.exists(json_path)) {
    tryCatch(jsonlite::fromJSON(json_path, simplifyVector = FALSE), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.null(bounds) && !is.null(old$bounds)) {
    bounds <- unlist(old$bounds)
  }
  if (is.null(min_zoom) && !is.null(old$minzoom)) {
    min_zoom <- as.integer(old$minzoom)
  }
  tilejson <- list(
    tilejson = "3.0.0",
    name = "windfarmGA terrain",
    tiles = list(sprintf("http://127.0.0.1:%s/tiles/{z}/{x}/{y}.png", port)),
    bounds = bounds,
    minzoom = as.integer(min_zoom),
    maxzoom = as.integer(max_zoom),
    scheme = "xyz",
    encoding = "terrarium"
  )
  jsonlite::write_json(tilejson, json_path, auto_unbox = TRUE, pretty = TRUE)
  json_path
}

mg_serve_tiles <- function(tile_dir, port = 8000) {
  mg_need("httpuv")
  tile_dir <- normalizePath(tile_dir, winslash = "/", mustWork = TRUE)
  json_path <- mg_write_tilejson(
    tile_dir, port, max_zoom = mg_tile_maxzoom(tile_dir)
  )
  mg_stop_tile_server()
  start_one <- function() {
    httpuv::startServer("127.0.0.1", port, list(
    call = function(req) {
      path <- sub("^/", "", req$PATH_INFO)
      path <- sub("\\?.*$", "", path)
      if (!nzchar(path) || path == "terrain_tiles.json") {
        f <- json_path
      } else {
        rel <- sub("^tiles/", "", path)
        rel <- sub("^terrain_tiles/", "", rel)
        f <- file.path(tile_dir, rel)
      }
      f <- normalizePath(f, winslash = "/", mustWork = FALSE)
      ok <- file.exists(f) && startsWith(tolower(f), tolower(tile_dir))
      if (!ok) {
        return(list(
          status = 404L,
          headers = list("Access-Control-Allow-Origin" = "*"),
          body = "not found"
        ))
      }
      ct <- if (grepl("\\.json$", f, ignore.case = TRUE)) {
        "application/json"
      } else if (grepl("\\.obj$", f, ignore.case = TRUE)) {
        "text/plain"
      } else if (grepl("\\.js$", f, ignore.case = TRUE)) {
        "application/javascript"
      } else {
        "image/png"
      }
      list(
        status = 200L,
        headers = list(
          "Access-Control-Allow-Origin" = "*",
          "Content-Type" = ct,
          "Cache-Control" = "no-store"
        ),
        body = readBin(f, "raw", file.info(f)$size)
      )
    }
  ))
  }
  h <- tryCatch(start_one(), error = function(e) {
    if (!grepl("already in use|Failed to create server", conditionMessage(e))) {
      stop(e)
    }
    message("Port ", port, " busy; stopping leftover httpuv servers and retrying.")
    httpuv::stopAllServers()
    start_one()
  })
  assign(".mg_tile_server", h, envir = .GlobalEnv)
  sprintf(
    "http://127.0.0.1:%s/terrain_tiles.json?t=%s",
    port, as.integer(Sys.time())
  )
}

mg_layout_ll <- function(result, area, which = NULL) {
  if (is.null(which)) {
    which <- nrow(result)
  }
  layout <- as.data.frame(result[which, "bestPaEn"][[1]])
  crs <- sf::st_crs(windfarmGA::isSpatial(area))
  pts <- sf::st_as_sf(layout, coords = c("X", "Y"), crs = crs)
  pts <- sf::st_transform(pts, 4326)
  xy <- sf::st_coordinates(pts)
  layout$lon <- xy[, 1]
  layout$lat <- xy[, 2]
  layout
}

mg_result_meta <- function(result) {
  inp <- result[1, "inputData"][[1]]
  if (is.list(inp) && !is.null(inp$Input_Data)) {
    inp <- inp$Input_Data
  }
  hub <- suppressWarnings(as.numeric(inp["Rotor Height", 1]))
  rotor <- suppressWarnings(as.numeric(inp["Rotorradius", 1]))
  list(
    hub = if (is.finite(hub) && hub > 0) hub else 100,
    rotor = if (is.finite(rotor) && rotor > 0) rotor else 50,
    wind = result[1, "inputWind"][[1]]
  )
}

mg_wind_to <- function(wind) {
  tab <- windfarmGA:::leaflet_wind_for_cones(wind)
  if (is.null(tab) || !nrow(tab)) {
    return(0)
  }
  (as.numeric(tab$wd[which.max(tab$prob)]) + 180) %% 360
}

mg_default_obj <- function() {
  p <- file.path(.mg_script_dir, "wind_turbine_v1.obj")
  if (file.exists(p)) p else NULL
}

mg_ensure_three <- function(dest_dir) {
  dest <- file.path(dest_dir, "three.min.js")
  if (file.exists(dest) && isTRUE(file.info(dest)$size > 1e5)) {
    return(TRUE)
  }
  urls <- c(
    "https://cdn.jsdelivr.net/npm/three@0.149.0/build/three.min.js",
    "https://unpkg.com/three@0.149.0/build/three.min.js"
  )
  for (u in urls) {
    ok <- tryCatch({
      utils::download.file(u, dest, mode = "wb", quiet = TRUE)
      file.exists(dest) && isTRUE(file.info(dest)$size > 1e5)
    }, error = function(e) FALSE)
    if (isTRUE(ok)) {
      message("Downloaded Three.js r149 (UMD) to ", dest)
      return(TRUE)
    }
  }
  warning("Could not download three.min.js; OBJ layer skipped.")
  FALSE
}

mg_elev_ll <- function(pts, result) {
  tm <- tryCatch(windfarmGA:::ga_result_terrain(result), error = function(e) NULL)
  dem <- if (!is.null(tm) && !is.null(tm$srtm_crop)) tm$srtm_crop[[1]] else NULL
  if (is.null(dem) || !inherits(dem, "SpatRaster")) {
    return(rep(NA_real_, nrow(pts)))
  }
  v <- terra::vect(sf::st_transform(pts, terra::crs(dem)))
  as.numeric(terra::extract(dem, v, ID = FALSE)[[1]])
}

mg_add_obj_turbines <- function(map, pts, obj_url, hub, yaw_deg,
                                elev, exaggeration, three_url = NULL) {
  mg_need("htmlwidgets")
  js <- file.path(.mg_script_dir, "plot_mapgl_turbines.js")
  if (!file.exists(js)) {
    warning("Missing ", js, "; OBJ turbines skipped.")
    return(map)
  }
  message("plot_mapgl turbines hook v12 (OBJ only, no placeholder towers)")
  htmlwidgets::onRender(
    map,
    paste(readLines(js, warn = FALSE), collapse = "\n"),
    list(
      obj_url = obj_url,
      three_url = three_url,
      hub = hub,
      yaw_rad = as.numeric(yaw_deg) * pi / 180,
      exaggeration = as.numeric(exaggeration),
      turbines = lapply(seq_len(nrow(pts)), function(i) {
        list(
          lng = pts$lon[i],
          lat = pts$lat[i],
          elev = if (is.finite(elev[i])) elev[i] else NULL
        )
      })
    )
  )
}

mg_wake_cols <- function(wake, n) {
  wake <- as.numeric(wake)
  if (length(wake) != n) {
    wake <- rep(wake, length.out = n)
  }
  mx <- max(c(0, wake), na.rm = TRUE)
  if (!is.finite(mx) || mx < 1e-9) {
    return(rep("#27ae60", n))
  }
  u <- pmin(pmax(wake / mx, 0), 1)
  u[!is.finite(u)] <- 0
  grDevices::rgb(
    grDevices::colorRamp(c("#27ae60", "#f1c40f", "#c0392b"))(u),
    maxColorValue = 255
  )
}

mg_buffer_m <- function(pts, dist_m, crs_m) {
  keep <- intersect(c("popup", "lab", "wake"), names(pts))
  out <- sf::st_buffer(sf::st_transform(pts[, keep], crs_m), dist = dist_m)
  sf::st_transform(out, 4326)
}

mg_plain_pts <- function(layout) {
  wake <- if ("AbschGesamt" %in% names(layout)) {
    as.numeric(layout$AbschGesamt)
  } else {
    rep(NA_real_, nrow(layout))
  }
  energy <- if ("EnergyOverall" %in% names(layout)) {
    as.numeric(unlist(layout$EnergyOverall))
  } else {
    NA_real_
  }
  energy <- rep(energy, length.out = nrow(layout))
  sf::st_as_sf(
    data.frame(
      lon = as.numeric(layout$lon),
      lat = as.numeric(layout$lat),
      wake = wake,
      popup = sprintf(
        "Wake %s%%<br>Energy %s kW",
        ifelse(is.finite(wake), as.character(round(wake, 1)), "—"),
        ifelse(is.finite(energy), as.character(round(energy, 0)), "—")
      ),
      lab = as.character(seq_len(nrow(layout))),
      stringsAsFactors = FALSE
    ),
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
}

mg_wake_cones <- function(layout, area, wind, rotor, cols) {
  wind_tab <- windfarmGA:::leaflet_wind_for_cones(wind)
  if (is.null(wind_tab) || !nrow(wind_tab) || !nrow(layout)) {
    return(NULL)
  }
  xy <- as.matrix(layout[, c("X", "Y")])
  half <- getOption("windfarmGA.max_angle", 20)
  len <- windfarmGA:::leaflet_wake_length(area, rotor)
  cones <- windfarmGA:::wake_cone_polys(
    xy, wind_tab, half, len, sf::st_crs(area), cols
  )
  sf::st_transform(cones, 4326)
}

mg_carto_url <- function(basemap) {
  switch(
    basemap,
    positron = mapgl::carto_style("positron"),
    dark = mapgl::carto_style("dark-matter"),
    satellite =, topo = mapgl::carto_style("voyager-no-labels"),
    mapgl::carto_style("voyager")
  )
}

mg_add_basemap_rasters <- function(map, satellite_on = FALSE, topo_on = FALSE) {
  map <- mapgl::add_raster_source(
    map,
    id = "src-satellite",
    tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
    tileSize = 256,
    maxzoom = 19
  )
  map <- mapgl::add_raster_layer(
    map,
    id = "basemap-satellite",
    source = "src-satellite",
    visibility = if (isTRUE(satellite_on)) "visible" else "none"
  )
  map <- mapgl::add_raster_source(
    map,
    id = "src-topo",
    tiles = "https://tile.opentopomap.org/{z}/{x}/{y}.png",
    tileSize = 256,
    maxzoom = 17
  )
  mapgl::add_raster_layer(
    map,
    id = "basemap-topo",
    source = "src-topo",
    visibility = if (isTRUE(topo_on)) "visible" else "none"
  )
}

#' 3D mapgl map of a GA result on a published Terrarium DEM.
#'
#' `basemap` is the starting view: `"voyager"` (default), `"positron"`,
#' `"dark"`, `"satellite"` (Esri imagery), or `"topo"` (OpenTopoMap).
#' The layers control can still toggle Satellite / Topo on top.
plot_mapgl_from_result <- function(result, area, which = NULL,
                                   dem = NULL, buffer = 8000, z = 12,
                                   min_zoom = 10L, max_zoom = 15L,
                                   exaggeration = 1.2,
                                   out_dir = "terrain_tiles",
                                   mbtiles = "terrain.mbtiles",
                                   port = 8000, serve = TRUE,
                                   rebuild = FALSE,
                                   basemap = "voyager",
                                   turbine_obj = TRUE) {
  mg_need("mapgl")
  mg_need("jsonlite")
  area <- windfarmGA::isSpatial(area)
  built <- if (isTRUE(rebuild)) NULL else mg_existing_tiles(out_dir, mbtiles)
  if (is.null(built)) {
    dem <- mg_prepare_dem(area, dem = dem, result = result, buffer = buffer, z = z)
    built <- mg_dem_to_mbtiles(
      dem, out_dir = out_dir, mbtiles = mbtiles,
      min_zoom = min_zoom, max_zoom = max_zoom
    )
  } else {
    message("Reusing ", built$n_tiles, " tiles in ", built$dir,
            " (rebuild = TRUE to remake).")
  }
  if (isTRUE(turbine_obj)) {
    turbine_obj <- mg_default_obj()
  }
  if (!is.null(turbine_obj) && !file.exists(turbine_obj)) {
    warning("turbine_obj not found: ", turbine_obj)
    turbine_obj <- NULL
  }
  if (!is.null(turbine_obj)) {
    file.copy(
      turbine_obj, file.path(built$dir, "wind_turbine.obj"),
      overwrite = TRUE
    )
  }
  three_ok <- isTRUE(serve) && isTRUE(mg_ensure_three(built$dir))
  tiles_url <- if (isTRUE(serve)) {
    mg_serve_tiles(built$dir, port = port)
  } else {
    normalizePath(built$json, winslash = "/")
  }
  obj_url <- if (!is.null(turbine_obj) && isTRUE(serve)) {
    sprintf("http://127.0.0.1:%s/wind_turbine.obj", port)
  } else {
    NULL
  }
  three_url <- if (isTRUE(three_ok)) {
    sprintf("http://127.0.0.1:%s/three.min.js", port)
  } else {
    NULL
  }
  meta <- mg_result_meta(result)
  layout <- mg_layout_ll(result, area, which)
  pts <- mg_plain_pts(layout)
  n <- nrow(pts)
  wake <- pts$wake
  cols <- mg_wake_cols(wake, n)
  site <- sf::st_transform(sf::st_zm(sf::st_geometry(area)), 4326)
  site <- sf::st_sf(geometry = site)
  cones <- mg_wake_cones(layout, area, meta$wind, meta$rotor, cols)
  if (!is.null(cones) && nrow(cones)) {
    if ("turb" %in% names(cones)) {
      cones$wake <- as.numeric(wake)[as.integer(cones$turb)]
    }
    cones$popup <- sprintf(
      "Wake %s%%",
      ifelse(is.finite(cones$wake), as.character(round(cones$wake, 1)), "—")
    )
    keep <- intersect(c("wake", "prob", "farbe", "turb", "popup"), names(cones))
    cones <- cones[, keep, drop = FALSE]
    message("Wake cones: ", nrow(cones), " polygons, ", n, " turbines.")
  } else {
    message("No wake cones (result$inputWind missing or empty). ", n, " turbines.")
  }
  cen <- as.numeric(sf::st_coordinates(sf::st_centroid(sf::st_union(site)))[1, ])
  map <- mapgl::maplibre(
    style = mg_carto_url(basemap),
    center = cen,
    zoom = 13,
    pitch = 65,
    bearing = 0,
    projection = "mercator",
    maxPitch = 85
  )
  map <- mapgl::add_navigation_control(map, visualize_pitch = TRUE)
  map <- mapgl::add_fullscreen_control(map)
  map <- mapgl::add_raster_dem_source(
    map,
    id = "windfarm-dem",
    url = tiles_url,
    tileSize = 256,
    maxzoom = mg_tile_maxzoom(built$dir)
  )
  map <- mapgl::set_terrain(map, source = "windfarm-dem", exaggeration = exaggeration)
  map <- mg_add_basemap_rasters(
    map,
    satellite_on = identical(basemap, "satellite"),
    topo_on = identical(basemap, "topo")
  )
  map <- mapgl::add_fill_layer(
    map,
    id = "site",
    source = site,
    fill_color = "#2980B9",
    fill_opacity = 0.12
  )
  map <- mapgl::add_line_layer(
    map,
    id = "site-outline",
    source = site,
    line_color = "#f4d03f",
    line_width = 2.2,
    line_opacity = 0.95
  )
  overlay <- list(
    "Satellite" = "basemap-satellite",
    "Topo" = "basemap-topo",
    "Site" = c("site-outline", "site")
  )
  if (!is.null(cones) && nrow(cones) && "wake" %in% names(cones)) {
    wake_max <- max(c(0, cones$wake), na.rm = TRUE)
    if (!is.finite(wake_max) || wake_max < 1e-9) {
      wake_max <- 1
    }
    wake_pal <- mapgl::interpolate(
      column = "wake",
      values = c(0, wake_max / 2, wake_max),
      stops = c("#27ae60", "#f1c40f", "#c0392b")
    )
    map <- mapgl::add_fill_layer(
      map,
      id = "wake-cones",
      source = cones,
      fill_color = wake_pal,
      fill_opacity = 0.32,
      popup = "popup"
    )
    map <- mapgl::add_line_layer(
      map,
      id = "wake-outlines",
      source = cones,
      line_color = wake_pal,
      line_width = 1.6,
      line_opacity = 0.8
    )
    overlay[["Wake cones"]] <- c("wake-outlines", "wake-cones")
  }
  map <- mapgl::add_symbol_layer(
    map,
    id = "turbine-labels",
    source = pts,
    text_field = mapgl::get_column("lab"),
    text_size = 14,
    text_color = "#ffffff",
    text_halo_color = "#1a1a1a",
    text_halo_width = 1.4,
    text_allow_overlap = TRUE,
    popup = "popup"
  )
  if (!is.null(obj_url) && !is.null(three_url)) {
    map <- mg_add_obj_turbines(
      map, pts, obj_url, meta$hub, mg_wind_to(meta$wind),
      elev = mg_elev_ll(pts, result),
      exaggeration = exaggeration,
      three_url = three_url
    )
    message("OBJ turbines from ", turbine_obj, " (hub ", round(meta$hub), " m).")
  }
  overlay[["Turbines"]] <- "turbine-labels"
  map <- mapgl::add_layers_control(
    map,
    position = "top-left",
    layers = overlay,
    collapsible = TRUE
  )
  if (any(is.finite(wake))) {
    map <- mapgl::add_continuous_legend(
      map,
      legend_title = "Wake %",
      values = c(0, max(c(0, wake), na.rm = TRUE)),
      colors = c("#27ae60", "#f1c40f", "#c0392b"),
      position = "bottom-left"
    )
  }
  map <- mapgl::fit_bounds(map, site, animate = FALSE)
  attr(map, "terrain") <- built
  attr(map, "tiles_url") <- tiles_url
  map
}

message("loaded experimental/plot_mapgl.R  (v12: OBJ only, no placeholder towers)")
