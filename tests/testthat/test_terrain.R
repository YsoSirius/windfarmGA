suppressWarnings({
  library(terra)
  library(elevatr)
  library(raster)
})

synth_ccl_path <- function(area, code = 12) {
  path <- tempfile(fileext = ".tif")
  bb <- sf::st_bbox(area)
  r <- terra::rast(
    xmin = as.numeric(bb$xmin), xmax = as.numeric(bb$xmax),
    ymin = as.numeric(bb$ymin), ymax = as.numeric(bb$ymax),
    resolution = 100, crs = terra::crs(area)
  )
  terra::values(r) <- code
  terra::writeRaster(r, path, overwrite = TRUE)
  path
}

test_that("Test Terrain and Weibull Effects", {
  # skip()
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  ## Function to suppress print/cat outputs
  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }

  ## Test Terrain_Model Function ###############
  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4651704, 4651704, 4654475, 4654475, 4651704),
      c(2692925, 2694746, 2694746, 2692925, 2692925)
    ))),
    crs = 3035
  ))
  ccl_file <- synth_ccl_path(area)
  polygon_wgs84 <- sf::st_transform(area, st_crs(4326))
  srtm <- suppressMessages(elevatr::get_elev_raster(locations = polygon_wgs84, z = 11))
  res <- terrain_model(srtm, area, ccl = terra::rast(ccl_file))
  expect_length(res, 2)
  expect_length(res[[1]], 3)
  expect_length(res[[2]], 1)
  expect_s4_class(res[[2]], "SpatRaster")
  expect_s4_class(res[[1]][[1]], "SpatRaster")
  expect_s4_class(res[[1]][[2]], "SpatRaster")
  expect_s4_class(res[[1]][[3]], "SpatRaster")

  res <- terrain_model(terra::rast(srtm), area, ccl = ccl_file)
  expect_length(res, 2)
  expect_length(res[[1]], 3)
  expect_length(res[[2]], 1)
  expect_s4_class(res[[2]], "SpatRaster")
  expect_s4_class(res[[1]][[1]], "SpatRaster")
  expect_s4_class(res[[1]][[2]], "SpatRaster")
  expect_s4_class(res[[1]][[3]], "SpatRaster")


  srtm_terra <- terra::rast(srtm)
  values(srtm_terra) <- NA
  res <- expect_warning(terrain_model(srtm_terra, area, ccl = ccl_file))
  res <- suppressWarnings(terrain_model(srtm_terra, area, ccl = ccl_file))
  expect_length(res, 2)
  expect_length(res[[1]], 3)
  expect_length(res[[2]], 1)
  expect_s4_class(res[[2]], "SpatRaster")
  expect_s4_class(res[[1]][[1]], "SpatRaster")
  expect_s4_class(res[[1]][[2]], "SpatRaster")
  expect_s4_class(res[[1]][[3]], "SpatRaster")

  ## Get DEM Fail (too big) ##############
  polygon <- structure(list(structure(
    list(structure(c(2627705.84970268, 3015019.42206679,
                     5354856.02640269, 5660619.11584955, 2627705.84970268,
                     1916658.62646189, 3437763.9404084, 3370878.53767667,
                     1834071.85234423, 1916658.62646189
    ), dim = c(5L, 2L))), class = c("XY", "POLYGON", "sfg"))),
    n_empty = 0L,
    crs = structure(list(input = NA_character_, wkt = NA_character_),
                    class = "crs"), class = c("sfc_POLYGON", "sfc"),
    precision = 0, bbox = structure(
      c(xmin = 2627705.84970268, ymin = 1834071.85234423,
        xmax = 5660619.11584955, ymax = 3437763.9404084), class = "bbox"))
  st_crs(polygon) <- 3035
  expect_error(
    terrain_model(terrain = TRUE, polygon, ccl = ccl_file)
  )

  ## Mock Packages not installed ############
  with_mocked_bindings(
    is_elevatr_installed = function() FALSE,
    expect_error(
      terrain_model(terrain = TRUE, area, ccl = ccl_file)
    )
  )

  ## Test GA with Terrain Model ###################
  Projection <- 3035
  vdata <- data.frame(ws = 12, wd = 0)

  ## Normal Terrain Example
  sp_polygon <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  ccl_sp <- synth_ccl_path(sp_polygon)

  resultrect <- quiet(suppressWarnings(
    genetic_algorithm(
      area = sp_polygon,
      n = 12, iteration = 1,
      wind = vdata,
      rotor = 30,
      rotor_height = 100,
      terrain = TRUE, verbose = TRUE,
      plot = TRUE
    )
  ))
  expect_true(nrow(resultrect) == 1)
  expect_true(is.matrix(resultrect))
  expect_false(any(unlist(sapply(resultrect, is.na))))

  ## CCL-Raster should be in directory already
  path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
  ccl_roughness <- paste0(path, "clc_legend.csv")
  resultrect <- quiet(suppressWarnings(
    genetic_algorithm(
      area = sp_polygon,
      n = 12, iteration = 1,
      wind = vdata,
      rotor = 30,
      rotor_height = 100,
      terrain = TRUE, verbose = TRUE,
      plot = TRUE, ccl = ccl_sp,
      ccl_roughness = ccl_roughness
    )
  ))
  expect_true(nrow(resultrect) == 1)
  expect_true(is.matrix(resultrect))
  expect_false(any(unlist(sapply(resultrect, is.na))))


  ## Weibull ################
  ## Weibull Params (FAKE).
  DEM <- suppressWarnings(elevatr::get_elev_raster(
    verbose = FALSE,
    locations = st_transform(sp_polygon, 4326), z = 11
  ))
  sp_polygonproj <- st_transform(sp_polygon, st_crs(DEM))
  DEM <- terra::rast(DEM)
  DEMcrop <- crop(DEM, sp_polygonproj, mask = TRUE)
  maxval <- max(values(DEMcrop), na.rm = TRUE)
  a_raster <- terra::app(DEMcrop, function(x) (x / maxval) + 1)
  k_raster <- terra::app(DEMcrop, function(x) (x / maxval) + 6)

  resultrect <- quiet(suppressWarnings(
    genetic_algorithm(
      area = sp_polygon,
      n = 12, iteration = 1,
      wind = vdata,
      rotor = 30,
      rotor_height = 100,
      verbose = TRUE,
      weibull = TRUE,
      weibull_src = list(k_raster, a_raster)
    )
  ))
  expect_true(nrow(resultrect) == 1)
  expect_true(is.matrix(resultrect))
  expect_false(any(unlist(sapply(resultrect, is.na))))

  rm(resultrect)
  resultrect <- quiet(suppressWarnings(
    genetic_algorithm(
      area = sp_polygon,
      n = 12, iteration = 1,
      wind = vdata,
      rotor = 30,
      rotor_height = 100,
      weibull = TRUE,
      weibull_src = list(raster::raster(k_raster), a_raster)
    )
  ))
  expect_true(nrow(resultrect) == 1)
  expect_true(is.matrix(resultrect))
  expect_false(any(unlist(sapply(resultrect, is.na))))

  rm(resultrect)
  resultrect <- quiet(suppressWarnings(
    genetic_algorithm(
      area = sp_polygon,
      n = 12, iteration = 1,
      wind = vdata,
      rotor = 30,
      rotor_height = 100,
      weibull = TRUE,
      weibull_src = list(k_raster, raster::raster(a_raster))
    )
  ))
  expect_true(nrow(resultrect) == 1)
  expect_true(is.matrix(resultrect))
  expect_false(any(unlist(sapply(resultrect, is.na))))

  expect_error(
    genetic_algorithm(
      area = sp_polygon,
      n = 12, iteration = 1, wind = vdata,
      rotor = 30, rotor_height = 100,
      weibull = TRUE
    )
  )

  ## Plotting Terrain Effects #############
  plres <- suppressWarnings(
    plot_result(resultrect, sp_polygon,
      terrain = TRUE,
      plot_en = 1,
      ccl_roughness = ccl_roughness,
      weibull_src = list(a_raster * (gamma(1 + (1 / values(k_raster)))))
    )
  )
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))

  plres <- suppressWarnings(
    plot_result(resultrect, sp_polygon,
      terrain = TRUE,
      plot_en = 1,
      ccl_roughness = ccl_roughness,
      weibull_src = list(raster::raster(a_raster * (gamma(1 + (1 / values(k_raster))))))
    )
  )
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))

  plres <- suppressWarnings(
    plot_result(resultrect, sp_polygon,
      terrain = TRUE,
      plot_en = 1,
      ccl_roughness = ccl_roughness,
      weibull_src = raster::raster(a_raster * (gamma(1 + (1 / values(k_raster)))))
    )
  )
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))

  plres <- plot_result(resultrect, sp_polygon,
    weibull_src = list(k_raster, a_raster)
  )
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))

  plres <- plot_result(resultrect, sp_polygon,
    weibull_src = list(
      raster::raster(k_raster),
      raster::raster(a_raster)
    )
  )
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))

  ## Weibull Single Raster for mean wind spead
  weibullraster <- a_raster * (gamma(1 + (1 / values(k_raster))))
  plres <- plot_result(resultrect, sp_polygon,
    plot_en = 2,
    weibull_src = weibullraster
  )
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))

  plres <- plot_result(resultrect,
    sp_polygon,
    terrain = TRUE,
    plot_en = 1,
    ccl = ccl_sp
  )
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))

  ## calculate_energy with Terrain + Plots!! ##################
  ## With Terrain (+new function)
  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  srtm <- suppressWarnings(
    elevatr::get_elev_raster(
      locations = area, z = 11
    )
  )
  srtm_crop <- terra::crop(terra::rast(srtm), area)

  vdata <- data.frame(ws = 12, wd = 0)
  Rotor <- 50
  fcr <- 3
  resGrid <- grid_area(
    area = area, size = Rotor * fcr,
    prop = 1, plot_grid = FALSE
  )
  resStartGA <- init_population(grid = resGrid[[1]], n = 15, n_start = 100)

  srtm_crop <- terra::mask(srtm_crop, area)
  roughrast <- terra::terrain(srtm_crop, "roughness")
  if (all(is.na(values(roughrast)))) {
    values(roughrast) <- 1
  }
  srtm_crop <- list(
    strm_crop = srtm_crop,
    orogr1 = srtm_crop / as.numeric(terra::global(srtm_crop, fun = "mean", na.rm = TRUE)),
    roughness = roughrast
  )

  ccl <- terra::rast(ccl_sp)
  ccl <- crop(ccl, area, mask = TRUE)
  path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
  ccl_roughness <- paste0(path, "clc_legend.csv")
  rauhigkeitz <- utils::read.csv(ccl_roughness,
    header = TRUE, sep = ";"
  )
  cclRaster <- terra::classify(ccl, matrix(c(
    rauhigkeitz$GRID_CODE,
    rauhigkeitz$Rauhigkeit_z
  ),
  ncol = 2
  ))
  resCalcEn <- calculate_energy(
    layout = resStartGA[[1]], reference_height = 50,
    elevation = srtm_crop, ccl_raster = cclRaster,
    rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
    wake_distance = 100000, wind = vdata,
    rotor = 50, area = area,
    terrain = TRUE, weibull = FALSE, plot = TRUE
  )
  expect_output(str(resCalcEn), "List of 1")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] <
    df[df[, "TotAbschProz"] != 0, "Windmean"]))
  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))

  resultrect <- quiet(suppressWarnings(
    genetic_algorithm(
      area = area,
      n = 12, iteration = 1,
      wind = vdata,
      rotor = 30,
      rotor_height = 100,
      terrain = srtm_crop$strm_crop, verbose = TRUE,
      plot = TRUE, ccl = ccl_sp,
      ccl_roughness = ccl_roughness
    )
  ))
  expect_true(nrow(resultrect) == 1)
  expect_true(is.matrix(resultrect))
  expect_false(any(unlist(sapply(resultrect, is.na))))

  ## Weibull + Plotting ##############
  DEMcrop <- srtm_crop$orogr1
  maxval <- max(values(DEMcrop))
  a_raster <- terra::app(DEMcrop, function(x) (x / maxval) + 1)
  k_raster <- terra::app(DEMcrop, function(x) (x / maxval) + 6)
  weibullraster <- a_raster * (gamma(1 + (1 / values(k_raster))))
  resCalcEn <- calculate_energy(
    layout = resStartGA[[1]], reference_height = 50,
    elevation = srtm_crop, ccl_raster = cclRaster,
    rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
    wake_distance = 100000, wind = vdata,
    rotor = 50, area = area, terrain = FALSE,
    weibull = weibullraster, plot = TRUE
  )
  expect_output(str(resCalcEn), "List of 1")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] <
    df[df[, "TotAbschProz"] != 0, "Windmean"]))
  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))


  resCalcEn <- calculate_energy(
    layout = resStartGA[[1]], reference_height = 50,
    elevation = srtm_crop, ccl_raster = cclRaster,
    rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
    wake_distance = 100000, wind = vdata,
    rotor = 50, area = area, terrain = FALSE,
    weibull = raster::raster(weibullraster), plot = TRUE
  )
  expect_output(str(resCalcEn), "List of 1")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] <
    df[df[, "TotAbschProz"] != 0, "Windmean"]))
  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))


  ## Make Hole in Weibull-Raster, so some Values are NA
  min_y_ppt <- data.frame(resStartGA[[1]][order(resStartGA[[1]][, 2]) <= 9, ])
  min_y_ppt <- st_as_sf(min_y_ppt, coords = c("X", "Y"))
  weibullrastercrop <- crop(weibullraster, terra::ext(min_y_ppt))
  resCalcEn <- calculate_energy(
    layout = resStartGA[[1]], reference_height = 50,
    elevation = srtm_crop, ccl_raster = cclRaster,
    rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
    wake_distance = 100000, wind = vdata,
    rotor = 50, area = area, terrain = FALSE,
    weibull = weibullrastercrop, plot = TRUE
  )
  expect_output(str(resCalcEn), "List of 1")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] <
    df[df[, "TotAbschProz"] != 0, "Windmean"]))
  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))
})
