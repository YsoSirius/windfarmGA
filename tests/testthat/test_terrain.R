context("Test Terrain and Weibull Effects")
library(terra)
library(elevatr)
library(raster)

## Function to suppress print/cat outputs
quiet <- function(x) {
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

test_that("Test Terrain and Weibull Effects", {
  # skip_on_cran()
  # skip_if_offline()
  skip_if_not_installed("rgdal")
  
  ## Test Terrain_Model Function ###############
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4651704, 4651704, 4654475, 4654475, 4651704),
      c(2692925, 2694746, 2694746, 2692925, 2692925)))),
    crs = 3035
  ))
  Polygon_wgs84 <-  sf::st_transform(Polygon1, st_crs(4326))
  srtm <- suppressMessages(elevatr::get_elev_raster(locations = as(Polygon_wgs84, "Spatial"), z = 11))
  res <- terrain_model(srtm, Polygon1, sourceCCL = terra::rast("g100_06.tif"))
  expect_length(res, 2)
  expect_length(res[[1]], 3)
  expect_length(res[[2]], 1)
  expect_is(res[[2]], "SpatRaster")
  expect_is(res[[1]][[1]], "SpatRaster")
  expect_is(res[[1]][[2]], "SpatRaster")
  expect_is(res[[1]][[3]], "SpatRaster")
  
  res <- terrain_model(srtm, Polygon1, sourceCCL = "g100_06.tif")
  expect_length(res, 2)
  expect_length(res[[1]], 3)
  expect_length(res[[2]], 1)
  expect_is(res[[2]], "SpatRaster")
  expect_is(res[[1]][[1]], "SpatRaster")
  expect_is(res[[1]][[2]], "SpatRaster")
  expect_is(res[[1]][[3]], "SpatRaster")
  
  res <- terrain_model(terra::rast(srtm), Polygon1, sourceCCL = "g100_06.tif")
  expect_length(res, 2)
  expect_length(res[[1]], 3)
  expect_length(res[[2]], 1)
  expect_is(res[[2]], "SpatRaster")
  expect_is(res[[1]][[1]], "SpatRaster")
  expect_is(res[[1]][[2]], "SpatRaster")
  expect_is(res[[1]][[3]], "SpatRaster")
  
  
  srtm_terra <- terra::rast(srtm)
  values(srtm_terra) <- NA
  res <- expect_warning(terrain_model(srtm_terra, Polygon1, sourceCCL = "g100_06.tif"))
  expect_length(res, 2)
  expect_length(res[[1]], 3)
  expect_length(res[[2]], 1)
  expect_is(res[[2]], "SpatRaster")
  expect_is(res[[1]][[1]], "SpatRaster")
  expect_is(res[[1]][[2]], "SpatRaster")
  expect_is(res[[1]][[3]], "SpatRaster")
  
  ## Mock Packages not installed
  with_mock(
    is_elevatr_installed = function() FALSE,
    expect_error(
      terrain_model(topograp = TRUE,Polygon1, sourceCCL = "g100_06.tif")
    )
  )
  
  ## Test GA with Terrain Model ###################
  Projection <- 3035
  data.in <- data.frame(ws = 12, wd = 0)
  
  ## Normal Terrain Example
  sp_polygon <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)))), 
    crs = 3035
  ))
  
  resultrect <- quiet(suppressWarnings(
    genetic_algorithm(Polygon1 = sp_polygon,
                      n = 12, iteration = 1,
                      vdirspe = data.in,
                      Rotor = 30,
                      RotorHeight = 100, 
                      topograp = TRUE, verbose = TRUE, 
                      plotit = TRUE)
  ))
  expect_true(nrow(resultrect) == 1)
  expect_is(resultrect, "matrix")
  expect_false(any(unlist(sapply(resultrect, is.na))))

  ## CCL-Raster should be in directory already
  path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
  sourceCCLRoughness <- paste0(path, "clc_legend.csv")
  resultrect <- quiet(suppressWarnings(
    genetic_algorithm(Polygon1 = sp_polygon,
                      n = 12, iteration = 1,
                      vdirspe = data.in,
                      Rotor = 30,
                      RotorHeight = 100, 
                      topograp = TRUE, verbose = TRUE, 
                      plotit = TRUE, sourceCCL = "g100_06.tif", 
                      sourceCCLRoughness = sourceCCLRoughness)
  ))
  expect_true(nrow(resultrect) == 1)
  expect_is(resultrect, "matrix")
  expect_false(any(unlist(sapply(resultrect, is.na))))
  
  
  
  ## Weibull ################
  ## Weibull Params (FAKE).
  DEM <- suppressWarnings(elevatr::get_elev_raster(verbose = FALSE,
    locations = as(st_transform(sp_polygon, 4326), "Spatial"), z = 11))
  sp_polygonproj <- st_transform(sp_polygon, st_crs(DEM))
  DEM <- terra::rast(DEM)
  DEMcrop <- crop(DEM, sp_polygonproj, mask=TRUE)
  maxval <- max(values(DEMcrop), na.rm=TRUE)
  a_raster <- terra::app(DEMcrop, function(x) (x / maxval)+1)
  k_raster <- terra::app(DEMcrop, function(x) (x / maxval)+6)
  resultrect <- quiet(suppressWarnings(
    genetic_algorithm(Polygon1 = sp_polygon,
                      n = 12, iteration = 1,
                      vdirspe = data.in,
                      Rotor = 30,
                      RotorHeight = 100,
                      verbose = TRUE, 
                      weibull = TRUE,
                      weibullsrc = list(k_raster, a_raster))
  ))
  expect_true(nrow(resultrect) == 1)
  expect_is(resultrect, "matrix")
  expect_false(any(unlist(sapply(resultrect, is.na))))
  
  ## Plotting Terrain Effects #############
  plres <- suppressWarnings(
    plot_result(resultrect, sp_polygon,
                topographie = TRUE,
                plotEn = 1,
                sourceCCLRoughness = sourceCCLRoughness, 
                weibullsrc = list(a_raster * (gamma(1 + (1 / values(k_raster))))))
  )
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  plres <- plot_result(resultrect, sp_polygon,
                weibullsrc = list(k_raster, a_raster))
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  plres <- plot_result(resultrect, sp_polygon,
                       weibullsrc = list(raster::raster(k_raster),
                                         raster::raster(a_raster)))
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  ## Weibull Single Raster for mean wind spead
  weibullraster <- a_raster * (gamma(1 + (1 / values(k_raster))))
  plres <- plot_result(resultrect, sp_polygon, plotEn = 2,
                weibullsrc = weibullraster)
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  if (length(list.files(pattern = "g100_06.tif")) != 0) {
    file.remove("g100_06.tif")
  }
  plres <- plot_result(resultrect,
                sp_polygon, 
                topographie = TRUE,
                plotEn = 1)
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  ## calculate_energy with Terrain + Plots!! ##################
  ## With Terrain (+new function)
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)))), 
    crs = 3035
  ))
  srtm <- suppressWarnings(
    elevatr::get_elev_raster(
    locations = as(Polygon1, "Spatial"), z = 11)
  )
  srtm_crop <- terra::crop(terra::rast(srtm), Polygon1)
  
  data.in <- data.frame(ws = 12, wd = 0)
  Rotor <- 50; fcrR <- 3
  resGrid <- grid_area(shape = Polygon1, size = Rotor * fcrR,
                       prop = 1, plotGrid = FALSE)
  resStartGA <- init_population(Grid = resGrid[[1]], n = 15, nStart = 100)
  
  srtm_crop <- terra::mask(srtm_crop, Polygon1)
  roughrast <- terra::terrain(srtm_crop, "roughness")
  if (all(is.na(values(roughrast)))) {
    values(roughrast) <- 1
  }
  srtm_crop <- list(
    strm_crop = srtm_crop,
    orogr1 = srtm_crop / as.numeric(terra::global(srtm_crop, fun="mean", na.rm = TRUE)),
    roughness = roughrast
  )
  
  ccl <- terra::rast("g100_06.tif")
  ccl <- crop(ccl, Polygon1, mask = TRUE)
  path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
  sourceCCLRoughness <- paste0(path, "clc_legend.csv")
  rauhigkeitz <- utils::read.csv(sourceCCLRoughness,
                                 header = TRUE, sep = ";")
  cclRaster <- terra::classify(ccl, matrix(c(rauhigkeitz$GRID_CODE,
                                             rauhigkeitz$Rauhigkeit_z),
                                           ncol = 2))
  resCalcEn <- calculate_energy(sel = resStartGA[[1]], referenceHeight = 50,
                                srtm_crop = srtm_crop, cclRaster = cclRaster,
                                RotorHeight = 50, SurfaceRoughness = 0.14, wnkl = 20,
                                distanz = 100000, dirSpeed = data.in,
                                RotorR = 50, polygon1 = Polygon1,
                                topograp = TRUE, weibull = FALSE, plotit = TRUE)
  expect_output(str(resCalcEn), "List of 1")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] <
                    df[df[, "TotAbschProz"] != 0, "Windmean"]))
  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))

    
  # Weibull + Plotting
  DEMcrop <- srtm_crop$orogr1
  maxval <- max(values(DEMcrop))
  a_raster <- terra::app(DEMcrop, function(x) (x / maxval)+1)
  k_raster <- terra::app(DEMcrop, function(x) (x / maxval)+6)
  weibullraster <- a_raster * (gamma(1 + (1 / values(k_raster))))
  resCalcEn <- calculate_energy(sel = resStartGA[[1]], referenceHeight = 50,
                                srtm_crop = srtm_crop, cclRaster = cclRaster,
                                RotorHeight = 50, SurfaceRoughness = 0.14, wnkl = 20,
                                distanz = 100000, dirSpeed = data.in,
                                RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
                                weibull = weibullraster, plotit = TRUE)
  expect_output(str(resCalcEn), "List of 1")
  # expect_true(class(resCalcEn[[1]]) == "matrix")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] <
                    df[df[, "TotAbschProz"] != 0, "Windmean"]))
  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))
  
  
  resCalcEn <- calculate_energy(sel = resStartGA[[1]], referenceHeight = 50,
                                srtm_crop = srtm_crop, cclRaster = cclRaster,
                                RotorHeight = 50, SurfaceRoughness = 0.14, wnkl = 20,
                                distanz = 100000, dirSpeed = data.in,
                                RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
                                weibull = raster::raster(weibullraster), plotit = TRUE)
  expect_output(str(resCalcEn), "List of 1")
  # expect_true(class(resCalcEn[[1]]) == "matrix")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] <
                    df[df[, "TotAbschProz"] != 0, "Windmean"]))
  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))
  
  
  ## Make Hole in Weibull-Raster, so some Values are NA 
  min_y_ppt <- data.frame(resStartGA[[1]][order(resStartGA[[1]][,2])<=9,])
  min_y_ppt <- st_as_sf(min_y_ppt, coords=c("X","Y"))
  weibullrastercrop <- crop(weibullraster, terra::ext(min_y_ppt))
  resCalcEn <- calculate_energy(sel = resStartGA[[1]], referenceHeight = 50,
                                srtm_crop = srtm_crop, cclRaster = cclRaster,
                                RotorHeight = 50, SurfaceRoughness = 0.14, wnkl = 20,
                                distanz = 100000, dirSpeed = data.in,
                                RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
                                weibull = weibullrastercrop, plotit = T)
  expect_output(str(resCalcEn), "List of 1")
  # expect_true(class(resCalcEn[[1]]) == "matrix")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] <
                    df[df[, "TotAbschProz"] != 0, "Windmean"]))
  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))
  
})
