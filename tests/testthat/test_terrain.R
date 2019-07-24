context("Test Terrain Effects")
library(testthat)
library(sp)
library(windfarmGA)
library(raster)


test_that("Test Genetic Algorithm with different Inputs", {
  ## Test Terrain Model ###################
  ## Create Warning, that no Sourface Roughness can be calculated.
  sp_polygon <- Polygon(rbind(c(4498482, 2619203), c(4498482, 2619343),
                              c(4499991, 2619343), c(4499991, 2619203)))
  sp_polygon <- Polygons(list(sp_polygon), 1)
  sp_polygon <- SpatialPolygons(list(sp_polygon))
  Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  data.in <- data.frame(ws = 12, wd = 0)
  proj4string(sp_polygon) <- CRS(Projection)
  options(windfarmGA.connection = stdin())
  resultrect <- expect_warning(genAlgo(Polygon1 = sp_polygon,
                                       n = 5, iteration = 1,
                                       vdirspe = data.in,
                                       Rotor = 20,
                                       RotorHeight = 100, 
                                       topograp = TRUE, verbose = TRUE, 
                                       plotit = TRUE))
  expect_true(nrow(resultrect) == 1)
  expect_is(resultrect, "matrix")
  expect_false(any(unlist(sapply(resultrect, is.na))))
  
  ## Normal Terrain Example
  sp_polygon <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
                              c(4499991, 2669343), c(4499991, 2668272)))
  sp_polygon <- Polygons(list(sp_polygon), 1)
  sp_polygon <- SpatialPolygons(list(sp_polygon))
  proj4string(sp_polygon) <- CRS(Projection)

  resultrect <- genAlgo(Polygon1 = sp_polygon,
                        n = 12, iteration = 1,
                        vdirspe = data.in,
                        Rotor = 30,
                        RotorHeight = 100, 
                        topograp = TRUE, verbose = TRUE, 
                        plotit = TRUE)
  expect_true(nrow(resultrect) == 1)
  expect_is(resultrect, "matrix")
  expect_false(any(unlist(sapply(resultrect, is.na))))

  ## CCL-Raster should be in directory already
  path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
  sourceCCLRoughness <- paste0(path, "clc_legend.csv")
  resultrect <- genAlgo(Polygon1 = sp_polygon,
                        n = 12, iteration = 1,
                        vdirspe = data.in,
                        Rotor = 30,
                        RotorHeight = 100, 
                        topograp = TRUE, verbose = TRUE, 
                        plotit = TRUE, sourceCCL = "g100_06.tif", 
                        sourceCCLRoughness = sourceCCLRoughness)
  expect_true(nrow(resultrect) == 1)
  expect_is(resultrect, "matrix")
  expect_false(any(unlist(sapply(resultrect, is.na))))
  
  ## Weibull Params (FAKE)
  DEM <- raster("srtm_39_03.tif")
  sp_polygonproj <- spTransform(sp_polygon, CRS(proj4string(DEM)))
  DEMcrop <- crop(DEM, sp_polygonproj)
  maxval <- max(values(DEMcrop))
  a_raster <- raster::calc(DEMcrop, function(x) (x / maxval)+1)
  k_raster <- raster::calc(DEMcrop, function(x) (x / maxval)+6)
  resultrect <- genAlgo(Polygon1 = sp_polygon,
                        n = 12, iteration = 1,
                        vdirspe = data.in,
                        Rotor = 30,
                        RotorHeight = 100, verbose = TRUE, 
                        weibull=TRUE, weibullsrc = list(k_raster, a_raster))
  expect_true(nrow(resultrect) == 1)
  expect_is(resultrect, "matrix")
  expect_false(any(unlist(sapply(resultrect, is.na))))
  
  ## Weibull-Raster from Package used (NOT WORKING!) 
  # resultrect <- genAlgo(Polygon1 = sp_polygon,
  #                       n = 12, iteration = 1,
  #                       vdirspe = data.in,
  #                       Rotor = 30,
  #                       RotorHeight = 100, verbose = TRUE, 
  #                       weibull=TRUE)
  # expect_true(nrow(resultrect) == 1)
  # expect_is(resultrect, "matrix")
  # expect_false(any(unlist(sapply(resultrect, is.na))))
  
  ## Plotting Terrain Effects #############
  plres <- plot_result(resultrect, sp_polygon, topographie = T)
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  plres <- plot_result(resultrect, sp_polygon, topographie = T,
                       sourceCCLRoughness = sourceCCLRoughness, 
                       weibullsrc = list(k_raster, a_raster))
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  plres <- plot_result(resultrect, sp_polygon, topographie = T, plotEn = 2,
                       sourceCCLRoughness = sourceCCLRoughness, 
                       weibullsrc = list(k_raster, a_raster))
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  plres <- plot_result(resultrect, sp_polygon, topographie = T, plotEn = 2,
                       sourceCCLRoughness = sourceCCLRoughness, 
                       weibullsrc = list(a_raster * (gamma(1 + (1 / k_raster)))))
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  weibullraster <- a_raster * (gamma(1 + (1 / k_raster)))
  plres <- plot_result(resultrect, sp_polygon, topographie = T, plotEn = 2,
                       sourceCCLRoughness = sourceCCLRoughness, 
                       weibullsrc = weibullraster)
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  
  plres <- plot_result(resultrect, sp_polygon, topographie = T, plotEn = 2)
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  if (length(list.files(pattern = "g100_06.tif")) != 0) {
    file.remove("g100_06.tif")
  }
  plres <- plot_result(resultrect, sp_polygon, topographie = T, plotEn = 1)
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  if (length(list.files(pattern = "g100_06.tif")) != 0) {
    file.remove("g100_06.tif")
  }
  plres <- plot_result(resultrect, sp_polygon, topographie = T, plotEn = 2)
  expect_false(anyNA(plres))
  expect_true(all(plres$EfficAllDir <= 100))
  
  ## calculate_energy with Terrain + Plots ##################
  ## With Terrain (+new function)
  Polygon1 <- Polygon(rbind(c(4488182, 2667172), c(4488182, 2669343),
                            c(4499991, 2669343), c(4499991, 2667172)))
  Polygon1 <- Polygons(list(Polygon1), 1);
  Polygon1 <- SpatialPolygons(list(Polygon1))
  Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  proj4string(Polygon1) <- CRS(Projection)
  srtm <- getDEM(Polygon1)
  srtm_crop <- raster::crop(srtm[[1]], Polygon1)
  
  data.in <- data.frame(ws = 12, wd = 0)
  Rotor <- 50; fcrR <- 3
  resGrid <- GridFilter(shape = Polygon1, resol = Rotor * 5, prop = 1,
                        plotGrid = FALSE)
  resStartGA <- StartGA(Grid = resGrid[[1]], n = 15, nStart = 100)
  
  srtm_crop <- raster::mask(srtm_crop, Polygon1)
  roughrast <- raster::terrain(srtm_crop, "roughness")
  if (all(is.na(values(roughrast)))) {
    values(roughrast) <- 1
  }
  srtm_crop <- list(
    strm_crop = srtm_crop,
    orogr1 = raster::calc(srtm_crop, function(x) {
      x / (raster::cellStats(srtm_crop, mean, na.rm = TRUE))
    }),
    roughness = roughrast
  )
  
  ccl <- raster::raster("g100_06.tif")
  ccl <- crop(ccl, Polygon1)
  ccl <- mask(ccl, Polygon1)
  path <- paste0(system.file(package = "windfarmGA"), "/extdata/")
  sourceCCLRoughness <- paste0(path, "clc_legend.csv")
  rauhigkeitz <- utils::read.csv(sourceCCLRoughness,
                                 header = TRUE, sep = ";")
  cclRaster <- raster::reclassify(ccl,
                                  matrix(c(rauhigkeitz$GRID_CODE,
                                           rauhigkeitz$Rauhigkeit_z),
                                         ncol = 2))
  resCalcEn <- calculate_energy(sel = resStartGA[[1]], referenceHeight = 50,
                                srtm_crop = srtm_crop, cclRaster = cclRaster,
                                RotorHeight = 50, SurfaceRoughness = 0.14, wnkl = 20,
                                distanz = 100000, resol = 200,dirSpeed = data.in,
                                RotorR = 50, polygon1 = Polygon1,
                                topograp = TRUE, weibull = FALSE, plotit = T)
  
  expect_output(str(resCalcEn), "List of 1")
  expect_true(class(resCalcEn[[1]]) == "matrix")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] <
                    df[df[, "TotAbschProz"] != 0, "Windmean"]))
  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))
  
  
  ## Weibull + Plotting
  DEM <- raster("srtm_39_03.tif")
  Polygon1 <- spTransform(Polygon1, CRS(proj4string(DEM)))
  DEMcrop <- crop(DEM, Polygon1)
  maxval <- max(values(DEMcrop))
  a_raster <- raster::calc(DEMcrop, function(x) (x / maxval)+1)
  k_raster <- raster::calc(DEMcrop, function(x) (x / maxval)+6)
  weibullraster <- a_raster * (gamma(1 + (1 / k_raster)))
  weibullraster <- projectRaster(weibullraster, crs = CRS(Projection))
  Polygon1 <- spTransform(Polygon1, CRS(Projection))
  resCalcEn <- calculate_energy(sel = resStartGA[[1]], referenceHeight = 50,
                                srtm_crop = srtm_crop, cclRaster = cclRaster,
                                RotorHeight = 50, SurfaceRoughness = 0.14, wnkl = 20,
                                distanz = 100000, resol = 200,dirSpeed = data.in,
                                RotorR = 50, polygon1 = Polygon1, topograp = FALSE,
                                weibull = weibullraster, plotit = T)
  expect_output(str(resCalcEn), "List of 1")
  expect_true(class(resCalcEn[[1]]) == "matrix")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] <
                    df[df[, "TotAbschProz"] != 0, "Windmean"]))
  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))
  
})