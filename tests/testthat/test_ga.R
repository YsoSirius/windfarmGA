context("Test GA")
library(testthat)
library(sp)
library(sf)
library(ggplot2)
library(windfarmGA)

test_that("Test Genetic Algorithm with different Inputs", {
  ## Data ##############
  Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
  Polygon1 <- Polygons(list(Polygon1), 1)
  Polygon1 <- SpatialPolygons(list(Polygon1))
  Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  proj4string(Polygon1) <- CRS(Projection)
  data.in <- data.frame(ws = 12, wd = 0)

  ## SpatialPolygon Input - 100 Iteration #####################
  resultSP <- genAlgo(Polygon1 = Polygon1,
                      n = 20, iteration = 100,
                      vdirspe = data.in,
                      Rotor = 35, Proportionality = 1,
                      RotorHeight = 100)
  expect_true(nrow(resultSP) == 100)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))))
  
  ## SpatialPolygon Input #####################
  resultSP <- genAlgo(Polygon1 = Polygon1,
                      n = 20, iteration = 1,
                      vdirspe = data.in,
                      Rotor = 35, Proportionality = 1,
                      RotorHeight = 100)
  
  expect_true(nrow(resultSP) == 1)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP[,1:13], is.na))))


  ## SimpleFeature Input #####################
  PolygonSF <- sf::st_as_sf(Polygon1)
  resultSF <- genAlgo(Polygon1 = PolygonSF,
                      n = 20, iteration = 1,
                      vdirspe = data.in,
                      Rotor = 35, Proportionality = 1,
                      RotorHeight = 100)
  expect_true(nrow(resultSF) == 1)
  expect_is(resultSF, "matrix")
  expect_false(any(unlist(sapply(resultSF[,1:13], is.na))))


  ## Data.Frame Input #####################
  PolygonDF <- ggplot2::fortify(Polygon1)
  resultDF <- genAlgo(Polygon1 = PolygonDF,
                        n = 20, iteration = 1,
                        vdirspe = data.in,
                        Rotor = 30,
                        RotorHeight = 100)
  expect_true(nrow(resultDF) == 1)
  expect_is(resultDF, "matrix")
  expect_false(any(unlist(sapply(resultDF[,1:13], is.na))))

  ## Matrix Input #####################
  PolygonMat <- ggplot2::fortify(Polygon1)
  PolygonMat <- as.matrix(PolygonMat[,1:2])
  resultMA <- genAlgo(Polygon1 = PolygonMat, verbose = T, plotit = TRUE,
                        n = 20, iteration = 1,
                        vdirspe = data.in,
                        Rotor = 30,
                        RotorHeight = 100)
  expect_true(nrow(resultMA) == 1)
  expect_is(resultMA, "matrix")
  expect_false(any(unlist(sapply(resultMA[,1:13], is.na))))
  
  ## Matrix Input - 100% #####################
  resultMA100 <- genAlgo(Polygon1 = PolygonMat, verbose = F, plotit = TRUE,
                      n = 10, iteration = 20,
                      vdirspe = data.in,
                      Rotor = 30,
                      RotorHeight = 100)
  expect_is(resultMA100, "matrix")
  expect_false(any(unlist(sapply(resultMA100[,1:13], is.na))))
  expect_true(any(as.vector(sapply(resultMA100[,3], function(x) x[,"EfficAllDir"] == 100))))
  
  ## Parellel Processing #########################
  # resultMAP <- genAlgo(Polygon1 = PolygonMat, verbose = T, plotit = F,
  #                      n = 20, iteration = 1, Parallel = T,
  #                      vdirspe = data.in,
  #                      Rotor = 30,
  #                      RotorHeight = 100)
  # expect_true(nrow(resultMAP) == 1)
  # expect_is(resultMAP, "matrix")
  # expect_false(any(unlist(sapply(resultMAP[,1:13], is.na))))
  # 
  # 
  # resultMAP50 <- genAlgo(Polygon1 = PolygonMat, verbose = T, plotit = F,
  #                     n = 20, iteration = 50, Parallel = T,
  #                     vdirspe = data.in,
  #                     Rotor = 30,
  #                     RotorHeight = 100)
  # expect_true(nrow(resultMAP50) == 50)
  # expect_is(resultMAP50, "matrix")
  # expect_false(any(unlist(sapply(resultMAP50[,1:13], is.na))))
  
  ## Test with non default arguments ####################
  PolygonMat <- ggplot2::fortify(Polygon1)
  PolygonMat <- as.matrix(PolygonMat[,1:2])
  resultMA <- genAlgo(Polygon1 = PolygonMat,
                      n = 20, iteration = 1, GridMethod = "h",
                      vdirspe = data.in, elitism = F, 
                      selstate = "var", crossPart1 = "ran", 
                      trimForce = TRUE,
                      Rotor = 30,
                      RotorHeight = 100)
  expect_true(nrow(resultMA) == 1)
  expect_is(resultMA, "matrix")
  expect_false(any(unlist(sapply(resultMA[,1:13], is.na))))
  
  PolygonMat <- ggplot2::fortify(Polygon1)
  PolygonMat <- as.matrix(PolygonMat[,1:2])
  resultMA <- genAlgo(Polygon1 = PolygonMat,
                      n = 20, iteration = 1, GridMethod = "h",
                      vdirspe = data.in, elitism = T, nelit = 10000, 
                      selstate = "var", crossPart1 = "ran", 
                      trimForce = TRUE, mutr = 15,
                      Rotor = 50, 
                      # Projection = "+proj=tmerc +lat_0=0 +lon_0=31 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +pm=ferro +units=m +no_defs",
                      Projection = "+proj=lcc +lat_1=49 +lat_2=46 +lat_0=47.5 +lon_0=13.33333333333333 +x_0=400000 +y_0=400000 +ellps=bessel +towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232 +units=m +no_defs",
                      # Projection = "+proj=tmerc +lat_0=0 +lon_0=28 +k=1 +x_0=0 +y_0=0 +ellps=bessel +pm=ferro +units=m +no_defs",
                      # Projection = "+proj=lcc +lat_1=49 +lat_2=46 +lat_0=47.5 +lon_0=13.33333333333333 +x_0=400000 +y_0=400000 +ellps=GRS80 +units=m +no_defs",
                      # Projection = "+proj=tmerc +lat_0=0 +lon_0=15 +k=1 +x_0=6500000 +y_0=0 +ellps=bessel +units=m +no_defs",
                      RotorHeight = 100, plotit = F)
  expect_true(nrow(resultMA) == 1)
  expect_is(resultMA, "matrix")
  expect_false(any(unlist(sapply(resultMA[,1:13], is.na))))


  ## Create errors ####################
  expect_error(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", plotit = TRUE,
                       vdirspe = data.in,
                       n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       Rotor = 30))
  
  expect_error(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", plotit = TRUE,
                       vdirspe = data.in,
                       # n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       Rotor = 30,
                       RotorHeight = 100))

  expect_error(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", 
                       # vdirspe = data.in, 
                       n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       Rotor = 30,
                       RotorHeight = 100))

  expect_error(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", 
                       vdirspe = data.in, 
                       n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       # Rotor = 30,
                       RotorHeight = 100))
  
  expect_error(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", 
                       vdirspe = data.in, 
                       n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       Rotor = 30
                       # ,RotorHeight = 100
                       ))
  expect_error(genetic_algorithm(Polygon1 = Polygon1,
                       GridMethod = "h", 
                       vdirspe = data.in, 
                       n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       Rotor = 30
                       # ,RotorHeight = 100
                       ))
  
  sp_polygon <- Polygon(rbind(c(1, 1), c(1, 2000),
                              c(2000, 2000), c(2000, 1)))
  sp_polygon <- Polygons(list(sp_polygon), 1)
  sp_polygon <- SpatialPolygons(list(sp_polygon))
  projection <- paste("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000",
                      "+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(sp_polygon) <- CRS(projection)
  expect_error(genAlgo(Polygon1 = sp_polygon,
                       n = 12, iteration = 1,
                       vdirspe = winddat,
                       Rotor = 30, 
                       RotorHeight = 100, topograp = TRUE, verbose = TRUE))
  

  
  ## Test Terrain Model ###################
  sp_polygon <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
                              c(4499991, 2669343), c(4499991, 2668272)))
  sp_polygon <- Polygons(list(sp_polygon), 1)
  sp_polygon <- SpatialPolygons(list(sp_polygon))
  proj4string(sp_polygon) <- CRS(projection)
  winddat <- data.frame(ws = 12, wd = 0)
  resultrect <- genAlgo(Polygon1 = sp_polygon,
                        n = 12, iteration = 1,
                        vdirspe = winddat,
                        Rotor = 30, 
                        RotorHeight = 100, topograp = TRUE, verbose = TRUE)
  expect_true(nrow(resultrect) == 1)
  expect_is(resultrect, "matrix")
  expect_false(any(unlist(sapply(resultrect, is.na))))
  
  sp_polygon <- Polygon(rbind(c(4498482, 2550272), c(4498482, 2669343),
                              c(4499991, 2669343), c(4499991, 2550272)))
  sp_polygon <- Polygons(list(sp_polygon), 1)
  sp_polygon <- SpatialPolygons(list(sp_polygon))
  projection <- paste("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000",
                      "+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(sp_polygon) <- CRS(projection)
  projection84 <- paste("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  sp_polygon <- spTransform(sp_polygon, CRS(projection84))
  winddat <- data.frame(ws = 12, wd = 0)
  resultrect <- genAlgo(Polygon1 = sp_polygon,
                        n = 12, iteration = 1,
                        vdirspe = winddat,
                        Rotor = 30, 
                        RotorHeight = 100, topograp = TRUE, verbose = TRUE)
  expect_true(nrow(resultrect) == 1)
  expect_is(resultrect, "matrix")
  expect_false(any(unlist(sapply(resultrect, is.na))))
  
  
  sp_polygon <- Polygon(rbind(c(4498482, 2550272), c(4498482, 2669343),
                              c(4499991, 2669343), c(4499991, 2550272)))
  sp_polygon <- Polygons(list(sp_polygon), 1)
  sp_polygon <- SpatialPolygons(list(sp_polygon))
  projection <- paste("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000",
                      "+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(sp_polygon) <- CRS(projection)
  resultrect <- genAlgo(Polygon1 = sp_polygon,
                        n = 12, iteration = 1,
                        vdirspe = winddat,
                        Rotor = 30, 
                        RotorHeight = 100, topograp = TRUE, verbose = TRUE)
  expect_true(nrow(resultrect) == 1)
  expect_is(resultrect, "matrix")
  expect_false(any(unlist(sapply(resultrect, is.na))))
  ## Weibull Raster ######################
  # resultrect <- genAlgo(Polygon1 = sp_polygon,
  #                       n = 12, iteration = 1,
  #                       vdirspe = winddat,
  #                       Rotor = 30, 
  #                       RotorHeight = 100, topograp = TRUE, weibull = TRUE)
  # expect_true(nrow(resultrect) == 1)
  # expect_is(resultrect, "matrix")
  # expect_false(any(unlist(sapply(resultrect, is.na))))
  
})