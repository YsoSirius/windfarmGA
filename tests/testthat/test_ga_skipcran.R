context("Test Terrain Model and Weibull Raster")
library(testthat)
library(sp)
library(sf)
library(ggplot2)
library(windfarmGA)
# testthat::test_file(path = "./tests/testthat/test_ga_skipcran.R")

test_that("Test Terrain Model and Weibull", {
  skip_on_cran()
  # skip_on_travis()
  # skip_on_appveyor()

  ## Data ##############
  # Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
  #                           c(4499991, 2669343), c(4499991, 2668272)))
  # Polygon1 <- Polygons(list(Polygon1), 1)
  # Polygon1 <- SpatialPolygons(list(Polygon1))
  # Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
  # +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  # proj4string(Polygon1) <- CRS(Projection)
  # winddf <- data.frame(ws = 12, wd = 0)

  ## Terrain Effect Model - Download Elevation and Corine Land Cover #####################
  # resultSP <- genAlgo(Polygon1 = Polygon1,
  #                     n = 12, iteration = 1,
  #                     vdirspe = winddf,
  #                     topograp = TRUE, verbose = T,
  #                     Rotor = 35, Proportionality = 1,
  #                     RotorHeight = 100)
  # expect_true(nrow(resultSP) == 1)
  # expect_is(resultSP, "matrix")
  # expect_false(any(unlist(sapply(resultSP[,1:13], is.na))))

  ##  Test Weibull Raster #####################
  # resultWB <- genAlgo(Polygon1 = Polygon1,
  #                     n = 12, iteration = 1,
  #                     vdirspe = winddf,
  #                     weibull = T, verbose = T,
  #                     Rotor = 35, Proportionality = 1,
  #                     RotorHeight = 100)
  # expect_true(nrow(resultWB) == 1)
  # expect_is(resultWB, "matrix")
  # expect_false(any(unlist(sapply(resultWB[,1:13], is.na))))


})
