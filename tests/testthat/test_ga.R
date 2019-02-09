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
  resultMA <- genAlgo(Polygon1 = PolygonMat,
                        n = 20, iteration = 1,
                        vdirspe = data.in,
                        Rotor = 30,
                        RotorHeight = 100)
  expect_true(nrow(resultMA) == 1)
  expect_is(resultMA, "matrix")
  expect_false(any(unlist(sapply(resultMA[,1:13], is.na))))
  
})