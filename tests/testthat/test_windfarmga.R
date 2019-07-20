context("Test windfarmGA")
library(testthat)
library(sp)
library(sf)
library(ggplot2)
library(windfarmGA)

test_that("Test windfarmGA", {
  ## Data ##############
  Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
  Polygon1 <- Polygons(list(Polygon1), 1)
  Polygon1 <- SpatialPolygons(list(Polygon1))
  Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  proj4string(Polygon1) <- CRS(Projection)
  data.in <- data.frame(ws = 12, wd = 0)
  
  ## SpatialPolygon Input #####################
  resultSP <- windfarmGA(Polygon1 = Polygon1,
                         n = 20, iteration = 5,
                         vdirspe = data.in, 
                         selstate = "FIX", crossPart1 = "EQU",
                         Rotor = 35, Proportionality = 1,
                         RotorHeight = 100, plotit = TRUE)
  expect_true(nrow(resultSP) == 5)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))))
  
  
  ## Use DNS and layer (Shapfile from Source) #######################
  
  dns <- system.file("extdata/shape.shp", package = "windfarmGA")
  resultSP <- windfarmGA(dns = dns, layer = "shape",
                         n = 20, iteration = 3,
                         vdirspe = data.in, 
                         selstate = "FIX", crossPart1 = "EQU",
                         Rotor = 35, Proportionality = 1,
                         RotorHeight = 100)
  expect_true(nrow(resultSP) == 3)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))))
  
  ## SpatialPolygon - No Projection #####################
  Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
  Polygon1 <- Polygons(list(Polygon1), 1)
  Polygon1 <- SpatialPolygons(list(Polygon1))
  resultSP <- windfarmGA(Polygon1 = Polygon1,
                         n = 20, iteration = 5,
                         vdirspe = data.in, GridMethod = "h",
                         selstate = "FIX", crossPart1 = "EQU",
                         Rotor = 50, Proportionality = 1,
                         RotorHeight = 100, plotit = TRUE)
  expect_true(nrow(resultSP) == 5)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))))
  
  ## SpatialPolygon - Other Projection #####################
  Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
  Polygon1 <- Polygons(list(Polygon1), 1)
  Polygon1 <- SpatialPolygons(list(Polygon1))
  # Projection <- "+proj=utm +zone=35 +ellps=intl +units=m +no_defs"
  Projection <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  resultSP <- windfarmGA(Polygon1 = Polygon1,Projection = Projection,
                         n = 20, iteration = 5,
                         vdirspe = data.in, GridMethod = "h",
                         selstate = "FIX", crossPart1 = "EQU",
                         Rotor = 80, Proportionality = 1,
                         RotorHeight = 100, plotit = TRUE)
  expect_true(nrow(resultSP) == 5)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))))
  
  ## Errors ############
  expect_error(windfarmGA(Polygon1 = Polygon1,
                         n = 20, iteration = 5,
                         # vdirspe = data.in, 
                         selstate = "FIX", crossPart1 = "EQU",
                         Rotor = 35, Proportionality = 1,
                         RotorHeight = 100, plotit = FALSE))
  expect_error(windfarmGA(Polygon1 = Polygon1,
                          n = "20", iteration = "5",
                          vdirspe = data.in,
                          selstate = "FIX", crossPart1 = "EQU",
                          Rotor = 35, Proportionality = 1,
                          RotorHeight = 100, plotit = FALSE))
  expect_error(windfarmGA(Polygon1 = Polygon1,
                          n = 20, iteration = 5,
                          vdirspe = data.in,
                          selstate = "FIX", crossPart1 = "EQU",
                          Rotor = 35, Proportionality = 1,
                          RotorHeight = 100, 
                          elitism = "asd"))

})
