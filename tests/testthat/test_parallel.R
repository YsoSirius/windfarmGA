context("Parallel")
library(testthat)
library(sp)
library(windfarmGA)

test_that("Test Parallelisation", {
  ## Inputs ##################
  Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
                      c(4499991, 2669343), c(4499991, 2668272)))
  Polygon1 <- Polygons(list(Polygon1),1);
  Polygon1 <- SpatialPolygons(list(Polygon1))
  Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  proj4string(Polygon1) <- CRS(Projection)
  wind <- data.frame(ws = 12, wd = 0)
  ##################
  
  ## genetic_algorithm ####################
  ## Default amount of Cluster
  res <- genetic_algorithm(Polygon1 = Polygon1,
                           n = 12, iteration = 2,
                           vdirspe = wind,
                           Rotor = 30,
                           RotorHeight = 100, Parallel = TRUE)
  expect_true(nrow(res) == 2)
  expect_is(res, "matrix")
  expect_false(any(unlist(sapply(res, is.na))))

  ## Too many Cluster
  res <- genetic_algorithm(Polygon1 = Polygon1,
                           n = 12, iteration = 2,
                           vdirspe = wind,
                           Rotor = 30,
                           RotorHeight = 100,
                           Parallel = TRUE, numCluster = 10)
  expect_true(nrow(res) == 2)
  expect_is(res, "matrix")
  expect_false(any(unlist(sapply(res, is.na))))
  
  ## windfarmGA ####################
  ## Default amount of Cluster
  # res <- windfarmGA(Polygon1 = Polygon1,
  #            n = 12, iteration = 5,
  #            vdirspe = data.in,
  #            selstate = "FIX", crossPart1 = "EQU",
  #            Rotor = 60, Parallel = TRUE,
  #            RotorHeight = 100)
  # expect_true(nrow(res) == 5)
  # expect_is(res, "matrix")
  # expect_false(any(unlist(sapply(res, is.na))))
  # 
  ## Too many Cluster
  # res <- windfarmGA(Polygon1 = Polygon1,
  #                   n = 12, iteration = 5,
  #                   vdirspe = data.in,
  #                   selstate = "FIX", crossPart1 = "EQU",
  #                   Rotor = 60, Parallel = TRUE, 
  #                   numCluster = 10,
  #                   RotorHeight = 100)
  # expect_true(nrow(res) == 5)
  # expect_is(res, "matrix")
  # expect_false(any(unlist(sapply(res, is.na))))
  
})
