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
  Grid1 <- grid_area(shape = Polygon1,resol = 200,prop = 1);
  Grid <- Grid1[[1]]
  AmountGrids <- nrow(Grid)
  wind <- list(wind, probab = 100)
  ##################
  
  # startsel <- init_population(Grid,10,20);
  # fit <- fitness(selection = startsel, referenceHeight = 100, RotorHeight=100,
  #                SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,rot=20,
  #                dirspeed = wind, srtm_crop="", topograp=FALSE, cclRaster="",
  #                Parallel = TRUE)
  # 
  # expect_output(str(fit), "List of 20")
  # expect_true(all(sapply(fit, nrow) == 10))
  # expect_false(any(unlist(sapply(fit, is.na))))
  # expect_false(any(unlist(do.call("rbind", fit)[,-c(1,2)] < 0)))
  
  
  res <- genetic_algorithm(Polygon1 = Polygon1,
                           n = 12,
                           vdirspe = wind,
                           Rotor = 30,
                           RotorHeight = 100, Parallel = TRUE)
  expect_true(nrow(res) == 20)
  expect_is(res, "matrix")
  expect_false(any(unlist(sapply(res, is.na))))
  
  res <- genetic_algorithm(Polygon1 = Polygon1,
                           n = 12,
                           vdirspe = wind,
                           Rotor = 30,
                           RotorHeight = 100, 
                           Parallel = TRUE, numCluster = 4)
  expect_true(nrow(res) == 20)
  expect_is(res, "matrix")
  expect_false(any(unlist(sapply(res, is.na))))
  
})