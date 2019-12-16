context("Test GA")
library(sp)
library(sf)
library(ggplot2)

## Function to suppress print/cat outputs
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

test_that("Test Genetic Algorithm with different Inputs", {
  skip_on_cran()
  
  ## Data ##############
  Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
  Polygon1 <- Polygons(list(Polygon1), 1)
  Polygon1 <- SpatialPolygons(list(Polygon1))
  Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  proj4string(Polygon1) <- CRS(Projection)
  data.in <- data.frame(ws = 12, wd = 0)

  ## SpatialPolygon Input - 100 Iteration #####################
  resultSP <- quiet(genAlgo(Polygon1 = Polygon1,
                      n = 20, iteration = 100,
                      vdirspe = data.in,
                      Rotor = 35, Proportionality = 1,
                      RotorHeight = 100, verbose = TRUE))
  expect_true(nrow(resultSP) == 100)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))));
  
  ## Multi Polygon ###########################
  resultSP <- quiet(genAlgo(Polygon1 = multi_shape,
                            n = 20, iteration = 3,
                            vdirspe = data.in,
                            Rotor = 35, Proportionality = 1,
                            RotorHeight = 100, plotit=T))
  expect_true(nrow(resultSP) == 3)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))));
  
  resultSP <- quiet(genAlgo(Polygon1 = multi_shape,
                            n = 20, iteration = 3, GridMethod = "h",
                            vdirspe = data.in,
                            Rotor = 35, Proportionality = 1,
                            RotorHeight = 100, plotit=T))
  expect_true(nrow(resultSP) == 3)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))));
  
  ## Hole Polygon ###########################
  resultSP <- quiet(genAlgo(Polygon1 = hole_shape,
                            n = 20, iteration = 3,
                            vdirspe = data.in,
                            Rotor = 35, Proportionality = 1,
                            RotorHeight = 100, plotit=T))
  expect_true(nrow(resultSP) == 3)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))));
  
  resultSP <- quiet(genAlgo(Polygon1 = hole_shape,
                            n = 20, iteration = 3, GridMethod = "h",
                            vdirspe = data.in,
                            Rotor = 35, Proportionality = 1,
                            RotorHeight = 100, plotit=T))
  expect_true(nrow(resultSP) == 3)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))));
  
  ## SpatialPolygon Input #####################
  resultSP <- genAlgo(Polygon1 = Polygon1,
                      n = 20, iteration = 1,
                      vdirspe = data.in,
                      Rotor = 35, Proportionality = 1,
                      RotorHeight = 100)
  
  expect_true(nrow(resultSP) == 1)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))));


  ## SimpleFeature Input #####################
  PolygonSF <- sf::st_as_sf(Polygon1)
  resultSF <- genAlgo(Polygon1 = PolygonSF,
                      n = 20, iteration = 1,
                      vdirspe = data.in,
                      Rotor = 35, Proportionality = 1,
                      RotorHeight = 100)
  expect_true(nrow(resultSF) == 1)
  expect_is(resultSF, "matrix")
  expect_false(any(unlist(sapply(resultSF, is.na)))) 


  ## Data.Frame Input #####################
  PolygonDF <- ggplot2::fortify(Polygon1)
  resultDF <- genAlgo(Polygon1 = PolygonDF,
                        n = 20, iteration = 1,
                        vdirspe = data.in,
                        Rotor = 30,
                        RotorHeight = 100)
  expect_true(nrow(resultDF) == 1)
  expect_is(resultDF, "matrix")
  expect_false(any(unlist(sapply(resultDF, is.na))));

  ## Matrix Input #####################
  PolygonMat <- ggplot2::fortify(Polygon1)
  PolygonMat <- as.matrix(PolygonMat[,1:2])
  resultMA <- genAlgo(Polygon1 = PolygonMat, plotit = TRUE,
                        n = 20, iteration = 1,
                        vdirspe = data.in,
                        Rotor = 30,
                        RotorHeight = 100)
  expect_true(nrow(resultMA) == 1)
  expect_is(resultMA, "matrix")
  expect_false(any(unlist(sapply(resultMA, is.na))));
  
  ## Matrix Input - 100% #####################
  resultMA100 <- genAlgo(Polygon1 = PolygonMat, verbose = F, plotit = TRUE,
                      n = 10, iteration = 20,
                      vdirspe = data.in,
                      Rotor = 30,
                      RotorHeight = 100)
  expect_is(resultMA100, "matrix")
  expect_false(any(unlist(sapply(resultMA100, is.na))))
  expect_true(any(as.vector(sapply(resultMA100[,3], function(x) x[,"EfficAllDir"] == 100))))

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
  expect_false(any(unlist(sapply(resultMA, is.na))))
  
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
  ## RotorHeight missing
  expect_error(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", plotit = TRUE,
                       vdirspe = data.in,
                       n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       # RotorHeight = 100,
                       Rotor = 30))
  expect_error(genetic_algorithm(Polygon1 = Polygon1,
                                 GridMethod = "h", 
                                 vdirspe = data.in, 
                                 n = 12,
                                 elitism = F, 
                                 selstate = "var", crossPart1 = "ran", 
                                 trimForce = TRUE,
                                 # ,RotorHeight = 100
                                 Rotor = 30))
  
  ## n is missing
  expect_error(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", plotit = TRUE,
                       vdirspe = data.in,
                       # n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       Rotor = 30,
                       RotorHeight = 100))
  ## No winddata
  expect_error(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", 
                       # vdirspe = data.in, 
                       n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       Rotor = 30,
                       RotorHeight = 100))

  ## No Rotor Radius
  expect_error(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", 
                       vdirspe = data.in, 
                       n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       # Rotor = 30,
                       RotorHeight = 100))
  
  ## Cannot download SRTM (Wrong Polygon)
  sp_polygon <- Polygon(rbind(c(1, 1), c(1, 2000),
                              c(2000, 2000), c(2000, 1)))
  sp_polygon <- Polygons(list(sp_polygon), 1)
  sp_polygon <- SpatialPolygons(list(sp_polygon))
  projection <- paste("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000",
                      "+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  proj4string(sp_polygon) <- CRS(projection)
  expect_error(genAlgo(Polygon1 = sp_polygon,
                       n = 12, iteration = 1,
                       vdirspe = data.in,
                       Rotor = 30, 
                       RotorHeight = 100, topograp = TRUE, verbose = TRUE))

})
