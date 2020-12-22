context("Test GA")
library(sf)
library(ggplot2)

## Function to suppress print/cat outputs
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

test_that("Test Genetic Algorithm with different Inputs", {
  # skip_on_cran()
  
  ## Data ##############
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)))),
    crs = 3035
  ))
  data.in <- data.frame(ws = 12, wd = 0)

  ## SpatialPolygon Input - 100 Iteration #####################
  resultSP <- expect_warning(quiet(genAlgo(Polygon1 = Polygon1,
                      n = 20, iteration = 100,
                      vdirspe = data.in,
                      Rotor = 35, Proportionality = 1,
                      RotorHeight = 100, verbose = TRUE)))
  expect_true(nrow(resultSP) == 100)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))));
  
  ## Multi Polygon ###########################
  resultSP <- expect_warning(quiet(genAlgo(Polygon1 = multi_shape,
                            n = 20, iteration = 3,
                            vdirspe = data.in,
                            Rotor = 35, Proportionality = 1,
                            RotorHeight = 100, plotit=T)))
  # plot_result(resultSP, multi_shape, best = 1, plotEn = 1, topographie = FALSE)
  expect_true(nrow(resultSP) == 3)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))));
  
  resultSP <- expect_warning(quiet(genAlgo(Polygon1 = multi_shape,
                            n = 20, iteration = 3, GridMethod = "h",
                            vdirspe = data.in,
                            Rotor = 35, Proportionality = 1,
                            RotorHeight = 100, plotit=T)))
  expect_true(nrow(resultSP) == 3)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))));
  
  ## Hole Polygon ###########################
  resultSP <- expect_warning(quiet(genAlgo(Polygon1 = hole_shape,
                            n = 20, iteration = 3,
                            vdirspe = data.in,
                            Rotor = 35, Proportionality = 1,
                            RotorHeight = 100, plotit=T)))
  # plot_result(resultSP, hole_shape, best = 1, plotEn = 1, topographie = FALSE)
  expect_true(nrow(resultSP) == 3)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))));
  
  resultSP <- expect_warning(quiet(genAlgo(Polygon1 = hole_shape,
                            n = 20, iteration = 3, GridMethod = "h",
                            vdirspe = data.in,
                            Rotor = 35, Proportionality = 1,
                            RotorHeight = 100, plotit=T)))
  expect_true(nrow(resultSP) == 3)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))));
  
  ## SpatialPolygon Input #####################
  PolygonSP <- expect_warning(as(Polygon1, "Spatial"))
  resultSP <- expect_warning(genAlgo(Polygon1 = PolygonSP,
                      n = 20, iteration = 1,
                      vdirspe = data.in,
                      Rotor = 35, Proportionality = 1,
                      RotorHeight = 100))
  expect_true(nrow(resultSP) == 1)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na)))) 


  ## SimpleFeature Input #####################
  resultSP <- expect_warning(genAlgo(Polygon1 = Polygon1,
                      n = 20, iteration = 1,
                      vdirspe = data.in,
                      Rotor = 35, Proportionality = 1,
                      RotorHeight = 100))
  expect_true(nrow(resultSP) == 1)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))));

  ## Data.Frame Input #####################
  PolygonDF <- st_coordinates(Polygon1)
  resultDF <- expect_warning(genAlgo(Polygon1 = PolygonDF,
                        n = 20, iteration = 1,
                        vdirspe = data.in,
                        Rotor = 30,
                        RotorHeight = 100))
  # plot_result(resultDF, Polygon1, best = 1, plotEn = 1, topographie = FALSE)
  expect_true(nrow(resultDF) == 1)
  expect_is(resultDF, "matrix")
  expect_false(any(unlist(sapply(resultDF, is.na))));

  ## Matrix Input #####################
  PolygonMat <- st_coordinates(Polygon1)
  PolygonMat <- as.matrix(PolygonMat[,1:2])
  resultMA <- expect_warning(genAlgo(Polygon1 = PolygonMat, plotit = TRUE,
                        n = 20, iteration = 1,
                        vdirspe = data.in,
                        Rotor = 30,
                        RotorHeight = 100))
  expect_true(nrow(resultMA) == 1)
  expect_is(resultMA, "matrix")
  expect_false(any(unlist(sapply(resultMA, is.na))));
  
  ## Matrix Input - 100% #####################
  resultMA100 <- expect_warning(quiet(
    genAlgo(Polygon1 = PolygonMat, verbose = F, plotit = TRUE,
            n = 10, iteration = 20,
            vdirspe = data.in,
            Rotor = 30,
            RotorHeight = 100)))
  expect_is(resultMA100, "matrix")
  expect_false(any(unlist(sapply(resultMA100, is.na))))
  # expect_true(any(as.vector(sapply(resultMA100[,3], function(x) x[,"EfficAllDir"] == 100))))

  ## Test with non default arguments ####################
  PolygonMat <- st_coordinates(Polygon1)
  PolygonMat <- as.matrix(PolygonMat[,1:2])
  colnames(PolygonMat) <- c("hor", "vert")
  resultMA <- expect_warning(genAlgo(Polygon1 = PolygonMat,
                      n = 20, iteration = 1, GridMethod = "h",
                      vdirspe = data.in, elitism = F, 
                      selstate = "var", crossPart1 = "ran", 
                      trimForce = TRUE,
                      Rotor = 30,
                      RotorHeight = 100))
  expect_true(nrow(resultMA) == 1)
  expect_is(resultMA, "matrix")
  expect_false(any(unlist(sapply(resultMA, is.na))))
  
  resultMA <- expect_warning(genAlgo(Polygon1 = PolygonMat,
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
                      RotorHeight = 100, plotit = F))
  expect_true(nrow(resultMA) == 1)
  expect_is(resultMA, "matrix")
  expect_false(any(unlist(sapply(resultMA[,1:13], is.na))))

  ## Create errors ####################
  ## RotorHeight missing
  expect_error(expect_warning(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", plotit = TRUE,
                       vdirspe = data.in,
                       n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       # RotorHeight = 100,
                       Rotor = 30)))
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
  expect_error(expect_warning(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", plotit = TRUE,
                       vdirspe = data.in,
                       # n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       Rotor = 30,
                       RotorHeight = 100)))
  ## No winddata
  expect_error(expect_warning(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", 
                       # vdirspe = data.in, 
                       n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       Rotor = 30,
                       RotorHeight = 100)))

  ## No Rotor Radius
  expect_error(expect_warning(genAlgo(Polygon1 = Polygon1,
                       GridMethod = "h", 
                       vdirspe = data.in, 
                       n = 12,
                       elitism = F, 
                       selstate = "var", crossPart1 = "ran", 
                       trimForce = TRUE,
                       # Rotor = 30,
                       RotorHeight = 100)))
  
  ## Cannot download SRTM (Wrong Polygon)
  wrong_poly <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(1, 1, 2000, 2000, 1),
      c(1, 2000, 2000, 1, 1)))),
    crs = 3035
  ))
  expect_error(expect_warning(
    genAlgo(Polygon1 = wrong_poly,
            n = 12, iteration = 1, plotit = TRUE,
            vdirspe = data.in,
            Rotor = 30,
            RotorHeight = 100, topograp = TRUE, verbose = TRUE)
  ))

})
