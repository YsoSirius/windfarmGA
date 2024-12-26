
## Function to suppress print/cat outputs
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

test_that("Test Genetic Algorithm with different Inputs", {

  ## Data ##############
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  vdata <- data.frame(ws = 12, wd = 0)

  ## All Green ################
  resultSP <- genetic_algorithm(
    Polygon1 = Polygon1,
    n = 5, iteration = 30,
    vdirspe = vdata,
    Rotor = 35, Proportionality = 1,
    RotorHeight = 100, verbose = FALSE,
    plotit = TRUE
  )
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## Replace Park with highest Fitness level ################
  resultSP <- suppressMessages(
    genetic_algorithm(
    Polygon1 = Polygon1,
    n = 16, iteration = 100,
    vdirspe = vdata,
    Rotor = 35, Proportionality = 1,
    RotorHeight = 100, verbose = TRUE
  ))
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## No optimization possible - Turbines in all Grid Cells ################
  resultSP <- genetic_algorithm(
    Polygon1 = Polygon1,
    n = 5, iteration = 30,
    vdirspe = vdata,
    Rotor = 71, Proportionality = 1,
    RotorHeight = 100, verbose = FALSE,
    plotit = TRUE
  )
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## SF Polygon Input - 30 Iteration #####################
  resultSP <- quiet(genetic_algorithm(
    Polygon1 = Polygon1,
    n = 20, iteration = 30,
    vdirspe = vdata,
    Rotor = 35, Proportionality = 1,
    RotorHeight = 100, verbose = TRUE
  ))
  expect_true(nrow(resultSP) == 30)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## Multi Polygon ###########################
  resultSP <- quiet(genetic_algorithm(
    Polygon1 = multi_shape,
    n = 20, iteration = 3,
    vdirspe = vdata,
    Rotor = 35, Proportionality = 1,
    RotorHeight = 100, plotit = TRUE
  ))
  expect_true(nrow(resultSP) == 3)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  resultSP <- quiet(genetic_algorithm(
    Polygon1 = multi_shape,
    n = 20, iteration = 3, GridMethod = "h",
    vdirspe = vdata,
    Rotor = 35, Proportionality = 1,
    RotorHeight = 100, plotit = TRUE
  ))
  expect_true(nrow(resultSP) == 3)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## Hole Polygon ###########################
  resultSP <- quiet(genetic_algorithm(
    Polygon1 = hole_shape,
    n = 20, iteration = 3,
    vdirspe = vdata,
    Rotor = 35, Proportionality = 1,
    RotorHeight = 100, plotit = TRUE
  ))
  expect_true(nrow(resultSP) == 3)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  resultSP <- quiet(genetic_algorithm(
    Polygon1 = hole_shape,
    n = 20, iteration = 3, GridMethod = "h",
    vdirspe = vdata,
    Rotor = 35, Proportionality = 1,
    RotorHeight = 100, plotit = TRUE
  ))
  expect_true(nrow(resultSP) == 3)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## SpatialPolygon Input #####################
  PolygonSP <- as(Polygon1, "Spatial")
  resultSP <- genetic_algorithm(
    Polygon1 = PolygonSP,
    n = 20, iteration = 1,
    vdirspe = vdata,
    Rotor = 35, Proportionality = 1,
    RotorHeight = 100
  )
  expect_true(nrow(resultSP) == 1)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))


  ## SimpleFeature Input #####################
  resultSP <- genetic_algorithm(
    Polygon1 = Polygon1,
    n = 20, iteration = 1,
    vdirspe = vdata,
    Rotor = 35, Proportionality = 1,
    RotorHeight = 100
  )
  expect_true(nrow(resultSP) == 1)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## Data.Frame Input #####################
  PolygonDF <- st_coordinates(Polygon1)
  resultDF <- genetic_algorithm(
    Polygon1 = PolygonDF,
    n = 20, iteration = 1,
    vdirspe = vdata,
    Rotor = 30,
    RotorHeight = 100
  )
  expect_true(nrow(resultDF) == 1)
  expect_true(is.matrix(resultDF))
  expect_false(any(unlist(sapply(resultDF, is.na))))

  ## Matrix Input #####################
  PolygonMat <- as.matrix(PolygonDF[, 1:2])
  resultMA <- genetic_algorithm(
    Polygon1 = PolygonMat, plotit = TRUE,
    n = 20, iteration = 1,
    vdirspe = vdata,
    Rotor = 30,
    RotorHeight = 100
  )
  expect_true(nrow(resultMA) == 1)
  expect_true(is.matrix(resultMA))
  expect_false(any(unlist(sapply(resultMA, is.na))))

  ## Matrix Input - 100% #####################
  resultMA100 <- quiet(genetic_algorithm(
    Polygon1 = PolygonMat,
    verbose = FALSE, plotit = TRUE,
    n = 10, iteration = 20,
    vdirspe = vdata,
    Rotor = 30,
    RotorHeight = 100
  ))
  expect_true(is.matrix(resultMA100))
  expect_false(any(unlist(sapply(resultMA100, is.na))))

  ## Test with non default arguments ####################
  colnames(PolygonMat) <- c("hor", "vert")
  resultMA <- genetic_algorithm(
    Polygon1 = PolygonMat,
    n = 20, iteration = 1, GridMethod = "h",
    vdirspe = vdata, elitism = FALSE,
    selstate = "var", crossPart1 = "ran",
    trimForce = TRUE,
    Rotor = 30,
    RotorHeight = 100
  )
  expect_true(nrow(resultMA) == 1)
  expect_true(is.matrix(resultMA))
  expect_false(any(unlist(sapply(resultMA, is.na))))

  resultMA <- genetic_algorithm(
    Polygon1 = PolygonMat,
    n = 15, iteration = 1, GridMethod = "h",
    vdirspe = vdata, elitism = TRUE, nelit = 10000,
    selstate = "var", crossPart1 = "ran",
    trimForce = TRUE, mutr = 15,
    Rotor = 30,
    Projection = 3035,
    RotorHeight = 100, plotit = FALSE
  )
  expect_true(nrow(resultMA) == 1)
  expect_true(is.matrix(resultMA))
  expect_false(any(unlist(sapply(resultMA[, 1:13], is.na))))

  ## Create errors ####################
  ## RotorHeight is missing
  expect_error(genetic_algorithm(
    Polygon1 = Polygon1,
    GridMethod = "h", plotit = TRUE,
    vdirspe = vdata,
    n = 12,
    elitism = FALSE,
    selstate = "var", crossPart1 = "ran",
    trimForce = TRUE,
    # RotorHeight = 100,
    Rotor = 30
  ))
  ## n is missing
  expect_error(genetic_algorithm(
    Polygon1 = Polygon1,
    GridMethod = "h", plotit = TRUE,
    vdirspe = vdata,
    # n = 12,
    elitism = FALSE,
    selstate = "var", crossPart1 = "ran",
    trimForce = TRUE,
    Rotor = 30,
    RotorHeight = 100
  ))
  ## No winddata
  expect_error(genetic_algorithm(
    Polygon1 = Polygon1,
    GridMethod = "h",
    # vdirspe = vdata,
    n = 12,
    elitism = FALSE,
    selstate = "var", crossPart1 = "ran",
    trimForce = TRUE,
    Rotor = 30,
    RotorHeight = 100
  ))

  ## No Rotor Radius
  expect_error(genetic_algorithm(
    Polygon1 = Polygon1,
    GridMethod = "h",
    vdirspe = vdata,
    n = 12,
    elitism = FALSE,
    selstate = "var", crossPart1 = "ran",
    trimForce = TRUE,
    # Rotor = 30,
    RotorHeight = 100
  ))

  ## Cannot download SRTM (Wrong Polygon)
  wrong_poly <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(1, 1, 2000, 2000, 1),
      c(1, 2000, 2000, 1, 1)
    ))),
    crs = 3035
  ))
  expect_error(expect_warning(
    genetic_algorithm(
      Polygon1 = wrong_poly,
      n = 12, iteration = 1, plotit = TRUE,
      vdirspe = vdata,
      Rotor = 30,
      RotorHeight = 100, topograp = TRUE, verbose = TRUE
    )
  ))
})
