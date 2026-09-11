
## Function to suppress print/cat outputs
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

test_that("Test Genetic Algorithm with different Inputs", {

  skip_on_os("mac", arch = "aarch64")

  ## Data ##############
  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  vdata <- data.frame(ws = 12, wd = 0)

  ## All Green ################
  resultSP <- genetic_algorithm(
    area = area,
    n = 5, iteration = 30,
    wind = vdata,
    rotor = 35, proportionality = 1,
    rotor_height = 100, verbose = FALSE,
    plot = TRUE
  )
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## Replace Park with highest Fitness level ################
  resultSP <- suppressMessages(
    genetic_algorithm(
    area = area,
    n = 16, iteration = 100,
    wind = vdata,
    rotor = 35, proportionality = 1,
    rotor_height = 100, verbose = TRUE
  ))
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## No optimization possible - Turbines in all Grid Cells ################
  resultSP <- genetic_algorithm(
    area = area,
    n = 5, iteration = 30,
    wind = vdata,
    rotor = 71, proportionality = 1,
    rotor_height = 100, verbose = FALSE,
    plot = TRUE
  )
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## SF Polygon Input - 30 Iteration #####################
  resultSP <- quiet(genetic_algorithm(
    area = area,
    n = 20, iteration = 30,
    wind = vdata,
    rotor = 35, proportionality = 1,
    rotor_height = 100, verbose = TRUE
  ))
  expect_true(nrow(resultSP) == 30)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## Multi Polygon ###########################
  resultSP <- quiet(genetic_algorithm(
    area = multi_shape,
    n = 20, iteration = 3,
    wind = vdata,
    rotor = 35, proportionality = 1,
    rotor_height = 100, plot = TRUE
  ))
  expect_true(nrow(resultSP) == 3)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  resultSP <- quiet(genetic_algorithm(
    area = multi_shape,
    n = 20, iteration = 3, grid_method = "h",
    wind = vdata,
    rotor = 35, proportionality = 1,
    rotor_height = 100, plot = TRUE
  ))
  expect_true(nrow(resultSP) == 3)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## Hole Polygon ###########################
  resultSP <- quiet(genetic_algorithm(
    area = hole_shape,
    n = 20, iteration = 3,
    wind = vdata,
    rotor = 35, proportionality = 1,
    rotor_height = 100, plot = TRUE
  ))
  expect_true(nrow(resultSP) == 3)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  resultSP <- quiet(genetic_algorithm(
    area = hole_shape,
    n = 20, iteration = 3, grid_method = "h",
    wind = vdata,
    rotor = 35, proportionality = 1,
    rotor_height = 100, plot = TRUE
  ))
  expect_true(nrow(resultSP) == 3)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## SpatialPolygon Input #####################
  PolygonSP <- as(area, "Spatial")
  resultSP <- genetic_algorithm(
    area = PolygonSP,
    n = 20, iteration = 1,
    wind = vdata,
    rotor = 35, proportionality = 1,
    rotor_height = 100
  )
  expect_true(nrow(resultSP) == 1)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))


  ## SimpleFeature Input #####################
  resultSP <- genetic_algorithm(
    area = area,
    n = 20, iteration = 1,
    wind = vdata,
    rotor = 35, proportionality = 1,
    rotor_height = 100
  )
  expect_true(nrow(resultSP) == 1)
  expect_true(is.matrix(resultSP))
  expect_false(any(unlist(sapply(resultSP, is.na))))

  ## Data.Frame Input #####################
  PolygonDF <- st_coordinates(area)
  resultDF <- genetic_algorithm(
    area = PolygonDF,
    n = 20, iteration = 1,
    wind = vdata,
    rotor = 30,
    rotor_height = 100
  )
  expect_true(nrow(resultDF) == 1)
  expect_true(is.matrix(resultDF))
  expect_false(any(unlist(sapply(resultDF, is.na))))

  ## Matrix Input #####################
  PolygonMat <- as.matrix(PolygonDF[, 1:2])
  resultMA <- genetic_algorithm(
    area = PolygonMat, plot = TRUE,
    n = 20, iteration = 1,
    wind = vdata,
    rotor = 30,
    rotor_height = 100
  )
  expect_true(nrow(resultMA) == 1)
  expect_true(is.matrix(resultMA))
  expect_false(any(unlist(sapply(resultMA, is.na))))

  ## Matrix Input - 100% #####################
  resultMA100 <- quiet(genetic_algorithm(
    area = PolygonMat,
    verbose = FALSE, plot = TRUE,
    n = 10, iteration = 20,
    wind = vdata,
    rotor = 30,
    rotor_height = 100
  ))
  expect_true(is.matrix(resultMA100))
  expect_false(any(unlist(sapply(resultMA100, is.na))))

  ## Test with non default arguments ####################
  colnames(PolygonMat) <- c("hor", "vert")
  resultMA <- genetic_algorithm(
    area = PolygonMat,
    n = 20, iteration = 1, grid_method = "h",
    wind = vdata, elitism = FALSE,
    selection_mode = "var",     rotor = 30,
    rotor_height = 100
  )
  expect_true(nrow(resultMA) == 1)
  expect_true(is.matrix(resultMA))
  expect_false(any(unlist(sapply(resultMA, is.na))))

  resultMA <- genetic_algorithm(
    area = PolygonMat,
    n = 15, iteration = 1, grid_method = "h",
    wind = vdata, elitism = TRUE, n_elite = 10000,
    selection_mode = "var", mutation_rate = 15,
    rotor = 30,
    crs = 3035,
    rotor_height = 100, plot = FALSE
  )
  expect_true(nrow(resultMA) == 1)
  expect_true(is.matrix(resultMA))
  expect_false(any(unlist(sapply(resultMA[, 1:13], is.na))))

  ## Create errors ####################
  ## rotor_height is missing
  expect_error(genetic_algorithm(
    area = area,
    grid_method = "h", plot = TRUE,
    wind = vdata,
    n = 12,
    elitism = FALSE,
    selection_mode = "var",     # rotor_height = 100,
    rotor = 30
  ))
  ## n is missing
  expect_error(genetic_algorithm(
    area = area,
    grid_method = "h", plot = TRUE,
    wind = vdata,
    # n = 12,
    elitism = FALSE,
    selection_mode = "var",     rotor = 30,
    rotor_height = 100
  ))
  ## No winddata
  expect_error(genetic_algorithm(
    area = area,
    grid_method = "h",
    # wind = vdata,
    n = 12,
    elitism = FALSE,
    selection_mode = "var",     rotor = 30,
    rotor_height = 100
  ))

  ## No Rotor Radius
  expect_error(genetic_algorithm(
    area = area,
    grid_method = "h",
    wind = vdata,
    n = 12,
    elitism = FALSE,
    selection_mode = "var",     # rotor = 30,
    rotor_height = 100
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
      area = wrong_poly,
      n = 12, iteration = 1, plot = TRUE,
      wind = vdata,
      rotor = 30,
      rotor_height = 100, terrain = TRUE, verbose = TRUE
    )
  ))
})
