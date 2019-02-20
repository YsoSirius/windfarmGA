context("Test Random Search")
library(testthat)
library(sp)

test_that("Test Random Search Functions", {
  skip_on_cran()
  ## Data ##############
  Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
  Polygon1 <- Polygons(list(Polygon1), 1)
  Polygon1 <- SpatialPolygons(list(Polygon1))
  Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  proj4string(Polygon1) <- CRS(Projection)
  data.in <- data.frame(ws = 12, wd = 0)

  resultSP <- genAlgo(Polygon1 = Polygon1,
                      n = 20, iteration = 4,
                      vdirspe = data.in,
                      Rotor = 35, Proportionality = 1,
                      RotorHeight = 100)

  new <- RandomSearch(resultSP, Polygon1, n = 20, best = 3, Plot = FALSE)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] < 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  new <- RandomSearch(resultSP, Polygon1, Plot = TRUE)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] < 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  new <- RandomSearch(resultSP, Polygon1, best = 10000)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] < 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  ## Test Plots with Hexagons
  new <- RandomSearch(resultSP, GridMethod = "h", Polygon1, n = 10, best = 1)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] < 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  ## Plots
  res = RandomSearchPlot(resultRS = new, result = resultSP,
                   Polygon1 = Polygon1, best = 2)
  expect_true(is.null(res))

  ## TODO - error when n = 1
  new <- RandomSearch(resultSP, GridMethod = "h", Polygon1, n = 2, best = 1)
  res = RandomSearchPlot(resultRS = new, result = resultSP,
                         Polygon1 = Polygon1, best = 100)
  expect_true(is.null(res))
  
  ## Cannot test, as User input is required.
  ## TODO If not enough results, then it errors
  # Res = RandomSearchTurb(result = resultSP, Polygon1 = Polygon1, n = 10)
  # RandomSearchPlot(resultRS = Res, result = resultSP,
                   # Polygon1 = Polygon1, best = 2)

})