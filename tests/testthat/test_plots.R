context("Plots")

library(testthat)
library(windfarmGA)
library(sp)

test_that("Test Plotting Functions", {
  ## Test some Plotting Functions
  sp_polygon <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
                            c(4499991, 2669343), c(4499991, 2668272)))
  sp_polygon <- Polygons(list(sp_polygon), 1);
  sp_polygon <- SpatialPolygons(list(sp_polygon))
  projection <- paste("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000",
                "+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

  proj4string(sp_polygon) <- CRS(projection)

  winddat <- data.frame(ws = 12,
                        wd = 0)

  resultrect <- genAlgo(Polygon1 = sp_polygon,
                        n = 12, iteration = 10,
                        vdirspe = winddat,
                        Rotor = 30, 
                        RotorHeight = 100)

  plot_res <- plotResult(resultrect, Polygon1 = sp_polygon)
  expect_false(anyNA(plot_res))

  cloud_res <- plotCloud(resultrect, pl = FALSE)
  expect_true(class(cloud_res) == "matrix")
  expect_false(anyNA(cloud_res))
  expect_true(ncol(cloud_res) == 15)

  beor_res <- plotbeorwor(resultrect)
  expect_true(is.null(beor_res))

  fitev_res <- plotfitnessevolution(resultrect)
  expect_true(is.null(fitev_res))

  expect_true(is.null(fitnes_res))
  
  heat_res <- heatmapGA(resultrect)
  expect_true(class(heat_res)[1] == "gg")

  evo_res <- plotEvolution(resultrect, ask = FALSE)
  expect_true(is.null(evo_res))
  
  windr_res <- plotWindrose(winddat, "ws", "wd")
  expect_true(class(windr_res)[1] == "gg")
})