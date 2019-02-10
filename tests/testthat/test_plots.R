context("Plots")

library(testthat)
library(windfarmGA)
library(sp)

## Function to suppress print/cat outputs
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

test_that("Test Plotting Functions", {
  ## Windrose Plotting #############
  
  wind_test <- data.frame(ws = runif(10, 10, 20), 
                        wd = runif(10, 0, 360) )
  a0 = plotWindrose(wind_test, plotit = FALSE)
  a1 = plotWindrose(wind_test, "ws", "wd", plotit = FALSE)
  a2 = plotWindrose(wind_test, 1, 2, plotit = FALSE)
  a3 = plotWindrose(spd = wind_test$ws, dir = wind_test$wd, plotit = FALSE)
  expect_true(all(c(all.equal(a0$data, a1$data), all.equal(a0$data, a2$data))))
  expect_true(all.equal(a0$data, a3$data, check.attributes = FALSE))

  wind_test <- data.frame(speed = c(12, 30, 45), 
                        direction = c(0, 90, 150) )
  b0 = plotWindrose(wind_test, plotit = FALSE)
  b1 = plotWindrose(wind_test, "speed", dir = "direction", plotit = FALSE)
  b2 = plotWindrose(wind_test, 1, 2, plotit = FALSE)
  b3 = plotWindrose(spd = wind_test$speed, dir = wind_test$direction,
                    plotit = FALSE)
  expect_true(all(c(all.equal(b0$data, b1$data), all.equal(b0$data, b2$data))))
  expect_true(all.equal(b0$data, b3$data, 
                        check.attributes = FALSE))
  
  wind_test <- data.frame(direction = c(0, 90, 150),
                        speed = c(12, 30, 45),
                        id = 1:3,
                        probab = 30:32)
  b0 = plotWindrose(wind_test, plotit = FALSE)
  b1 = plotWindrose(wind_test, "speed", dir = "direction", plotit = FALSE)
  b2 = plotWindrose(wind_test, 1, 2, spdres = 5, spdseq = seq(1,40,5),
                    plotit = FALSE)
  b3 = plotWindrose(spd = wind_test$speed, dir = wind_test$direction,
                    plotit = FALSE)
  expect_true(all(c(all.equal(b0$data, b1$data), all.equal(b0$data, b2$data))))
  expect_true(all.equal(b0$data$direction, b3$data$dir, 
                        check.attributes = FALSE))
  expect_true(all.equal(b0$data$speed, b3$data$spd, 
                        check.attributes = FALSE))
  
  wind_test <- data.frame(richt = c(0, 90, 150),
                        gesch = c(12, 30, 45),
                        id = 1:3,
                        probab = 30:32)
  c0 = plotWindrose(wind_test, plotit = FALSE)
  c1 = plotWindrose(wind_test, "gesch", "richt", plotit = FALSE)
  c2 = plotWindrose(wind_test, 1, 2, plotit = FALSE)
  c3 = plotWindrose(spd = wind_test$gesch, dir = wind_test$richt,
                    palette = "Set3", plotit = FALSE)
  expect_true(all(c(all.equal(c0$data, c1$data), all.equal(c0$data, c2$data))))
  expect_true(all.equal(c0$data$richt, b3$data$dir, check.attributes = FALSE))
  expect_true(all.equal(c0$data$gesch, b3$data$spd, check.attributes = FALSE))
  

  ## GenAlgo plotting functions #############
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
                        n = 12, iteration = 6,
                        vdirspe = winddat,
                        Rotor = 30, 
                        RotorHeight = 100)

  plot_res <- quiet(plotResult(resultrect, Polygon1 = sp_polygon))
  expect_false(anyNA(plot_res))

  cloud_res <- plotCloud(resultrect, pl = FALSE)
  expect_true(class(cloud_res) == "matrix")
  expect_false(anyNA(cloud_res))
  expect_true(ncol(cloud_res) == 15)

  beor_res <- plotbeorwor(resultrect)
  expect_true(is.null(beor_res))

  fitnes_res <- plotfitnessevolution(resultrect)
  expect_true(is.null(fitnes_res))
  
  heat_res <- heatmapGA(resultrect)
  expect_true(class(heat_res)[1] == "gg")
  
  heat_res <- heatmapGA(resultrect, idistw = 2)
  expect_true(class(heat_res)[1] == "gg")

  heat_res <- heatmapGA(resultrect, idistw = 50, si = 5)
  expect_true(class(heat_res)[1] == "gg")
  
  evo_res <- plotEvolution(resultrect, ask = FALSE)
  expect_true(is.null(evo_res))
  
  windr_res <- plotWindrose(winddat, "ws", "wd")
  expect_true(class(windr_res)[1] == "gg")
})