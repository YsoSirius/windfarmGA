context("Plots")
library(testthat)
library(windfarmGA)
library(sp)
library(raster)

## Function to suppress print/cat outputs
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

test_that("Test Plotting Functions", {
  skip_on_cran()
  
  ## Windrose Plotting #############
  wind_test <- data.frame(x = runif(10, 10, 20), 
                          y = runif(10, 0, 360) )
  a0 <- plot_windrose(wind_test, plotit = FALSE)
  expect_true(is.recursive(a0))
  
  wind_test <- data.frame(ws = runif(10, 10, 20), 
                        wd = runif(10, 0, 360) )
  a0 <- plot_windrose(wind_test, plotit = FALSE)
  a0 <- plotWindrose(wind_test, plotit = FALSE)
  a1 <- plotWindrose(wind_test, "ws", "wd", plotit = FALSE)
  a2 <- plotWindrose(wind_test, 1, 2, plotit = FALSE)
  a3 <- plotWindrose(spd = wind_test$ws, dir = wind_test$wd, plotit = FALSE)
  expect_true(all(c(all.equal(a0$data, a1$data), all.equal(a0$data, a2$data))))
  expect_true(all.equal(a0$data, a3$data, check.attributes = FALSE))

  wind_test <- data.frame(speed = c(12, 30, 45), 
                        direction = c(0, 90, 150) )
  b0 <- plotWindrose(wind_test, plotit = FALSE)
  b1 <- plotWindrose(wind_test, "speed", dir = "direction", plotit = FALSE)
  b2 <- plotWindrose(wind_test, 1, 2, plotit = FALSE)
  b3 <- plotWindrose(spd = wind_test$speed, dir = wind_test$direction,
                    plotit = FALSE)
  expect_true(all(c(all.equal(b0$data, b1$data), all.equal(b0$data, b2$data))))
  expect_true(all.equal(b0$data, b3$data, 
                        check.attributes = FALSE))

  wind_test <- data.frame(direction = c(0, 90, 150),
                        speed = c(12, 30, 45),
                        id = 1:3,
                        probab = 30:32)
  b0 <- plotWindrose(wind_test, plotit = FALSE)
  b1 <- plotWindrose(wind_test, "speed", dir = "direction", plotit = FALSE)
  b2 <- plotWindrose(wind_test, 1, 2, spdres = 5, spdseq = seq(1,40,5),
                    plotit = FALSE)
  b3 <- plotWindrose(spd = wind_test$speed, dir = wind_test$direction,
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
  c0 <- plotWindrose(wind_test, plotit = FALSE)
  c1 <- plotWindrose(wind_test, "gesch", "richt", plotit = FALSE)
  c2 <- plotWindrose(wind_test, 1, 2, plotit = FALSE)
  c3 <- plotWindrose(spd = wind_test$gesch, dir = wind_test$richt,
                    palette = "Set3", plotit = FALSE)
  expect_true(all(c(all.equal(c0$data, c1$data), all.equal(c0$data, c2$data))))
  expect_true(all.equal(c0$data$richt, b3$data$dir, check.attributes = FALSE))
  expect_true(all.equal(c0$data$gesch, b3$data$spd, check.attributes = FALSE))
  
  
  wind_test <- data.frame(gesch = c(12, 30, 45),
                          richt = c(0, 90, 150),
                          id = 1:3,
                          probab = 30:32)
  colnames(wind_test) <- NULL
  c4 <- plotWindrose(wind_test, plotit = FALSE)
  expect_true(identical(c4$data[,"spd"], c0$data[,"gesch"]))

  wind_test <- data.frame(blabla = c(12, 30, 45),
                          blablaa = c(0, 90, 150),
                          id = 1:3,
                          somegting = 30:32)
  colnames(wind_test) <- NULL
  c5 <- plotWindrose(wind_test, plotit = FALSE)
  expect_true(all.equal(c4$data, c5$data))

  winddat <- data.frame(ws = 12,
                        wd = 0)
  windr_res <- plotWindrose(winddat, "ws", "wd")
  expect_true(class(windr_res)[1] == "gg")
  

  ## plot_parkfitness ###############
  respf <- plotparkfitness(resultrect)
  expect_true(is.null(respf))
  
  ## plot_result ###############
  sp_polygonnp <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
                              c(4499991, 2669343), c(4499991, 2668272)))
  sp_polygonnp <- Polygons(list(sp_polygonnp), 1)
  sp_polygonnp <- SpatialPolygons(list(sp_polygonnp))
  plot_res <- quiet(plot_result(resultrect[1:10,], Polygon1 = sp_polygonnp, best = 5000, 
                                plotEn = 1))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))
  plot_res <- quiet(plot_result(resultrect[1:10,], Polygon1 = sp_polygonnp, best = 5000, plotEn = 2))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))
  
  ## Create a result with 100% Efficiency (to plot all green)
  resultrect100 <- genAlgo(Polygon1 = sp_polygon,
                        n = 5, iteration = 60,
                        vdirspe = winddat,
                        Rotor = 30, 
                        RotorHeight = 100)
  plot_res <- quiet(plot_result(resultrect100, Polygon1 = sp_polygonnp, best = 5000, plotEn = 1))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))
  plot_res <- quiet(plot_result(resultrect100, Polygon1 = sp_polygonnp, best = 5000, plotEn = 2))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))
  
  plot_res <- quiet(plotResult(resultrect, Polygon1 = sp_polygon))
  expect_false(anyNA(plot_res))
  
  plot_res <- quiet(plotResult(resultrect, best = 5, Polygon1 = sp_polygon))
  expect_false(anyNA(plot_res))
  
  Grid <- GridFilter(sp_polygon, resol = 150)
  plot_res <- quiet(plotResult(resultrect, best = 5, Polygon1 = sp_polygon, 
                               Grid = Grid[[2]]))
  expect_false(anyNA(plot_res))
  
  plot_res <- quiet(plotResult(resultrect, Polygon1 = sp_polygon, plotEn = 2, 
                               Grid = Grid[[2]]))
  expect_false(anyNA(plot_res))
  
  projection <- paste("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000",
                      "+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  plot_res <- quiet(plotResult(resultrect, best = 5, Polygon1 = sp_polygon, 
                               Projection = projection))
  expect_false(anyNA(plot_res))

  expect_error(quiet(plotResult(resultrect, Polygon1 = sp_polygon, plotEn = 3)))

  ## plot_windfarmGA ###############
  respwf <- plot_windfarmGA(resultrect, GridMethod = "r", sp_polygon, whichPl = "all",
                            best = 1, plotEn = 1)
  expect_true(is.null(respwf))
  respwf <- plot_windfarmGA(resultrect[1:3,], GridMethod = "r", sp_polygon, whichPl = "all",
                            best = 1, plotEn = 1)
  expect_true(is.null(respwf))
  respwf <- PlotWindfarmGA(resultrect[1:3,], GridMethod = "r", sp_polygon, whichPl = "all",
                            best = 1, plotEn = 1)
  expect_true(is.null(respwf))
  
  load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
  load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
  respwf <- plot_windfarmGA(resulthex, GridMethod = "h", polygon, whichPl = "all",
                  best = 2, plotEn = 1)
  expect_true(is.null(respwf))
  
  
  ## plot_cloud ###############
  cloud_res <- plotCloud(resultrect, pl = FALSE)
  # expect_true(class(cloud_res) == "matrix")
  expect_false(anyNA(cloud_res))
  expect_true(ncol(cloud_res) == 15)

  cloud_res <- plotCloud(resultrect, pl = TRUE)
  # expect_true(class(cloud_res) == "matrix")
  expect_false(anyNA(cloud_res))
  expect_true(ncol(cloud_res) == 15)
  
  ## plot_development ###############
  beor_res <- plotbeorwor(resultrect)
  expect_true(is.null(beor_res))

  ## plot_fitness_evolution ###############
  fitnes_res <- plotfitnessevolution(resultrect)
  expect_true(is.null(fitnes_res))
  
  ## plot_heatmap ###############
  heat_res <- heatmapGA(resultrect)
  expect_true(class(heat_res) == "list")
  expect_false(anyNA(heat_res[[2]]))
  expect_false(anyNA(heat_res[[1]][, 1:3]))
  
  heat_res <- heatmapGA(resultrect, idistw = 2)
  expect_true(class(heat_res) == "list")
  expect_false(anyNA(heat_res[[2]]))
  expect_false(anyNA(heat_res[[1]][, 1:3]))

  heat_res <- heatmapGA(resultrect, idistw = 50, si = 5)
  expect_true(class(heat_res) == "list")
  expect_false(anyNA(heat_res[[2]]))
  expect_false(anyNA(heat_res[[1]][, 1:3]))
  
  ## plot_evolution ###############
  evo_res <- plotEvolution(resultrect, ask = FALSE)
  expect_true(is.null(evo_res))
  
  
  ## plot_leaflet #######################
  pl <- leafPlot(result = resultrect, Polygon1 = sp_polygon, which = 1)
  expect_true(is.recursive(pl)); rm(pl)
  
  gr <- GridFilter(sp_polygon, resol = 220)
  spnop <- gr[[2]]
  raster::projection(spnop) <- NA
  pl <- leafPlot(result = resultrect, Polygon1 = sp_polygon, GridPol = spnop)
  expect_true(is.recursive(pl));   rm(pl)
  
  pl <- leafPlot(result = resulthex, Polygon1 = polygon, which = 1)
  expect_true(is.recursive(pl));rm(pl)
  pl <- leafPlot(result = resulthex, Polygon1 = polygon, which = 1, orderitems = FALSE)
  expect_true(is.recursive(pl));rm(pl)
  pl <- leafPlot(result = resulthex, Polygon1 = polygon, which = 1000, orderitems = FALSE)
  expect_true(is.recursive(pl));rm(pl)
  gr <- GridFilter(polygon, resol = 250)
  pl <- leafPlot(result = resulthex, Polygon1 = polygon, GridPol = gr[[2]])
  expect_true(is.recursive(pl));rm(pl)

  # sp_polygon <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
  #                             c(4499991, 2669343), c(4499991, 2668272)))
  # sp_polygon <- Polygons(list(sp_polygon), 1)
  # sp_polygon <- SpatialPolygons(list(sp_polygon))
  # projection <- paste("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000",
  #                     "+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # proj4string(sp_polygon) <- CRS(projection)
  # winddat <- data.frame(ws = 12, wd = 0)
  # resultrect <- genAlgo(Polygon1 = sp_polygon,
  #                       n = 12, iteration = 60,
  #                       vdirspe = winddat,
  #                       Rotor = 30,
  #                       RotorHeight = 100)
  
  ## No Projection
  sp_polygon <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
                              c(4499991, 2669343), c(4499991, 2668272)))
  sp_polygon <- Polygons(list(sp_polygon), 1)
  sp_polygon <- SpatialPolygons(list(sp_polygon))
  pl <- leafPlot(result = resultrect, Polygon1 = sp_polygon)
  expect_true(is.recursive(pl));rm(pl)
  expect_error(genAlgo(Polygon1 = sp_polygon,
                       n = 12, iteration = 60,
                       vdirspe = winddat,
                       Rotor = 30,
                       RotorHeight = 100))
  
  
})
