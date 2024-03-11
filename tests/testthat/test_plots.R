context("Plots")

test_that("Test Plotting Functions", {
  library(terra)
  library(stars)

  ## Function to suppress print/cat outputs
  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }

  wind_test <- data.frame(
    x = runif(10, 10, 20),
    y = runif(10, 0, 360)
  )

  ## Windrose Plotting #############
  ## Mock Packages not installed
  with_mock(
    is_ggplot2_installed = function() FALSE,
    expect_error(
      plot_windrose(wind_test, plotit = FALSE)
    )
  )
  a0 <- plot_windrose(wind_test, plotit = FALSE)
  expect_true(is.recursive(a0))

  wind_test <- data.frame(
    ws = runif(10, 10, 20),
    wd = runif(10, 0, 360)
  )
  a0 <- plot_windrose(wind_test, plotit = FALSE)
  a0 <- plot_windrose(wind_test, plotit = FALSE)
  a1 <- plot_windrose(wind_test, "ws", "wd", plotit = FALSE)
  a2 <- plot_windrose(wind_test, 1, 2, plotit = FALSE)
  a3 <- plot_windrose(spd = wind_test$ws, dir = wind_test$wd, plotit = FALSE)
  expect_true(all(c(all.equal(a0$data, a1$data), all.equal(a0$data, a2$data))))
  expect_true(all.equal(a0$data, a3$data, check.attributes = FALSE))

  wind_test <- data.frame(
    speed = c(12, 30, 45),
    direction = c(0, 90, 150)
  )
  b0 <- plot_windrose(wind_test, plotit = FALSE)
  b1 <- plot_windrose(wind_test, "speed", dir = "direction", plotit = FALSE)
  b2 <- plot_windrose(wind_test, 1, 2, plotit = FALSE)
  b3 <- plot_windrose(
    spd = wind_test$speed, dir = wind_test$direction,
    plotit = FALSE
  )
  expect_true(all(c(all.equal(b0$data, b1$data), all.equal(b0$data, b2$data))))
  expect_true(all.equal(b0$data, b3$data,
    check.attributes = FALSE
  ))

  wind_test <- data.frame(
    direction = c(0, 90, 150),
    speed = c(12, 30, 45),
    id = 1:3,
    probab = 30:32
  )
  b0 <- plot_windrose(wind_test, plotit = FALSE)
  b1 <- plot_windrose(wind_test, "speed", dir = "direction", plotit = FALSE)
  b2 <- plot_windrose(wind_test, 1, 2,
    spdres = 5, spdseq = seq(1, 40, 5),
    plotit = FALSE
  )
  b3 <- plot_windrose(
    spd = wind_test$speed, dir = wind_test$direction,
    plotit = FALSE
  )
  expect_true(all(c(all.equal(b0$data, b1$data), all.equal(b0$data, b2$data))))
  expect_true(all.equal(b0$data$direction, b3$data$dir,
    check.attributes = FALSE
  ))
  expect_true(all.equal(b0$data$speed, b3$data$spd,
    check.attributes = FALSE
  ))

  wind_test <- data.frame(
    richt = c(0, 90, 150),
    gesch = c(12, 30, 45),
    id = 1:3,
    probab = 30:32
  )
  c0 <- plot_windrose(wind_test, plotit = FALSE)
  c1 <- plot_windrose(wind_test, "gesch", "richt", plotit = FALSE)
  c2 <- plot_windrose(wind_test, 1, 2, plotit = FALSE)
  c3 <- plot_windrose(
    spd = wind_test$gesch, dir = wind_test$richt,
    palette = "Set3", plotit = FALSE
  )
  expect_true(all(c(all.equal(c0$data, c1$data), all.equal(c0$data, c2$data))))
  expect_true(all.equal(c0$data$richt, b3$data$dir, check.attributes = FALSE))
  expect_true(all.equal(c0$data$gesch, b3$data$spd, check.attributes = FALSE))


  wind_test <- data.frame(
    gesch = c(12, 30, 45),
    richt = c(0, 90, 150),
    id = 1:3,
    probab = 30:32
  )
  colnames(wind_test) <- NULL
  c4 <- plot_windrose(wind_test, plotit = FALSE)
  expect_true(identical(c4$data[, "spd"], c0$data[, "gesch"]))

  wind_test <- data.frame(
    blabla = c(12, 30, 45),
    blablaa = c(0, 90, 150),
    id = 1:3,
    somegting = 30:32
  )
  colnames(wind_test) <- NULL
  c5 <- plot_windrose(wind_test, plotit = FALSE)
  expect_true(all.equal(c4$data, c5$data))

  winddat <- data.frame(
    ws = 12,
    wd = 0
  )
  windr_res <- plot_windrose(winddat, "ws", "wd")
  expect_true(class(windr_res)[1] == "gg")


  ## plot_parkfitness ###############
  respf <- plot_parkfitness(resultrect)
  expect_true(is.null(respf))

  ## plot_result ###############
  sp_polygonnp <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  plot_res <- quiet(plot_result(resultrect[1:10, ],
    Polygon1 = sp_polygonnp, best = 5000,
    plotEn = 1
  ))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))
  plot_res <- quiet(plot_result(resultrect[1:10, ],
    Polygon1 = sp_polygonnp,
    best = 5000, plotEn = 2
  ))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))

  polywgs84 <- sp_polygonnp
  polywgs84 <- st_transform(polywgs84, 4326)
  st_crs(polywgs84) <- NA
  plot_res <- quiet(plot_result(resultrect[1:10, ],
    Polygon1 = polywgs84
  ))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))

  ## Create a result with 100% Efficiency (to plot all green)
  resultrect100 <- genetic_algorithm(
    Polygon1 = sp_polygon,
    n = 5, iteration = 60,
    vdirspe = winddat,
    Rotor = 30,
    RotorHeight = 100
  )
  plot_res <- quiet(plot_result(resultrect100,
    Polygon1 = sp_polygonnp,
    best = 5000, plotEn = 1
  ))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))
  plot_res <- quiet(plot_result(resultrect100,
    Polygon1 = sp_polygonnp,
    best = 5000, plotEn = 2
  ))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))

  plot_res <- quiet(plot_result(resultrect,
    Polygon1 = sp_polygon
  ))
  expect_false(anyNA(plot_res))

  plot_res <- quiet(plot_result(resultrect,
    best = 5,
    Polygon1 = sp_polygon
  ))
  expect_false(anyNA(plot_res))

  Grid <- grid_area(st_as_sf(sp_polygon), size = 150)
  plot_res <- quiet(plot_result(resultrect,
    best = 5,
    Polygon1 = sp_polygon,
    Grid = Grid[[2]]
  ))
  expect_false(anyNA(plot_res))

  plot_res <- quiet(plot_result(resultrect,
    Polygon1 = sp_polygon,
    plotEn = 2,
    Grid = Grid[[2]]
  ))
  expect_false(anyNA(plot_res))

  expect_error(quiet(plot_result(resultrect,
    Polygon1 = sp_polygon,
    plotEn = 3
  )))

  ## plot_windfarmGA ###############
  respwf <- plot_windfarmGA(resultrect, sp_polygon,
    whichPl = "all",
    best = 1, plotEn = 1
  )
  expect_true(is.null(respwf))
  respwf <- plot_windfarmGA(resultrect[1:3, ], sp_polygon,
    whichPl = "all",
    best = 1, plotEn = 1
  )
  expect_true(is.null(respwf))
  respwf <- plot_windfarmGA(resultrect[1:3, ], sp_polygon,
    whichPl = "all",
    best = 1, plotEn = 1
  )
  expect_true(is.null(respwf))

  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  respwf <- plot_windfarmGA(resulthex, Polygon1,
    whichPl = "all",
    best = 2, plotEn = 1
  )
  expect_true(is.null(respwf))


  ## plot_cloud ###############
  cloud_res <- plot_cloud(resultrect, pl = FALSE)
  expect_false(anyNA(cloud_res))
  expect_true(ncol(cloud_res) == 15)

  cloud_res <- plot_cloud(resultrect, pl = TRUE)
  expect_false(anyNA(cloud_res))
  expect_true(ncol(cloud_res) == 15)

  ## plot_development ###############
  beor_res <- plot_development(resultrect)
  expect_true(is.null(beor_res))

  ## plot_fitness_evolution ###############
  fitnes_res <- plot_fitness_evolution(resultrect)
  expect_true(is.null(fitnes_res))

  ## plot_evolution ###############
  evo_res <- plot_evolution(resultrect, ask = FALSE)
  expect_true(is.null(evo_res))

  ## plot_leaflet #######################
  skip_if("4.1.0", compareVersion(paste0(R.version$major,".",R.version$minor)) != 0,
          "Skip as the version is <= 4.1.0. Errors in test_plots unresolved (leaflet::addMarkers(...))")
  ## Mock Packages not installed
  with_mock(
    is_leaflet_installed = function() FALSE,
    expect_error(
      plot_leaflet(result = resultrect, Polygon1 = sp_polygon, which = 1)
    )
  )

  vdata <- data.frame(ws = 12, wd = 0)
  resultSP <- genetic_algorithm(
    Polygon1 = Polygon1,
    n = 5, iteration = 3,
    vdirspe = vdata, Rotor = 35,
    RotorHeight = 100
  )
  p <- plot_leaflet(resultSP, Polygon1 = Polygon1, which = 1)
  expect_is(p, "leaflet")

  ## Plot the best wind farm on a leaflet map (ordered by energy values)
  p <- plot_leaflet(result = resultrect, Polygon1 = sp_polygon, which = 1)
  expect_is(p, "leaflet")

  ## Plot the best wind farm on a leaflet map (ordered by energy values)
  p <- plot_leaflet(result = resulthex, Polygon1 = sp_polygon, which = 1)
  expect_is(p, "leaflet")

  ## Plot the last wind farm (ordered by chronology).
  p <- plot_leaflet(
    result = resulthex, Polygon1 = sp_polygon, orderitems = FALSE,
    which = 1
  )
  expect_is(p, "leaflet")

  ## Plot the best wind farm on a leaflet map with the rectangular Grid
  Grid <- grid_area(sp_polygon, size = 150, prop = 0.4)
  p <- plot_leaflet(
    result = resultrect, Polygon1 = sp_polygon, which = 1,
    GridPol = Grid[[2]]
  )
  expect_is(p, "leaflet")

  ## Plot the last wind farm with hexagonal Grid
  Grid <- hexa_area(sp_polygon, size = 75)
  p <- plot_leaflet(
    result = resulthex, Polygon1 = sp_polygon, which = 1,
    GridPol = Grid[[2]]
  )
  expect_is(p, "leaflet")

  p <- plot_leaflet(result = resultrect, Polygon1 = sp_polygon, which = 1)
  expect_is(p, "leaflet")

  gr <- grid_area(st_as_sf(sp_polygon), size = 220)
  spnop <- gr[[2]]
  st_crs(spnop) <- NA
  p <- plot_leaflet(result = resultrect, Polygon1 = sp_polygon, GridPol = spnop)
  expect_is(p, "leaflet")

  p <- plot_leaflet(result = resulthex, Polygon1 = sp_polygon, which = 1, orderitems = FALSE)
  expect_is(p, "leaflet")

  p <- plot_leaflet(result = resulthex, Polygon1 = sp_polygon, which = 1000, orderitems = FALSE)
  expect_is(p, "leaflet")


  ## No Projection
  poly_nocrs <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    )))
  ))

  p <- plot_leaflet(result = resultrect, Polygon1 = poly_nocrs)
  expect_is(p, "leaflet")

  expect_error(genetic_algorithm(
    Polygon1 = poly_nocrs,
    n = 12, iteration = 60,
    vdirspe = winddat,
    Rotor = 30,
    RotorHeight = 100
  ))
})
