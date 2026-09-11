
test_that("Test Plotting Functions", {
  library(terra)
  library(stars)

  skip_on_os("mac", arch = "aarch64")

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
  with_mocked_bindings(
    is_ggplot2_installed = function() FALSE,
    expect_error(
      plot_windrose(wind_test, plot = FALSE)
    )
  )
  a0 <- plot_windrose(wind_test, plot = FALSE)
  expect_true(inherits(a0, c("ggplot", "ggplot2::ggplot")))

  wind_test <- data.frame(
    ws = runif(10, 10, 20),
    wd = runif(10, 0, 360)
  )
  a0 <- plot_windrose(wind_test, plot = FALSE)
  a0 <- plot_windrose(wind_test, plot = FALSE)
  a1 <- plot_windrose(wind_test, "ws", "wd", plot = FALSE)
  a2 <- plot_windrose(wind_test, 1, 2, plot = FALSE)
  a3 <- plot_windrose(spd = wind_test$ws, dir = wind_test$wd, plot = FALSE)
  expect_true(all(c(all.equal(a0$data, a1$data), all.equal(a0$data, a2$data))))
  expect_true(all.equal(a0$data, a3$data, check.attributes = FALSE))

  wind_test <- data.frame(
    speed = c(12, 30, 45),
    direction = c(0, 90, 150)
  )
  b0 <- plot_windrose(wind_test, plot = FALSE)
  b1 <- plot_windrose(wind_test, "speed", dir = "direction", plot = FALSE)
  b2 <- plot_windrose(wind_test, 1, 2, plot = FALSE)
  b3 <- plot_windrose(
    spd = wind_test$speed, dir = wind_test$direction,
    plot = FALSE
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
  b0 <- plot_windrose(wind_test, plot = FALSE)
  b1 <- plot_windrose(wind_test, "speed", dir = "direction", plot = FALSE)
  b2 <- plot_windrose(wind_test, 1, 2,
    spdres = 5, spdseq = seq(1, 40, 5),
    plot = FALSE
  )
  b3 <- plot_windrose(
    spd = wind_test$speed, dir = wind_test$direction,
    plot = FALSE
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
  c0 <- plot_windrose(wind_test, plot = FALSE)
  c1 <- plot_windrose(wind_test, "gesch", "richt", plot = FALSE)
  c2 <- plot_windrose(wind_test, 1, 2, plot = FALSE)
  c3 <- plot_windrose(
    spd = wind_test$gesch, dir = wind_test$richt,
    palette = "Set3", plot = FALSE
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
  c4 <- plot_windrose(wind_test, plot = FALSE)
  expect_true(identical(c4$data[, "spd"], c0$data[, "gesch"]))

  wind_test <- data.frame(
    blabla = c(12, 30, 45),
    blablaa = c(0, 90, 150),
    id = 1:3,
    somegting = 30:32
  )
  colnames(wind_test) <- NULL
  c5 <- plot_windrose(wind_test, plot = FALSE)
  expect_true(all.equal(c4$data, c5$data))

  winddat <- data.frame(
    ws = 12,
    wd = 0
  )
  windr_res <- plot_windrose(winddat, "ws", "wd")
  expect_true(inherits(windr_res, c("ggplot", "ggplot2::ggplot")))


  ## plot_parkfitness ###############
  respf <- plot_parkfitness(resultrect, interactive = FALSE)
  expect_true(is.list(respf) || inherits(respf, c("ggplot", "plotly", "plotly_hash")))
  expect_true(is.list(plot_fitness_evolution(resultrect, interactive = FALSE)))

  result_neg <- resultrect
  sc <- result_neg[[1, "selcross"]]
  sc[1, 1] <- -1
  result_neg[[1, "selcross"]] <- sc
  expect_true(is.list(plot_parkfitness(result_neg, interactive = FALSE)))

  ## plot_generation ###############
  gl <- generation_layouts(resultrect, generation = 2)
  expect_true(is.list(gl))
  expect_true(all(c("turbines", "layouts", "generation") %in% names(gl)))
  expect_true(nrow(gl$layouts) >= 1)
  expect_equal(gl$generation, 2)
  glp <- plot_generation(resultrect, sp_polygon, generation = 2,
                         n_show = 2, interactive = FALSE, ask = FALSE)
  expect_equal(glp$generation, 2)
  expect_true("elite" %in% names(gl$layouts))

  ## plot_population ###############
  cen <- population_census(resultrect)
  expect_true(is.data.frame(cen))
  expect_true(all(c(
    "generation", "evaluated", "distinct", "duplicates",
    "selected", "elites", "cells", "cells_cum", "cells_elite", "elite_kids"
  ) %in% names(cen)))
  expect_equal(nrow(cen), nrow(resultrect))
  expect_true(all(cen$evaluated >= cen$distinct, na.rm = TRUE))
  popp <- plot_population(resultrect, interactive = FALSE, ask = FALSE)
  expect_true(is.list(popp) || inherits(popp, c("ggplot", "plotly", "plotly_hash")))
  expect_true(is.data.frame(attr(popp, "census")))

  ## plot_result ###############
  sp_polygonnp <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  plot_res <- quiet(plot_result(resultrect[1:10, ],
    area = sp_polygonnp, best = 5000,
    plot_en = 1
  ))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))
  plot_res <- quiet(plot_result(resultrect[1:10, ],
    area = sp_polygonnp,
    best = 5000, plot_en = 2
  ))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))

  polywgs84 <- sp_polygonnp
  polywgs84 <- st_transform(polywgs84, 4326)
  st_crs(polywgs84) <- NA
  plot_res <- quiet(plot_result(resultrect[1:10, ],
    area = polywgs84
  ))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))

  ## Create a result with 100% Efficiency (to plot all green)
  resultrect100 <- genetic_algorithm(
    area = sp_polygon,
    n = 5, iteration = 60,
    wind = winddat,
    rotor = 30,
    rotor_height = 100
  )
  plot_res <- quiet(plot_result(resultrect100,
    area = sp_polygonnp,
    best = 5000, plot_en = 1
  ))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))
  plot_res <- quiet(plot_result(resultrect100,
    area = sp_polygonnp,
    best = 5000, plot_en = 2
  ))
  expect_false(anyNA(plot_res))
  expect_true(all(plot_res$EfficAllDir <= 100))

  plot_res <- quiet(plot_result(resultrect,
    area = sp_polygon
  ))
  expect_false(anyNA(plot_res))

  plot_res <- quiet(plot_result(resultrect,
    best = 5,
    area = sp_polygon
  ))
  expect_false(anyNA(plot_res))

  Grid <- grid_area(st_as_sf(sp_polygon), size = 150)
  plot_res <- quiet(plot_result(resultrect,
    best = 5,
    area = sp_polygon,
    plot_grid = Grid[[2]]
  ))
  expect_false(anyNA(plot_res))

  plot_res <- quiet(plot_result(resultrect,
    area = sp_polygon,
    plot_en = 2,
    plot_grid = Grid[[2]]
  ))
  expect_false(anyNA(plot_res))

  expect_error(quiet(plot_result(resultrect,
    area = sp_polygon,
    plot_en = 3
  )))

  ## plot_windfarmGA ###############
  respwf <- plot_windfarmGA(resultrect, sp_polygon,
    which_plot = "all",
    best = 1, plot_en = 1, plotly = FALSE
  )
  expect_true(is.null(respwf))
  respwf <- plot_windfarmGA(resultrect[1:3, ], sp_polygon,
    which_plot = "all",
    best = 1, plot_en = 1, plotly = FALSE
  )
  expect_true(is.null(respwf))
  respwf <- plot_windfarmGA(resultrect[1:3, ], sp_polygon,
    which_plot = "all",
    best = 1, plot_en = 1, plotly = FALSE
  )
  expect_true(is.null(respwf))

  ## plot_cell_heatmap ###############
  hm <- plot_cell_heatmap(resultrect, sp_polygon, plot = TRUE)
  expect_true(is.data.frame(hm))
  expect_true(all(c("ID", "X", "Y", "n_probed") %in% names(hm)))
  expect_true(all(hm$n_probed >= 0))
  expect_true(sum(hm$n_probed) > 0)
  hm_nolog <- plot_cell_heatmap(resultrect, sp_polygon, log = FALSE, plot = FALSE)
  expect_equal(hm$n_probed, hm_nolog$n_probed)

  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  respwf <- plot_windfarmGA(resulthex, area,
    which_plot = "all",
    best = 2, plot_en = 1, plotly = FALSE
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
  fitnes_res <- plot_fitness_evolution(resultrect, interactive = FALSE)
  expect_true(is.list(fitnes_res) || inherits(fitnes_res, c("ggplot", "plotly", "plotly_hash")))

  ## plot_evolution ###############
  evo_res <- plot_evolution(resultrect, ask = FALSE)
  expect_true(is.null(evo_res))

  ## plot_leaflet #######################
  skip_if(compareVersion("4.3.0", paste0(R.version$major,".",R.version$minor)) == 1,
          "Skip as the version is <= 4.3.0. Errors in test_plots unresolved (leaflet::addMarkers(...))")
  ## Mock Packages not installed
  with_mocked_bindings(
    is_leaflet_installed = function() FALSE,
    expect_error(
      plot_leaflet(result = resultrect, area = sp_polygon, which = 1)
    )
  )

  vdata <- data.frame(ws = 12, wd = 0)
  resultSP <- genetic_algorithm(
    area = area,
    n = 5, iteration = 3,
    wind = vdata, rotor = 35,
    rotor_height = 100
  )
  p <- plot_leaflet(resultSP, area = area, which = 1)
  expect_s3_class(p, "leaflet")

  ## Plot the best wind farm on a leaflet map (ordered by energy values)
  p <- plot_leaflet(result = resultrect, area = sp_polygon, which = 1)
  expect_s3_class(p, "leaflet")

  ## Plot the best wind farm on a leaflet map (ordered by energy values)
  p <- plot_leaflet(result = resulthex, area = sp_polygon, which = 1)
  expect_s3_class(p, "leaflet")

  ## Plot the last wind farm (ordered by chronology).
  p <- plot_leaflet(
    result = resulthex, area = sp_polygon, orderitems = FALSE,
    which = 1
  )
  expect_s3_class(p, "leaflet")

  ## Plot the best wind farm on a leaflet map with the rectangular Grid
  Grid <- grid_area(sp_polygon, size = 150, prop = 0.4)
  p <- plot_leaflet(
    result = resultrect, area = sp_polygon, which = 1,
    grid = Grid[[2]]
  )
  expect_s3_class(p, "leaflet")

  ## Plot the last wind farm with hexagonal Grid
  Grid <- hexa_area(sp_polygon, size = 75)
  p <- plot_leaflet(
    result = resulthex, area = sp_polygon, which = 1,
    grid = Grid[[2]]
  )
  expect_s3_class(p, "leaflet")

  p <- plot_leaflet(result = resultrect, area = sp_polygon, which = 1)
  expect_s3_class(p, "leaflet")

  gr <- grid_area(st_as_sf(sp_polygon), size = 220)
  spnop <- gr[[2]]
  st_crs(spnop) <- NA
  p <- plot_leaflet(result = resultrect, area = sp_polygon, grid = spnop)
  expect_s3_class(p, "leaflet")

  p <- plot_leaflet(result = resulthex, area = sp_polygon, which = 1, orderitems = FALSE)
  expect_s3_class(p, "leaflet")

  p <- plot_leaflet(result = resulthex, area = sp_polygon, which = 1000, orderitems = FALSE)
  expect_s3_class(p, "leaflet")


  ## No Projection
  poly_nocrs <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    )))
  ))

  p <- plot_leaflet(result = resultrect, area = poly_nocrs)
  expect_s3_class(p, "leaflet")

  expect_error(genetic_algorithm(
    area = poly_nocrs,
    n = 12, iteration = 60,
    wind = winddat,
    rotor = 30,
    rotor_height = 100
  ))
})
