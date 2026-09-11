
test_that("Test Wake Functions", {
  ## Input Data ---------------------
  ###########################################
  polYgon <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(0, 0, 2000, 2000, 0),
      c(0, 2000, 2000, 0, 0)
    ))),
    crs = 3035
  ))
  wnkl <- 20
  dist <- 100000
  dirct <- 0
  t <- sf::st_coordinates(sf::st_sample(polYgon, 10))
  t <- cbind(t, "Z" = 1)

  ## Test circle_intersection Function --------------
  ###########################################
  aov <- circle_intersection(10, 20, 10, 20, 10)
  expect_type(aov, "double")
  expect_length(aov, 1)
  expect_gt(aov, 250)
  expect_lt(aov, 260)
  aov <- circle_intersection(10, 20, 10, 30, 0)
  expect_type(aov, "double")
  expect_length(aov, 1)
  expect_gt(aov, 130)
  expect_lt(aov, 150)
  aov <- circle_intersection(10, 20, 10, 20, 100)
  expect_type(aov, "double")
  expect_identical(aov, 0)
  aov <- circle_intersection(10, 20, 10, 100, 10)
  expect_type(aov, "double")
  expect_identical(aov, 0)
  aov <- circle_intersection(10, 30, 10, 10, 0)
  expect_type(aov, "double")
  expect_identical(aov, 10^2 * pi)
  aov <- circle_intersection(40, 30, 10, 10, 0)
  expect_type(aov, "double")
  expect_identical(aov, 30^2 * pi)
  aov_vec <- circle_intersection(
    c(10, 10, 10),
    c(20, 20, 30),
    c(10, 10, 10),
    c(20, 20, 10),
    c(10, 100, 0)
  )
  expect_length(aov_vec, 3)
  expect_gt(aov_vec[1], 250)
  expect_lt(aov_vec[1], 260)
  expect_identical(aov_vec[2], 0)
  expect_identical(aov_vec[3], 10^2 * pi)


  ## Test get_dist_angles Function --------------
  ###########################################
  distanz <- 100000
  colnms <- c("Ay", "Cx", "Cy", "Laenge_C", "Laenge_B", "Laenge_A", "alpha", "betha", "gamma")
  ## Evaluate and plot for every turbine all other potentially influencing turbines
  potInfTur <- list()
  for (i in 1:(length(t[, 1]))) {
    potInfTur[[i]] <- get_dist_angles(t, i, wnkl, distanz, polYgon)
  }
  expect_false(all(unlist(sapply(potInfTur, is.na))))
  dr <- do.call("rbind", potInfTur)
  expect_true(all((dr[dr[, "Ax"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[, "Ay"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[, "Cx"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[, "Cy"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[, "Ax"] != 0, colnms]) != 0))

  ## With Plotting
  pointInfluences <- list()
  for (i in 1:(length(t[, 1]))) {
    pointInfluences[[i]] <- get_dist_angles(
      t = t, o = i, wnkl = wnkl,
      dist = distanz, area = polYgon,
      plot_angles = TRUE
    )
  }
  expect_false(all(unlist(sapply(potInfTur, is.na))))
  expect_true(identical(potInfTur, pointInfluences))
  dr <- do.call("rbind", potInfTur)
  expect_true(all((dr[dr[, "Ax"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[, "Ay"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[, "Cx"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[, "Cy"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[, "Ax"] != 0, colnms]) != 0))

  ## Fixed geometry: wake cone from upwind (higher Y)
  tfix <- cbind(
    X = c(0, 10, 50, 0),
    Y = c(0, 100, 100, 200),
    Z = 1
  )
  in_cone <- get_dist_angles(tfix, 1, 20, 100000, polYgon)
  expect_equal(nrow(in_cone), 2L)
  expect_equal(unname(in_cone[1, "Ax"]), 10)
  expect_equal(unname(in_cone[1, "Ay"]), 100)
  expect_lt(unname(in_cone[1, "alpha"]), 20)
  expect_equal(unname(in_cone[2, "Ax"]), 0)
  expect_equal(unname(in_cone[2, "Ay"]), 200)
  expect_equal(unname(in_cone[2, "alpha"]), 0)
  expect_equal(get_dist_angles(tfix, 1, 20, 100000, polYgon), in_cone)
  wide <- get_dist_angles(tfix, 1, 40, 100000, polYgon)
  expect_equal(nrow(wide), 3L)
  dummy <- get_dist_angles(tfix, 4, 20, 100000, polYgon)
  expect_equal(unname(dummy[1, "Ax"]), 0)
  expect_equal(unname(dummy[1, "Bx"]), 0)
  expect_equal(unname(dummy[1, "By"]), 200)

  ## Test turbine_influences Function --------------
  ###########################################
  resInfluPoi <- turbine_influences(t, wnkl, dist, polYgon, dirct)
  expect_type(resInfluPoi, "list")
  expect_output(str(resInfluPoi), "List of 10")
  expect_false(any(unlist(sapply(resInfluPoi, is.na))))
  df <- do.call("rbind", resInfluPoi)
  expect_true(all((df[dr[, "Ax"] == 0, colnms]) == 0))
  expect_true(all((df[dr[, "Ax"] != 0, colnms]) != 0))

  ## Bigger Angle
  wnkl <- 50
  t <- sf::st_coordinates(sf::st_sample(polYgon, 10))
  t <- cbind(t, "Z" = 100)
  resInfluPoiWin <- turbine_influences(t, wnkl, dist, polYgon, dirct)
  expect_output(str(resInfluPoiWin), "List of 10")
  expect_false(any(unlist(sapply(resInfluPoiWin, is.na))))
  df1 <- do.call("rbind", resInfluPoiWin)
  expect_true(all((df1[df1[, "Ax"] == 0, colnms]) == 0))
  expect_true(all((df1[df1[, "Ax"] != 0, colnms]) != 0))
  expect_true(nrow(df1) > nrow(df))
  rm(df1, resInfluPoi)

  ## More Points and bigger Angle
  t <- sf::st_coordinates(sf::st_sample(polYgon, 20))
  t <- cbind(t, "Z" = 1)
  resInfluPoi <- turbine_influences(t, wnkl, dist, polYgon, dirct)
  expect_output(str(resInfluPoi), "List of 20")
  expect_false(any(unlist(sapply(resInfluPoi, is.na))))
  df1 <- do.call("rbind", resInfluPoi)
  expect_true(all((df1[df1[, "Ax"] == 0, colnms]) == 0))
  expect_true(all((df1[df1[, "Ax"] != 0, colnms]) != 0))
  rm(resInfluPoi)

  ## Same Points & Smaller Angle
  wnkl <- 10
  resInfluPoi <- turbine_influences(t, wnkl, dist, polYgon, dirct)
  expect_output(str(resInfluPoi), "List of 20")
  expect_false(any(unlist(sapply(resInfluPoi, is.na))))
  df2 <- do.call("rbind", resInfluPoi)
  expect_true(all((df2[
    df2[, "Ax"] == 0,
    c(
      "Ay", "Cx", "Cy", "Laenge_C", "Laenge_B", "Laenge_A",
      "alpha", "betha", "gamma"
    )
  ]) == 0))
  expect_true(all((df2[
    df2[, "Ax"] != 0,
    c(
      "Ay", "Cx", "Cy", "Laenge_C", "Laenge_B", "Laenge_A",
      "alpha", "betha", "gamma"
    )
  ]) != 0))
  expect_true(nrow(df1) > nrow(df2))


  ## Test calculate_energy Function ----------------------------
  ###########################################
  ## Initialize a dummy wind speed raster with value 1
  windraster <- suppressWarnings(
    terra::rasterize(polYgon, terra::rast(
      terra::ext(polYgon),
      ncol = 180, nrow = 180
    ), field = 1)
  )

  ## Create a uniform and unidirectional wind data.frame and plot the
  ## resulting wind rose
  vdata <- data.frame(ws = 12, wd = 0)

  ## Assign the rotor radius and a factor of the radius for grid spacing.
  Rotor <- 50
  fcr <- 3
  resGrid <- grid_area(
    area = polYgon, size = Rotor * fcr, prop = 1,
    plot_grid = FALSE
  )

  ## Create an initial population with the indexed Grid, 15 turbines and
  ## 100 individuals.
  resStartGA <- init_population(grid = resGrid[[1]], n = 15, n_start = 100)
  expect_true(all(sapply(resStartGA, ncol) == 4))
  expect_true(all(sapply(resStartGA, nrow) == 15))
  expect_true(length(resStartGA) == 100)
  expect_false(any(sapply(resStartGA, is.na)))

  ## Calculate the expected energy output of the first individual of the
  ## population.
  resCalcEn <- calculate_energy(
    layout = resStartGA[[1]], reference_height = 50,
    rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
    wake_distance = 100000, wind = vdata,
    rotor = 50, area = polYgon,
    terrain = FALSE, weibull = FALSE
  )

  expect_output(str(resCalcEn), "List of 1")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] <
    df[df[, "TotAbschProz"] != 0, "Windmean"]))
  expect_true(all(tapply(df[, "TotAbschProz"], df[, "Punkt_id"], function(x) {
    diff(range(x)) < 1e-10
  })))
  expect_true(all(tapply(df[, "V_New"], df[, "Punkt_id"], function(x) {
    diff(range(x)) < 1e-10
  })))

  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))

  ## North wind + rectangular grid: same-X column is directly upwind (alpha = 0)
  options(windfarmGA.power_curve = NULL)
  site <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  grid_n <- grid_area(area = site, size = 30 * 5, prop = 1, plot_grid = FALSE)
  lay_n <- init_population(grid = grid_n[[1]], n = 20, n_start = 1)[[1]]
  en_n <- calculate_energy(
    layout = lay_n, reference_height = 100, rotor_height = 100,
    surface_roughness = 0.3, wake_angle = 20, wake_distance = 100000,
    wind = data.frame(ws = 12, wd = 0), rotor = 30, area = site,
    terrain = FALSE, weibull = FALSE
  )
  df_n <- do.call(rbind, en_n)
  expect_gt(max(df_n[, "TotAbschProz"]), 0)
  expect_lt(unname(df_n[1, "Parkwirkungsgrad"]), 100)

  ## Logarithmic wind profile: hub above reference height increases wind speed
  resEq <- calculate_energy(
    layout = resStartGA[[1]], reference_height = 50,
    rotor_height = 50, surface_roughness = 0.03, wake_angle = 20,
    wake_distance = 100000, wind = vdata,
    rotor = 50, area = polYgon,
    terrain = FALSE, weibull = FALSE
  )
  resHub <- calculate_energy(
    layout = resStartGA[[1]], reference_height = 50,
    rotor_height = 100, surface_roughness = 0.03, wake_angle = 20,
    wake_distance = 100000, wind = vdata,
    rotor = 50, area = polYgon,
    terrain = FALSE, weibull = FALSE
  )
  expect_gt(
    mean(do.call(rbind, resHub)[, "Windmean"]),
    mean(do.call(rbind, resEq)[, "Windmean"])
  )

  ## Cut-in: speeds below the threshold contribute no power
  old_cut <- options(windfarmGA.cut_in = 20)
  resCut <- calculate_energy(
    layout = resStartGA[[1]], reference_height = 50,
    rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
    wake_distance = 100000, wind = data.frame(ws = 12, wd = 0),
    rotor = 50, area = polYgon,
    terrain = FALSE, weibull = FALSE
  )
  options(old_cut)
  expect_equal(unname(do.call(rbind, resCut)[1, "Energy_Output_Red"]), 0)

  ## Power curve: park totals (sum of turbine kW), not the first machine
  curve <- data.frame(
    ws = c(0, 3, 4, 12, 25, 26),
    power = c(0, 0, 80, 2000, 2000, 0)
  )
  old_pc <- options(windfarmGA.power_curve = curve)
  resPc <- calculate_energy(
    layout = resStartGA[[1]], reference_height = 50,
    rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
    wake_distance = 100000, wind = data.frame(ws = 8, wd = 0),
    rotor = 50, area = polYgon,
    terrain = FALSE, weibull = FALSE
  )
  options(old_pc)
  dfPc <- do.call(rbind, resPc)
  n_t <- length(unique(dfPc[, "Punkt_id"]))
  p8 <- lookup_power_curve(8, curve)
  expect_equal(unname(dfPc[1, "Energy_Output_Voll"]), n_t * p8, tolerance = 1e-6)
  expect_gt(unname(dfPc[1, "Energy_Output_Voll"]), p8)
  expect_lt(unname(dfPc[1, "Energy_Output_Red"]), unname(dfPc[1, "Energy_Output_Voll"]))
  expect_lt(unname(dfPc[1, "Parkwirkungsgrad"]), 100)

  resCalcEn <- calculate_energy(
    layout = resStartGA[[1]], reference_height = 50,
    rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
    wake_distance = 100000, wind = vdata,
    rotor = 50, area = polYgon,
    terrain = FALSE, weibull = FALSE, plot = TRUE
  )

  expect_output(str(resCalcEn), "List of 1")
  rm(resCalcEn, df)

  ## 2 Wind Directions
  vdata <- as.data.frame(cbind(ws = c(12, 12), wd = c(0, 30)))
  resCalcEn <- calculate_energy(
    layout = resStartGA[[1]], reference_height = 50,
    rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
    wake_distance = 100000, wind = vdata,
    rotor = 50, area = polYgon, terrain = FALSE,
    weibull = FALSE
  )

  expect_output(str(resCalcEn), "List of 2")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] < df[df[, "TotAbschProz"] != 0, "Windmean"]))

  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))


  ## Polygon with Holes is not plotted correctly (Hole is omitted)
  # windraster <- terra::rasterize(hole_shape, terra::rast(
  #   terra::ext(hole_shape),
  #   ncol = 180, nrow = 180), field = 1)
  # vdata <- data.frame(ws = c(12,12), wd = c(0,90))
  # Rotor <- 50; fcr <- 3
  # resGrid <- grid_area(area = hole_shape, size = Rotor * fcr, prop = 1,
  #                       plot_grid = FALSE)
  # resStartGA <- init_population(Grid = resGrid[[1]], n = 15, n_start = 100)
  # resCalcEn <- calculate_energy(layout = resStartGA[[1]], reference_height = 50,
  #                          rotor_height = 50, surface_roughness = 0.14, wake_angle = 20,
  #                          wake_distance = 100000, wind = vdata,
  #                          rotor = 50, area = hole_shape,
  #                          terrain = FALSE, weibull=FALSE,
  #                          plot = TRUE)
})

