context("Test Wake Functions")
library(raster)

test_that("Test Wake Functions", {
  ## Input Data ---------------------
  ###########################################
  polYgon <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(0, 0, 2000, 2000, 0),
      c(0, 2000, 2000, 0, 0)))),
    crs = 3035
  ))
  wnkl <- 20; dist <- 100000; dirct <- 0
  t <- st_coordinates(st_sample(polYgon, 10))
  
  ## Test get_dist_angles Function --------------
  ###########################################
  distanz <- 100000
  colnms <- c("Ay","Cx","Cy","Laenge_C","Laenge_B","Laenge_A","alpha","betha","gamma")
  ## Evaluate and plot for every turbine all other potentially influencing turbines
  potInfTur <- list()
  for (i in 1:(length(t[,1]))) {
    potInfTur[[i]] <- get_dist_angles(t, i, wnkl, distanz, polYgon)
  }
  expect_false(all(unlist(sapply(potInfTur, is.na))))
  dr <- do.call("rbind", potInfTur)
  expect_true(all((dr[dr[,"Ax"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[,"Ay"] == 0,colnms]) == 0))
  expect_true(all((dr[dr[,"Cx"] == 0,colnms]) == 0))
  expect_true(all((dr[dr[,"Cy"] == 0,colnms]) == 0))
  expect_true(all((dr[dr[,"Ax"] != 0,colnms]) != 0))
  
  ## With Plotting
  potInfTur_pl <- list()
  for (i in 1:(length(t[,1]))) {
    potInfTur_pl[[i]] <- get_dist_angles(t = t, o = i, wnkl = wnkl,
                                         dist = distanz, polYgon = polYgon,
                                         plotAngles = TRUE)
  }
  expect_false(all(unlist(sapply(potInfTur, is.na))))
  expect_true(identical(potInfTur, potInfTur_pl))
  dr <- do.call("rbind", potInfTur)
  expect_true(all((dr[dr[,"Ax"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[,"Ay"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[,"Cx"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[,"Cy"] == 0, colnms]) == 0))
  expect_true(all((dr[dr[,"Ax"] != 0, colnms]) != 0))

  
  
  ## Test turbine_influences Function --------------
  ###########################################
  resInfluPoi <- turbine_influences(t, wnkl, dist, polYgon, dirct)
  expect_is(resInfluPoi, "list")
  expect_output(str(resInfluPoi), "List of 10")
  expect_false(any(unlist(sapply(resInfluPoi, is.na))))
  df <- do.call("rbind", resInfluPoi)
  expect_true(all((df[dr[, "Ax"] == 0, colnms]) == 0))
  expect_true(all((df[dr[, "Ax"] != 0, colnms]) != 0))
  
  ## Bigger Angle
  wnkl <- 50
  t <- st_coordinates(st_sample(polYgon, 10))
  resInfluPoiWin <- turbine_influences(t, wnkl, dist, polYgon, dirct)
  expect_output(str(resInfluPoiWin), "List of 10")
  expect_false(any(unlist(sapply(resInfluPoiWin, is.na))))
  df1 <- do.call("rbind", resInfluPoiWin)
  expect_true(all((df1[df1[, "Ax"] == 0, colnms]) == 0))
  expect_true(all((df1[df1[, "Ax"] != 0, colnms]) != 0))
  expect_true(nrow(df1) > nrow(df))
  rm(df1, resInfluPoi)
  
  ## More Points and bigger Angle
  t <- st_coordinates(st_sample(polYgon, 20))
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
  expect_true(all((df2[df2[, "Ax"] == 0, 
                       c("Ay","Cx","Cy","Laenge_C","Laenge_B","Laenge_A",
                         "alpha","betha","gamma")]) == 0))
  expect_true(all((df2[df2[, "Ax"] != 0, 
                       c("Ay","Cx","Cy","Laenge_C","Laenge_B","Laenge_A",
                         "alpha","betha","gamma")]) != 0))
  expect_true(nrow(df1) > nrow(df2))
  
  
  ## Test calculate_energy Function ----------------------------
  ###########################################
  ## Initialize a dummy wind speed raster with value 1
  windraster <- suppressWarnings(
    terra::rasterize(polYgon, terra::rast(
      terra::ext(polYgon),
    ncol = 180, nrow = 180), field = 1))
  
  ## Create a uniform and unidirectional wind data.frame and plot the
  ## resulting wind rose
  data.in <- data.frame(ws = 12, wd = 0)
  
  ## Assign the rotor radius and a factor of the radius for grid spacing.
  Rotor <- 50; fcrR <- 3
  resGrid <- grid_area(shape = polYgon, size = Rotor * fcrR, prop = 1,
                       plotGrid = FALSE)
  
  ## Create an initial population with the indexed Grid, 15 turbines and
  ## 100 individuals.
  resStartGA <- init_population(Grid = resGrid[[1]], n = 15, nStart = 100)
  # expect_true(all(sapply(resStartGA, class) == "matrix"))
  expect_true(all(sapply(resStartGA, ncol) == 4))
  expect_true(all(sapply(resStartGA, nrow) == 15 ))
  expect_true(length(resStartGA) == 100)
  expect_false(any(sapply(resStartGA, is.na)))
  
  ## Calculate the expected energy output of the first individual of the
  ## population.
  resCalcEn <- calculate_energy(sel = resStartGA[[1]], referenceHeight = 50,
                           RotorHeight = 50, SurfaceRoughness = 0.14, wnkl = 20,
                           distanz = 100000, dirSpeed = data.in,
                           RotorR = 50, polygon1 = polYgon, 
                           topograp = FALSE, weibull = FALSE)
  
  expect_output(str(resCalcEn), "List of 1")
  # expect_true(class(resCalcEn[[1]]) == "matrix")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] < 
                    df[df[, "TotAbschProz"] != 0, "Windmean"]))
  
  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))
  
  resCalcEn <- calculate_energy(sel = resStartGA[[1]], referenceHeight = 50,
                           RotorHeight = 50, SurfaceRoughness = 0.14, wnkl = 20,
                           distanz = 100000, dirSpeed = data.in,
                           RotorR = 50, polygon1 = polYgon, 
                           topograp = FALSE, weibull = FALSE, plotit = TRUE)
  
  expect_output(str(resCalcEn), "List of 1")
  # expect_true(class(resCalcEn[[1]]) == "matrix")
  rm(resCalcEn, df)
  
  ## 2 Wind Directions 
  data.in <- as.data.frame(cbind(ws=c(12,12),wd=c(0,30)))
  resCalcEn <- calculate_energy(sel=resStartGA[[1]],referenceHeight= 50,
                           RotorHeight= 50, SurfaceRoughness = 0.14,wnkl = 20,
                           distanz = 100000, dirSpeed = data.in,
                           RotorR = 50, polygon1 = polYgon, topograp = FALSE, 
                           weibull = FALSE)
  
  expect_output(str(resCalcEn), "List of 2")
  # expect_true(class(resCalcEn[[1]]) == "matrix")
  df <- do.call(rbind, resCalcEn)
  expect_true(all(df[df[, "A_ov"] != 0, "TotAbschProz"] != 0))
  expect_true(all(df[df[, "TotAbschProz"] != 0, "V_New"] < df[df[, "TotAbschProz"] != 0, "Windmean"]))
  
  expect_false(any(unlist(sapply(resCalcEn, is.na))))
  expect_true(all(df[, "Rect_ID"] %in% resGrid[[1]][, "ID"]))
  
  
  ## Polygon with Holes is not plotted correctly (Hole is omitted)
  # windraster <- terra::rasterize(hole_shape, terra::rast(
  #   terra::ext(hole_shape),
  #   ncol = 180, nrow = 180), field = 1)
  # data.in <- data.frame(ws = c(12,12), wd = c(0,90))
  # Rotor <- 50; fcrR <- 3
  # resGrid <- grid_area(shape = hole_shape, size = Rotor * fcrR, prop = 1,
  #                       plotGrid = FALSE)
  # resStartGA <- init_population(Grid = resGrid[[1]], n = 15, nStart = 100)
  # resCalcEn <- calculate_energy(sel = resStartGA[[1]], referenceHeight = 50,
  #                          RotorHeight = 50, SurfaceRoughness = 0.14, wnkl = 20,
  #                          distanz = 100000, dirSpeed = data.in,
  #                          RotorR = 50, polygon1 = hole_shape, 
  #                          topograp = FALSE, weibull=FALSE,
  #                          plotit = TRUE)

})
