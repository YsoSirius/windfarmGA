context("User Interaction")
library(windfarmGA)

test_that("User Input", {
  ## Rects #############
  load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
  load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
  
  ##which IDs #############
  resldat <- do.call("rbind", resultrect[,"bestPaEn"])
  maxDist <- as.numeric(resultrect[,"inputData"][[1]]['Rotorradius',]) * 2.2
  resldat <- as.data.frame(resldat[!duplicated(resldat[,'Run']),, drop=FALSE])
  resldat$GARun <- 1:nrow(resldat)
  resldat <- resldat[order(resldat[, 4], decreasing = TRUE),]
  bestGARun <- resldat$GARun[1]
  resolu <- as.numeric(resultrect[bestGARun,]$inputData["Resolution",][1])
  rotRad <- as.numeric(resultrect[bestGARun,]$inputData["Rotorradius",][1])
  propu  <- as.numeric(resultrect[bestGARun,]$inputData["Percentage of Polygon",][1])
  winddata <- resultrect[bestGARun,]$inputWind
  Grid <- grid_area(shape = polygon, resol = resolu, 
                    prop = propu, plotGrid = FALSE)
  maxFac <- rotRad * (resolu / (rotRad * 2))
  winddata <- windata_format(winddata)
  probabDir <- winddata[[2]]
  winddata <- winddata[[1]]
  layout_start <- resultrect[bestGARun,]$bestPaEn
  #####################
  
  id <- sample(layout_start[,"Rect_ID"], size = 10, T)
  f <- file()
  options(windfarmGA.connection = f)
  ans <- paste(id, collapse = "\n")
  write(ans, f)
  
  new <- RandomSearchTurb(resultrect, polygon)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  rm(new, new_df)
  
  new <- random_search_single(resultrect, polygon)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  plres <- plot_random_search(resultRS = new, result = resultrect, Polygon1 = polygon, best = 2)
  expect_true(is.null(plres))
  
  # reset connection
  options(windfarmGA.connection = stdin())
  # close the file
  close(f)
  
  
  ## Hexagons ##################
  rm(list = ls())
  load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
  load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
  
  ##which IDs #############
  resldat <- do.call("rbind", resulthex[,"bestPaEn"])
  resldat <- as.data.frame(resldat[!duplicated(resldat[,'Run']),, drop=FALSE])
  resldat$GARun <- 1:nrow(resldat)
  resldat <- resldat[order(resldat[, 4], decreasing = TRUE),]
  bestGARun <- resldat$GARun[1]
  resolu <- as.numeric(resulthex[bestGARun,]$inputData["Resolution",][1])
  rotRad <-  as.numeric(resulthex[bestGARun,]$inputData["Rotorradius",][1])
  maxDist <- as.numeric(resulthex[,"inputData"][[1]]['Rotorradius',]) * 2.2
  propu  <- as.numeric(resulthex[bestGARun,]$inputData["Percentage of Polygon",][1])
  winddata <- resulthex[bestGARun,]$inputWind
  Grid <- hexa_area(polygon, size = resolu / 2)
  maxFac <- rotRad * (resolu / (rotRad * 2))
  winddata <- windata_format(winddata)
  probabDir <- winddata[[2]]
  winddata <- winddata[[1]]
  layout_start <- resulthex[bestGARun,]$bestPaEn
  
  id <- sample(layout_start[,"Rect_ID"], 2)
  f <- file()
  options(windfarmGA.connection = f)
  ans <- paste(id, collapse = "\n")
  write(ans, f)
  
  new <- RandomSearchTurb(resulthex, polygon, GridMethod = "h")
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  rm(new, new_df)
  
  # reset connection
  options(windfarmGA.connection = stdin())
  # close the file
  close(f)
  
  
  ## windfarmGA ###############
  f <- file()
  options(windfarmGA.connection = f)
  ans <- paste(c("E","E", "F", "n", "n"), collapse = "\n")
  write(ans, f)

  Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
                            c(4499991, 2669343), c(4499991, 2668272)))
  Polygon1 <- Polygons(list(Polygon1), 1);
  Polygon1 <- SpatialPolygons(list(Polygon1))
  Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  proj4string(Polygon1) <- CRS(Projection)
  data.in <- data.frame(ws = 12, wd = 0)
  # expect_error(windfarmGA(Polygon1 = Polygon1,
  #                      n = 12,
  #                      vdirspe = data.in,
  #                      Rotor = 60,
  #                      RotorHeight = 100))
  
  # expect_error(windfarmGA(Polygon1 = Polygon1,
  #                         n = 12,
  #                         vdirspe = data.in,
  #                         selstate = "FIX", crossPart1 = "EQU",
  #                         Rotor = 60, weibull = TRUE, 
  #                         weibullsrc = data.frame(x=1,y=2),
  #                         RotorHeight = 100))
  # 
  # expect_error(windfarmGA(Polygon1 = Polygon1,
  #                         n = 12,
  #                         vdirspe = data.in,
  #                         selstate = "FIX", crossPart1 = "EQU",
  #                         Rotor = 60, weibull = TRUE, 
  #                         weibullsrc = list(x=1,y=2),
  #                         RotorHeight = 100))
  
  # reset connection
  options(windfarmGA.connection = stdin())
  # close the file
  close(f)
  
})
