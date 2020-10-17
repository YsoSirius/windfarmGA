context("User Interaction")
library(windfarmGA)

test_that("User Input", {
  skip_on_cran()
  # skip_on_appveyor()
  
  ## getISO3 ################
  f <- file()
  options(windfarmGA.connection = f)
  ans <- paste("ADMIN", collapse = "\n")
  write(ans, f)
  
  points <- cbind(c(4488182.26267016, 4488852.91748256),
                 c(2667398.93118627, 2667398.93118627))
  res <- getISO3(pp = points, crs_pp = 3035, col="?")
  expect_true(res == "Austria")
  expect_false(anyNA(res))
  expect_true(all(dim(res) == c(1,1)))
  
  # reset connection
  options(windfarmGA.connection = stdin())
  # close the file
  close(f)
  
  ## Create Error
  f <- file()
  options(windfarmGA.connection = f)
  ans <- paste("notexistent", collapse = "\n")
  write(ans, f)
  
  points <- cbind(c(4488182.26267016, 4488852.91748256),
                 c(2667398.93118627, 2667398.93118627))
  expect_error(getISO3(pp = points, crs_pp = 3035, col="?"))
  
  # reset connection
  options(windfarmGA.connection = stdin())
  close(f)
  
  ## random_search_single - Rects #############
  load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
  load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
  
  ##which IDs
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
  
  id <- sample(layout_start[,"Rect_ID"], size = 100, T)
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

  for (i in 1:40) {
    new <- random_search_single(resultrect, polygon, max_dist = 5, Plot = TRUE)
    expect_is(new, "list")
    expect_false(anyNA(unlist(new)))
    new_df <- do.call(rbind, new)
    expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
    expect_true(all(new_df[, "EnergyOverall"] > 0))
    expect_true(all(new_df[, "AbschGesamt"] >= 0))
    rm(new, new_df)    
  }
    
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
  
  
  ## random_search_single - Hexagons ##################
  rm(list = ls())
  load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
  load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
  
  ##which IDs
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

  Polygon1 <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
                            c(4499991, 2669343), c(4499991, 2668272)))
  Polygon1 <- Polygons(list(Polygon1), 1)
  Polygon1 <- SpatialPolygons(list(Polygon1))
  Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  proj4string(Polygon1) <- CRS(Projection)
  data.in <- data.frame(ws = 12, wd = 0)
  
  ## grid spacing NOT appropriate
  f <- file()
  options(windfarmGA.connection = f)
  write(paste(c("E","E", "F", "n"), collapse = "\n"), f)
  expect_error(windfarmGA(
    Polygon1 = Polygon1,
    n = 12, 
    vdirspe = data.in,
    Rotor = 60,
    RotorHeight = 100
  ))
  # reset connection / close the file
  options(windfarmGA.connection = stdin())
  close(f)
  

  ## Wrong crossPart1 argument
  f <- file()
  options(windfarmGA.connection = f)
  write(paste(c("E","E", "F", "", ""), collapse = "\n"), f)
  res <- windfarmGA(
    Polygon1 = Polygon1, crossPart1 = "somethign",
    n = 12, iteration = 3,
    vdirspe = data.in,
    Rotor = 60,
    RotorHeight = 100
  )
  expect_true(nrow(res) == 3)
  expect_is(res, "matrix")
  expect_false(any(unlist(sapply(res, is.na))))
  # reset connection / close the file
  options(windfarmGA.connection = stdin())
  close(f)
  
})

