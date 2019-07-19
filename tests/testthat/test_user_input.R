context("User Interaction")
library(windfarmGA)

test_that("User Input", {
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
  
  id <- sample(layout_start[,"Rect_ID"], 2)
  f <- file()
  options(windfarmGA.connection = f)
  ans <- paste(id, collapse = "\n")
  write(ans, f)
  
  
  new <- random_search_single(resultrect, polygon, n = 20)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] < 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  plres <- plot_random_search(resultRS = new, result = resultrect, Polygon1 = polygon, best = 2)
  expect_true(is.null(plres))
  
  # reset connection
  options(windfarmGA.connection = stdin())
  # close the file
  close(f)
})