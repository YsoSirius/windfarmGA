context("User Interaction")
library(sf)

## Function to suppress print/cat outputs
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

test_that("User Input", {
  # skip_on_cran()

  ## random_search_single - Rects #############
  polygon <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))

  ## which IDs
  resldat <- do.call("rbind", resultrect[, "bestPaEn"])
  maxDist <- as.numeric(resultrect[, "inputData"][[1]]["Rotorradius", ]) * 2.2
  resldat <- as.data.frame(resldat[!duplicated(resldat[, "Run"]), , drop = FALSE])
  resldat$GARun <- 1:nrow(resldat)
  resldat <- resldat[order(resldat[, 4], decreasing = TRUE), ]
  bestGARun <- resldat$GARun[1]
  resolu <- as.numeric(resultrect[bestGARun, ]$inputData["Resolution", ][1])
  rotRad <- as.numeric(resultrect[bestGARun, ]$inputData["Rotorradius", ][1])
  propu <- as.numeric(resultrect[bestGARun, ]$inputData["Percentage of Polygon", ][1])
  winddata <- resultrect[bestGARun, ]$inputWind
  Grid <- grid_area(
    shape = polygon, size = resolu,
    prop = propu, plotGrid = FALSE
  )
  maxFac <- rotRad * (resolu / (rotRad * 2))
  winddata <- windata_format(winddata)
  probabDir <- winddata[[2]]
  winddata <- winddata[[1]]
  layout_start <- resultrect[bestGARun, ]$bestPaEn

  id <- sample(layout_start[, "Rect_ID"], size = 100, T)
  f <- file()
  options(windfarmGA.connection = f)
  ans <- paste(id, collapse = "\n")
  write(ans, f)

  new <- random_search_single(resultrect, polygon)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  rm(new, new_df)

  for (i in 1:40) {
    new <- quiet(random_search_single(resultrect, polygon, max_dist = 5, Plot = TRUE))
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
  polygon <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))

  ## which IDs
  resldat <- do.call("rbind", resulthex[, "bestPaEn"])
  resldat <- as.data.frame(resldat[!duplicated(resldat[, "Run"]), , drop = FALSE])
  resldat$GARun <- 1:nrow(resldat)
  resldat <- resldat[order(resldat[, 4], decreasing = TRUE), ]
  bestGARun <- resldat$GARun[1]
  resolu <- as.numeric(resulthex[bestGARun, ]$inputData["Resolution", ][1])
  rotRad <- as.numeric(resulthex[bestGARun, ]$inputData["Rotorradius", ][1])
  maxDist <- as.numeric(resulthex[, "inputData"][[1]]["Rotorradius", ]) * 2.2
  propu <- as.numeric(resulthex[bestGARun, ]$inputData["Percentage of Polygon", ][1])
  winddata <- resulthex[bestGARun, ]$inputWind
  Grid <- hexa_area(polygon, size = resolu)
  maxFac <- rotRad * (resolu / (rotRad * 2))
  winddata <- windata_format(winddata)
  probabDir <- winddata[[2]]
  winddata <- winddata[[1]]
  layout_start <- resulthex[bestGARun, ]$bestPaEn

  id <- sample(layout_start[, "Rect_ID"], 2)
  f <- file()
  options(windfarmGA.connection = f)
  ans <- paste(id, collapse = "\n")
  write(ans, f)

  new <- random_search_single(resulthex, polygon)
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
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  data.in <- data.frame(ws = 12, wd = 0)
})
