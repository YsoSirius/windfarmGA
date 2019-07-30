context("Test Random Search")
library(testthat)
library(sp)

test_that("Test Random Search Functions", {
  ## Data ##############
  load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
  resultSP <- resultrect
  Polygon1 <- polygon
  ## RandomSearch #########################
  new <- RandomSearch(resultSP, Polygon1, n = 20, best = 3, Plot = TRUE)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  new <- RandomSearch(resultSP, Polygon1, Plot = TRUE)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  new <- RandomSearch(resultSP[1:30,], Polygon1, best = 10000)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  ## Test Plots with Hexagons
  new <- RandomSearch(resultSP, GridMethod = "h", Polygon1, n = 10, best = 1)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  ## Plots ###################
  res <- RandomSearchPlot(resultRS = new, result = resultSP,
                   Polygon1 = Polygon1, best = 2)
  expect_true(is.null(res))

  data.in <- data.frame(ws = 12, wd = 0)
  result100 <- genAlgo(Polygon1 = Polygon1,
                       n = 5, iteration = 4,
                       vdirspe = data.in,
                       Rotor = 35, Proportionality = 1,
                       RotorHeight = 100)
  new10 <- RandomSearch(result100, Polygon1, n = 20, best = 3, Plot = TRUE)
  respl <- plot_random_search(new10, result100, Polygon1 = Polygon1)
  expect_true(is.null(respl))
  respl <- plot_random_search(new10, result100, Polygon1 = Polygon1, best = 1)
  expect_true(is.null(respl))
  
  new <- RandomSearch(resultSP, GridMethod = "h", Polygon1, n = 2, best = 1)
  res <- RandomSearchPlot(resultRS = new, result = resultSP,
                         Polygon1 = Polygon1, best = 100)
  expect_true(is.null(res))

})