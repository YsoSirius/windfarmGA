context("Test Random Search")
library(testthat)
library(sf)

test_that("Test Random Search Functions", {
  ## Data ##############
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)))),
    crs = 3035
  ))
  
  ## RandomSearch #########################
  new <- random_search(resultrect, Polygon1, n = 20, best = 3, Plot = TRUE)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  new <- random_search(resultrect, Polygon1, Plot = TRUE)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  new <- random_search(resultrect[1:30,], Polygon1, best = 10000)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  ## Test Plots with Hexagons
  new <- random_search(resultrect, Polygon1, n = 10, best = 1)
  expect_is(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
  
  ## Plots ###################
  res <- plot_random_search(resultRS = new, result = resultrect,
                            Polygon1 = Polygon1, best = 2)
  expect_true(is.null(res))

  data.in <- data.frame(ws = 12, wd = 0)
  result100 <- genetic_algorithm(Polygon1 = Polygon1,
                                 n = 5, iteration = 4,
                                 vdirspe = data.in,
                                 Rotor = 35, Proportionality = 1,
                                 RotorHeight = 100)
  new10 <- random_search(result100, Polygon1, n = 20, best = 3, Plot = TRUE)
  respl <- plot_random_search(new10, result100, Polygon1 = Polygon1)
  expect_true(is.null(respl))
  respl <- plot_random_search(new10, result100, Polygon1 = Polygon1, best = 1)
  expect_true(is.null(respl))
  
  new <- random_search(resultrect, Polygon1, n = 2, best = 1)
  res <- plot_random_search(resultRS = new, result = resultrect,
                            Polygon1 = Polygon1, best = 100)
  expect_true(is.null(res))

})