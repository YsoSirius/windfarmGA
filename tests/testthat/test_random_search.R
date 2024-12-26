
test_that("Test Random Search Functions", {
  ## Data ##############
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))

  ## RandomSearch #########################
  new <- random_search(resultrect, Polygon1, n = 20, best = 3, Plot = TRUE)
  expect_type(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))

  new <- random_search(resultrect, Polygon1, Plot = TRUE)
  expect_type(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))

  new <- random_search(resulthex, Polygon1, Plot = TRUE)
  expect_type(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))

  new <- random_search(resultrect[1:30, ], Polygon1, best = 10000)
  expect_type(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))

  ## Test Plots with Hexagons
  new <- random_search(resultrect, Polygon1, n = 10, best = 1)
  expect_type(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))

  ## Plots ###################
  res <- plot_random_search(
    resultRS = new, result = resultrect,
    Polygon1 = Polygon1, best = 1
  )
  expect_true(is.null(res))

  new10 <- random_search(resulthex, Polygon1, n = 20, best = 3, Plot = TRUE)
  respl <- plot_random_search(new10, resulthex, Polygon1 = Polygon1)
  expect_true(is.null(respl))

  new <- random_search(resultrect, Polygon1, n = 2, best = 1)
  res <- plot_random_search(
    resultRS = new, result = resultrect,
    Polygon1 = Polygon1, best = 100
  )
  expect_true(is.null(res))

  vdata <- data.frame(ws = 12, wd = 0)
  resultSP <- genetic_algorithm(
    Polygon1 = Polygon1,
    n = 5, iteration = 3,
    vdirspe = vdata, Rotor = 35,
    RotorHeight = 100
  )
  new <- random_search(resultSP, Polygon1, n = 2, best = 1)
  res <- plot_random_search(
    resultRS = new, result = resultSP,
    Polygon1 = Polygon1, best = 100
  )
  expect_true(is.null(res))
})
