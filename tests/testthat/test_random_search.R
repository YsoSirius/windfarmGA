
test_that("Test Random Search Functions", {
  ## Data ##############
  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))

  ## RandomSearch #########################
  new <- random_search(resultrect, area, n = 20, best = 3, plot = TRUE)
  expect_type(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))

  new <- random_search(resultrect, area, plot = TRUE)
  expect_type(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))

  new <- random_search(resulthex, area, plot = TRUE)
  expect_type(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))

  new <- random_search(resultrect[1:30, ], area, best = 10000)
  expect_type(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))

  ## Test Plots with Hexagons
  new <- random_search(resultrect, area, n = 10, best = 1)
  expect_type(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))

  ## Plots ###################
  res <- plot_random_search(
    resultRS = new, result = resultrect,
    area = area, best = 1
  )
  expect_true(is.null(res))

  new10 <- random_search(resulthex, area, n = 20, best = 3, plot = TRUE)
  respl <- plot_random_search(new10, resulthex, area = area)
  expect_true(is.null(respl))

  new <- random_search(resultrect, area, n = 2, best = 1)
  res <- plot_random_search(
    resultRS = new, result = resultrect,
    area = area, best = 100
  )
  expect_true(is.null(res))

  vdata <- data.frame(ws = 12, wd = 0)
  resultSP <- genetic_algorithm(
    area = area,
    n = 5, iteration = 3,
    wind = vdata, rotor = 35,
    rotor_height = 100
  )
  new <- random_search(resultSP, area, n = 2, best = 1)
  res <- plot_random_search(
    resultRS = new, result = resultSP,
    area = area, best = 100
  )
  expect_true(is.null(res))
})
