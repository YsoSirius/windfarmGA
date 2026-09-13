## Function to suppress print/cat outputs
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

test_that("User Input", {
  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  vdata <- data.frame(ws = 12, wd = 0)

  expect_error(
    genetic_algorithm(
      area = area,
      selection_mode = "asdasd",
      n = 12,
      wind = vdata,
      rotor = 35, iteration = 1,
      rotor_height = 100
    ),
    "selection_mode"
  )

  polygon <- area
  id <- resultrect[1, "bestPaEn"][[1]][1, "Rect_ID"]
  new <- random_search_single(resultrect, polygon, turbine = id, runs = 5)
  expect_type(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))

  new <- quiet(random_search_single(
    resultrect, polygon, turbine = id, runs = 3,
    max_dist = 5, plot = TRUE
  ))
  expect_type(new, "list")
  plres <- plot_random_search(
    resultRS = new, result = resultrect, area = polygon, best = 2
  )
  expect_true(is.null(plres))

  id_hex <- resulthex[1, "bestPaEn"][[1]][1, "Rect_ID"]
  new <- random_search_single(resulthex, polygon, turbine = id_hex, runs = 5)
  expect_type(new, "list")
  expect_false(anyNA(unlist(new)))
  new_df <- do.call(rbind, new)
  expect_true(all(new_df[, "EfficAllDir"] <= 100 & new_df[, "EfficAllDir"] > 0))
  expect_true(all(new_df[, "EnergyOverall"] > 0))
  expect_true(all(new_df[, "AbschGesamt"] >= 0))
})
