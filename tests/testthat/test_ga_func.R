
## Function to suppress print/cat outputs
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

expect_selected_ids <- function(sel, n_turb, grid_ids) {
  expect_type(sel, "list")
  expect_length(sel, 2)
  ids <- sel[[1]]
  expect_true(is.matrix(ids))
  expect_equal(nrow(ids), n_turb)
  expect_true(all(apply(ids, 2, function(x) length(unique(x)) == n_turb)))
  expect_true(all(ids %in% grid_ids))
  expect_true(all(sel[[2]] > 0))
  expect_false(anyNA(ids))
}

selection_as_binary <- function(sel, Grid) {
  ids <- sel[[1]]
  k <- ncol(ids)
  bins <- matrix(0, nrow = nrow(Grid), ncol = k)
  for (j in seq_len(k)) {
    bins[match(ids[, j], Grid[, "ID"]), j] <- 1
  }
  list(
    data.frame(ID = Grid[, "ID"], bins, check.names = FALSE),
    data.frame(ID = 1, matrix(sel[[2]], nrow = 1), check.names = FALSE)
  )
}

test_that("Test Genetic Algorithm Function", {
  ## Data ##############
  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(0, 0, 2000, 2000, 0),
      c(0, 2000, 2000, 0, 0)
    ))),
    crs = 3035
  ))

  Polygon2 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(0, 0, 1500, 2000, 0),
      c(0, 3500, 2000, 0, 0)
    ))),
    crs = 3035
  ))

  ## BAROHOEHE ################################
  data <- matrix(seq(0, 5000, 500))
  res <- barometric_height(data)
  expect_false(anyNA(res))
  res1 <- barometric_height(data[, 1])
  expect_false(anyNA(res1))
  expect_true(all.equal(res, res1))

  data <- data.frame(
    id = sample(1:10, length(seq(0, 5000, 500)), replace = TRUE),
    elev = seq(0, 5000, 500)
  )
  res2 <- barometric_height(data = data, "elev")
  expect_false(anyNA(res2))
  expect_true(all.equal(res2, res1))
  expect_error(barometric_height(data = data))
  rm(data, res, res1, res2)

  ## GRIDFILTER ################################
  Grid <- grid_area(area = area, size = 200, prop = 1)
  expect_true(is.matrix(Grid[[1]]))
  expect_s3_class(Grid[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid[[1]]))

  Grid <- grid_area(area = area, size = 200, prop = 0.1)
  expect_true(is.matrix(Grid[[1]]))
  expect_s3_class(Grid[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid[[1]]))

  Grid <- grid_area(area = area, size = 500, prop = 0.1)
  expect_true(is.matrix(Grid[[1]]))
  expect_s3_class(Grid[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid[[1]]))

  Grid <- grid_area(area = area, size = 500, prop = 0)
  expect_true(is.matrix(Grid[[1]]))
  expect_s3_class(Grid[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid[[1]]))

  Grid <- grid_area(area = area, size = 300, prop = 0, plot_grid = TRUE)
  expect_true(is.matrix(Grid[[1]]))
  expect_s3_class(Grid[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid[[1]]))

  ## too high resolution - error
  quiet(expect_error(grid_area(area = area, size = 1e+06, prop = -1)))
  ## TODO - no check for too small size
  # expect_error(GridFilter(area = area, size = 0.5, prop = -1))

  Grid <- grid_area(area = Polygon2, size = 300, prop = 100)
  expect_true(is.matrix(Grid[[1]]))
  expect_s3_class(Grid[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid[[1]]))

  Grid1 <- grid_area(area = Polygon2, size = 300, prop = 0.1)
  expect_true(is.matrix(Grid[[1]]))
  expect_s3_class(Grid1[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid1[[1]]))
  expect_true(nrow(Grid1[[1]]) > nrow(Grid[[1]]))

  Grid1 <- grid_area(area = Polygon2, size = 300, prop = -100)
  expect_true(is.matrix(Grid[[1]]))
  expect_s3_class(Grid1[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid1[[1]]))
  expect_true(nrow(Grid1[[1]]) > nrow(Grid[[1]]))
  rm(Grid1, Polygon2)

  ## HEXATEX #################
  HexGrid <- hexa_area(area, 100, FALSE)
  expect_true(is.matrix(HexGrid[[1]]))
  expect_s3_class(HexGrid[[2]], "sfc_POLYGON")
  expect_false(anyNA(HexGrid[[1]]))

  HexGrid <- hexa_area(area, 100, TRUE)
  expect_true(is.matrix(HexGrid[[1]]))
  expect_s3_class(HexGrid[[2]], "sfc_POLYGON")
  expect_false(anyNA(HexGrid[[1]]))

  HexGrid <- hexa_area(area, 200, FALSE)
  expect_true(is.matrix(HexGrid[[1]]))
  expect_s3_class(HexGrid[[2]], "sfc_POLYGON")
  expect_false(anyNA(HexGrid[[1]]))

  HexGrid <- hexa_area(area, 400.1, FALSE)
  expect_true(is.matrix(HexGrid[[1]]))
  expect_s3_class(HexGrid[[2]], "sfc_POLYGON")
  expect_false(anyNA(HexGrid[[1]]))

  quiet(expect_error(hexa_area(area, 1000000000, FALSE)))

  ## STARTGA ################################
  startsel <- init_population(Grid[[1]], n = 10, n_start = 20)
  expect_type(startsel, "list")
  expect_true(all(sapply(startsel, nrow) == 10))
  expect_true(all(sapply(startsel, ncol) == 4))
  expect_output(str(startsel), "List of 20")
  expect_false(any(unlist(sapply(startsel, is.na))))

  # Produce Errors (quietly)
  quiet(expect_error(init_population(Grid[[1]][1:10, ], n = 10, n_start = 20)))
  quiet(expect_error(init_population(Grid[[1]][1:10, ], n = 7, n_start = 20)))

  startsel <- init_population(Grid[[1]], n = 20, n_start = 25)
  expect_type(startsel, "list")
  expect_true(all(sapply(startsel, nrow) == 20))
  expect_true(all(sapply(startsel, ncol) == 4))
  expect_output(str(startsel), "List of 25")
  expect_false(any(unlist(sapply(startsel, is.na))))

  startsel <- init_population(Grid[[1]], n = 20, n_start = 100)
  expect_type(startsel, "list")
  expect_true(all(sapply(startsel, nrow) == 20))
  expect_true(all(sapply(startsel, ncol) == 4))
  expect_output(str(startsel), "List of 100")
  expect_false(any(unlist(sapply(startsel, is.na))))

  startsel <- init_population(Grid[[1]], n = 20, n_start = 300)
  expect_type(startsel, "list")
  expect_true(all(sapply(startsel, nrow) == 20))
  expect_true(all(sapply(startsel, ncol) == 4))
  expect_output(str(startsel), "List of 300")
  expect_false(any(unlist(sapply(startsel, is.na))))

  startsel <- init_population(Grid[[1]], n = 10, n_start = 20)
  expect_type(startsel, "list")
  expect_true(all(sapply(startsel, nrow) == 10))
  expect_true(all(sapply(startsel, ncol) == 4))
  expect_output(str(startsel), "List of 20")
  expect_false(any(unlist(sapply(startsel, is.na))))

  ## FITNESS ################################
  wind <- data.frame(ws = 12, wd = 0)
  wind <- list(wind, probab = 100)
  fit <- fitness(
    population = startsel, reference_height = 100, rotor_height = 100,
    surface_roughness = 0.3, area = area, rotor = 20,
    wind = wind, terrain = FALSE
  )
  expect_output(str(fit), "List of 20")
  expect_true(all(sapply(fit, nrow) == 10))
  expect_false(any(unlist(sapply(fit, is.na))))
  expect_false(any(unlist(do.call("rbind", fit)[, -c(1, 2)] < 0)))
  one <- do.call("rbind", lapply(fit, function(x) x[1, , drop = FALSE]))
  expect_equal(
    as.numeric(one[, "Parkfitness"]),
    as.numeric(one[, "EnergyOverall"] * (one[, "EfficAllDir"] / 100)),
    tolerance = 1e-8
  )

  fit1 <- fitness(
    population = startsel, reference_height = 100, rotor_height = 100,
    surface_roughness = 0.3, area = area, rotor = 20,
    wind = wind, terrain = FALSE
  )
  expect_output(str(fit1), "List of 20")
  expect_true(all(sapply(fit1, nrow) == 10))
  expect_false(any(unlist(sapply(fit1, is.na))))
  expect_false(any(unlist(do.call("rbind", fit1)[, -c(1, 2)] < 0)))
  rm(fit1)

  with_mocked_bindings(
    is_foreach_installed = function() FALSE,
    expect_error(
      fitness(
        population = startsel, reference_height = 100, rotor_height = 100,
        surface_roughness = 0.3, area = area, rotor = 20,
        wind = wind, terrain = FALSE, parallel = TRUE
      )
    )
  )

  ## SELECTION ################################
  allparks <- do.call("rbind", fit)
  grid_ids <- Grid[[1]][, "ID"]
  n_turb <- 10
  selec6best <- selection(fit, Grid[[1]], 2, TRUE, 6, "VAR")
  expect_selected_ids(selec6best, n_turb, grid_ids)
  rm(selec6best)


  allparks <- do.call("rbind", fit)
  selec6best <- selection(fit, Grid[[1]], 2, TRUE, 600, "VAR")
  expect_selected_ids(selec6best, n_turb, grid_ids)
  rm(selec6best)

  ## Produce error
  fitNA <- fit
  fitNA[[1]][, "Parkfitness"] <- NA
  a <- lapply(1:length(fitNA), function(i) {
    fitNA[[i]][, "Parkfitness"] <<- NA
  })
  rm(a)
  expect_error(selection(fitNA, Grid[[1]], 2, TRUE, 6, "VAR"))

  selec6best <- selection(fit, Grid[[1]], share = 1, TRUE, 6, "FIX")
  expect_selected_ids(selec6best, n_turb, grid_ids)
  rm(selec6best)

  selec6best <- selection(fit, Grid[[1]], 2, TRUE, 6, "FIX")
  expect_selected_ids(selec6best, n_turb, grid_ids)
  rm(selec6best)

  selec6best <- selection(fit, Grid[[1]], 2, TRUE, 6, "FIX")
  expect_selected_ids(selec6best, n_turb, grid_ids)
  rm(selec6best)

  selec6best <- quiet(selection(fit, Grid[[1]], 4, FALSE, 6,
    selection_mode = "VAR",
    verbose = TRUE
  ))
  expect_selected_ids(selec6best, n_turb, grid_ids)
  rm(selec6best)

  selec6best <- quiet(selection(fit, Grid[[1]], 4, FALSE, 6, "FIX",
    verbose = TRUE
  ))
  expect_selected_ids(selec6best, n_turb, grid_ids)

  selec6best <- quiet(selection(fit, Grid[[1]], 4, TRUE, 6, "FIX",
    verbose = TRUE
  ))
  expect_selected_ids(selec6best, n_turb, grid_ids)

  ## SET CROSSOVER / SWAP MUTATION #####################
  cross_ids <- set_crossover(
    selec6best[[1]], grid_ids, uplimit = 12, seed = 11, verbose = TRUE
  )
  expect_equal(nrow(cross_ids), n_turb)
  expect_equal(ncol(cross_ids), 12)
  expect_true(all(apply(cross_ids, 2, function(x) length(unique(x)) == n_turb)))
  expect_true(all(cross_ids %in% grid_ids))
  cross_ids1 <- set_crossover(selec6best[[1]], grid_ids, uplimit = 8, seed = 22)
  cross_ids2 <- set_crossover(selec6best[[1]], grid_ids, uplimit = 8, seed = 22)
  expect_identical(cross_ids1, cross_ids2)

  mut_ids <- swap_mutation(cross_ids, grid_ids, p = 0.4, seed = 7)
  expect_equal(dim(mut_ids), dim(cross_ids))
  expect_true(all(apply(mut_ids, 2, function(x) length(unique(x)) == n_turb)))
  expect_true(all(mut_ids %in% grid_ids))
  mut_ids1 <- swap_mutation(cross_ids, grid_ids, p = 0.4, seed = 7)
  expect_identical(mut_ids, mut_ids1)

  ## Unused cells must enter via crossover / min swaps
  ids_closed <- cbind(c(1L, 2L, 3L, 4L), c(1L, 2L, 5L, 6L))
  ch_open <- set_crossover(
    ids_closed, grid_ids = 1:20, uplimit = 30, seed = 3, p_inject = 0.5
  )
  expect_true(any(!as.vector(ch_open) %in% unique(as.vector(ids_closed))))

  ids_same <- cbind(c(1L, 2L, 3L, 4L), c(1L, 2L, 3L, 4L))
  ch_same <- set_crossover(ids_same, grid_ids = 1:20, uplimit = 4, seed = 9)
  expect_true(any(!as.vector(ch_same) %in% 1:4))

  mut_min <- swap_mutation(ids_closed, grid_ids = 1:20, p = 0, seed = 4, min_swaps = 1)
  expect_true(all(vapply(seq_len(ncol(mut_min)), function(j) {
    length(setdiff(mut_min[, j], ids_closed[, j])) >= 1
  }, logical(1))))

  gxy <- cbind(ID = 1:20, X = rep(1:5, 4), Y = rep(1:4, each = 5))
  ch_sp <- set_crossover(
    ids_closed, grid_ids = 1:20, uplimit = 8, seed = 12,
    grid_xy = gxy, p_spatial = 1
  )
  expect_equal(nrow(ch_sp), 4)
  expect_true(all(apply(ch_sp, 2, function(x) length(unique(x)) == 4)))

  vis <- stats::setNames(rep(1e6, 20), as.character(1:20))
  vis[c("18", "19", "20")] <- 0
  mut_w <- swap_mutation(
    matrix(1:4, ncol = 1), grid_ids = 1:20, p = 1, min_swaps = 4,
    visit = vis, seed = 8
  )
  expect_true(any(mut_w[, 1] %in% c(18L, 19L, 20L)))

  id_layouts <- get_grids(mut_ids, Grid[[1]])
  expect_type(id_layouts, "list")
  expect_true(all(sapply(id_layouts, nrow) == n_turb))
  expect_true(all(sapply(id_layouts, ncol) == 3))

  cache <- new.env(parent = emptyenv())
  sel2 <- list(id_layouts[[1]], id_layouts[[1]])
  fit_c <- windfarmGA:::fitness_with_cache(
    cache, sel2,
    reference_height = 100, rotor_height = 100,
    surface_roughness = 0.3, area = area, rotor = 20,
    wind = wind, terrain = FALSE
  )
  expect_length(ls(cache), 1)
  expect_equal(attr(fit_c, "n_new"), 1L)
  expect_equal(fit_c[[1]][1, "Parkfitness"], fit_c[[2]][1, "Parkfitness"])
  fit_c2 <- windfarmGA:::fitness_with_cache(
    cache, sel2,
    reference_height = 100, rotor_height = 100,
    surface_roughness = 0.3, area = area, rotor = 20,
    wind = wind, terrain = FALSE
  )
  expect_equal(attr(fit_c2, "n_new"), 0L)

  elite <- cbind(1:4, 2:5)
  worse <- cbind(10:13, 11:14)
  ekids <- windfarmGA:::elite_offspring(
    elite, worse, 1:20, n_mut = 2, n_mix = 1, mut_p = 1
  )
  expect_true(is.matrix(ekids))
  expect_equal(nrow(ekids), 4)
  expect_equal(ncol(ekids), 6)
  expect_true(all(apply(ekids, 2, function(x) length(unique(x)) == 4)))

  ## CROSSOVER (legacy binary) #####################
  selec6best <- selection_as_binary(selec6best, Grid[[1]])
  crossOut <- quiet(crossover(selec6best, 2,
    uplimit = 300, crossPart = "RAN",
    verbose = TRUE
  ))
  expect_output(str(crossOut), "num")
  expect_false(any(is.na(crossOut)))
  expect_true(all(crossOut %in% c(0, 1)))
  rm(crossOut)

  crossOut <- crossover(selec6best, 7, uplimit = 500, crossPart = "RAN")
  expect_output(str(crossOut), "num")
  expect_false(any(is.na(crossOut)))
  expect_true(all(crossOut %in% c(0, 1)))
  rm(crossOut)

  crossOut <- quiet(crossover(
    se6 = selec6best, u = 6, uplimit = 100,
    crossPart = "EQU", seed = 105, verbose = TRUE
  ))
  expect_output(str(crossOut), "num")
  expect_false(any(is.na(crossOut)))
  expect_true(all(crossOut %in% c(0, 1)))

  crossOut1 <- crossover(
    se6 = selec6best, u = 3, uplimit = 300,
    crossPart = "EQU", seed = 105
  )
  crossOut2 <- crossover(
    se6 = selec6best, u = 3, uplimit = 300,
    crossPart = "EQU", seed = 105
  )
  expect_true(all.equal(crossOut1, crossOut2, tolerance = 2))

  expect_output(str(crossOut1), "num")
  expect_false(any(is.na(crossOut1)))
  expect_true(all(crossOut1 %in% c(0, 1)))
  rm(crossOut, crossOut1)

  crossOut <- crossover(
    se6 = selec6best, u = 7, uplimit = 500,
    crossPart = "RAN", seed = 105
  )
  expect_output(str(crossOut), "num")
  expect_false(any(is.na(crossOut)))
  expect_true(all(crossOut %in% c(0, 1)))
  rm(crossOut)

  ## Produce error
  expect_error(crossover(
    se6 = selec6best, u = 7, uplimit = 500,
    crossPart = "something"
  ))

  crossOut <- crossover(selec6best, 3, uplimit = 300, crossPart = "EQU")
  expect_output(str(crossOut), "num")
  expect_false(any(is.na(crossOut)))
  expect_true(all(crossOut %in% c(0, 1)))


  ## MUTATION #####################
  ## Variable Mutation Rate is activated if more than 2 individuals represent the
  ## current best solution.
  mut <- mutation(a = crossOut, p = 0.3)
  expect_output(str(mut), "num")
  expect_false(any(is.na(mut)))
  expect_true(all(mut %in% c(0, 1)))

  mut <- mutation(a = crossOut, p = 0.1)
  expect_output(str(mut), "num")
  expect_false(any(is.na(mut)))
  expect_true(all(mut %in% c(0, 1)))

  mut <- mutation(a = crossOut, p = 1)
  expect_output(str(mut), "num")
  expect_false(any(is.na(mut)))
  expect_true(all(mut %in% c(0, 1)))

  mut <- mutation(a = crossOut, p = 100)
  expect_output(str(mut), "num")
  expect_false(any(is.na(mut)))
  expect_true(all(mut %in% c(0, 1)))

  mut <- mutation(a = crossOut, p = -1)
  expect_output(str(mut), "num")
  expect_false(any(is.na(mut)))
  expect_true(all(mut %in% c(0, 1)))

  mut <- mutation(a = crossOut, p = -1, seed = 104)
  mut1 <- mutation(a = crossOut, p = -1, seed = 104)
  expect_true(identical(mut, mut1))

  mut <- mutation(a = crossOut, p = 0.0005)
  expect_output(str(mut), "num")
  expect_false(any(is.na(mut)))
  expect_true(all(mut %in% c(0, 1)))

  ## TRIMTON #####################
  ## After Crossover and Mutation, the amount of turbines in a windpark change
  ## and have to be corrected to the required amount of turbines.
  mut1 <- trimton(
    mut = mut, nturb = 1, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = FALSE
  )
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == 1))
  expect_true(all(dim(mut) == dim(mut1)))
  rm(mut1)

  mut1 <- trimton(
    mut = mut, nturb = min(colSums(mut)), allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = FALSE
  )
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == min(colSums(mut))))
  expect_true(all(dim(mut) == dim(mut1)))
  rm(mut1)

  mut1 <- trimton(
    mut = mut, nturb = 10, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = FALSE
  )
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == 10))
  expect_true(all(dim(mut) == dim(mut1)))

  mut1 <- trimton(
    mut = mut, nturb = 5, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = TRUE
  )
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == 5))
  expect_true(all(dim(mut) == dim(mut1)))
  rm(mut1)

  mut1 <- trimton(
    mut = mut, nturb = 1, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = TRUE
  )
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == 1))
  expect_true(all(dim(mut) == dim(mut1)))
  rm(mut1)

  mut1 <- trimton(
    mut = mut, nturb = min(colSums(mut)), allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = TRUE
  )
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == min(colSums(mut))))
  expect_true(all(dim(mut) == dim(mut1)))
  rm(mut1)

  mut1 <- trimton(
    mut = mut, nturb = 20, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = TRUE
  )
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == 20))
  expect_true(all(dim(mut) == dim(mut1)))

  mut1 <- trimton(
    mut = mut, nturb = 20, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = TRUE, seed = 104
  )
  mut2 <- trimton(
    mut = mut, nturb = 20, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = TRUE, seed = 104
  )
  expect_true(identical(mut1, mut2))

  mut1 <- trimton(
    mut = mut, nturb = 20, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = FALSE, seed = 234
  )
  mut2 <- trimton(
    mut = mut, nturb = 20, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = FALSE, seed = 234
  )
  expect_true(identical(mut1, mut2))

  mut1 <- trimton(
    mut = mut, nturb = 5, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = TRUE, seed = 300
  )
  mut2 <- trimton(
    mut = mut, nturb = 5, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = TRUE, seed = 300
  )
  expect_true(identical(mut1, mut2))

  mut1 <- trimton(
    mut = mut, nturb = 5, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = FALSE, seed = 234
  )
  mut2 <- trimton(
    mut = mut, nturb = 5, allparks = allparks,
    nGrids = nrow(Grid[[1]]), trimForce = FALSE, seed = 234
  )
  expect_true(identical(mut1, mut2))

  ## GETRECTV #####################
  getRectV <- get_grids(mut1, Grid[[1]])
  expect_type(getRectV, "list")
  expect_true(all(sapply(getRectV, ncol) == 3))
  expect_false(any(unlist(sapply(getRectV, is.na))))
  expect_true(all(sapply(getRectV, colnames) %in% c("ID", "X", "Y")))


  ## FITNESS AGAIN #####################
  fit <- fitness(
    population = getRectV, reference_height = 100, rotor_height = 100,
    surface_roughness = 0.3, area = area, rotor = 20,
    wind = wind, terrain = FALSE
  )
  expect_type(fit, "list")
  expect_true(length(fit) == length(getRectV))
  expect_false(any(unlist(sapply(fit, is.na))))
  expect_false(any(unlist(do.call("rbind", fit)[, -c(1, 2)] < 0)))
})
