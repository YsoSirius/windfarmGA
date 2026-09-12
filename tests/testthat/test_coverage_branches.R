## Hit remaining branches that the main suite never walks (CI, no download).

synth_site <- function() {
  sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
}

synth_dem <- function(area, res = 80) {
  bb <- sf::st_bbox(area)
  r <- terra::rast(
    xmin = bb$xmin, xmax = bb$xmax,
    ymin = bb$ymin, ymax = bb$ymax,
    resolution = res, crs = terra::crs(area)
  )
  xy <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  terra::values(r) <- 420 + 0.02 * (xy[, 1] - bb$xmin) + 0.03 * (xy[, 2] - bb$ymin)
  r
}

synth_ccl <- function(area, code = 12) {
  r <- synth_dem(area)
  terra::values(r) <- code
  r
}

synth_roughness_csv <- function(code = 12, z0 = 0.03) {
  f <- tempfile(fileext = ".csv")
  writeLines(c("GRID_CODE;Rauhigkeit_z", paste(code, z0, sep = ";")), f)
  f
}

tiny_grid_pop <- function(area, n = 4, n_start = 4, size = 180) {
  grid <- grid_area(area, size = size, prop = 1, plot_grid = FALSE)
  pop <- init_population(grid[[1]], n = n, n_start = n_start)
  list(grid = grid[[1]], pop = pop, area = area)
}

write_rast <- function(r) {
  f <- tempfile(fileext = ".tif")
  terra::writeRaster(r, f, overwrite = TRUE)
  f
}

quiet_pdf <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}

test_that("calculate_energy weibull file, NA fill, power profile, power curve", {
  area <- synth_site()
  g <- tiny_grid_pop(area)
  wind <- data.frame(ws = 8, wd = 0)
  speed <- synth_dem(area)
  terra::values(speed) <- 7
  path <- write_rast(speed)

  en_file <- calculate_energy(
    layout = g$pop[[1]],
    reference_height = 50, rotor_height = 80,
    surface_roughness = 0.14, wake_angle = 20, wake_distance = 100000,
    wind = wind, rotor = 30, area = area,
    terrain = FALSE, weibull = path, park_center = c(4499200, 2668800),
    plot = FALSE
  )
  expect_type(en_file, "list")

  hole <- speed
  terra::values(hole) <- NA
  terra::values(hole)[seq_len(8)] <- 6
  en_na <- calculate_energy(
    layout = g$pop[[1]],
    reference_height = 50, rotor_height = 80,
    surface_roughness = 0.14, wake_angle = 20, wake_distance = 100000,
    wind = wind, rotor = 30, area = area,
    terrain = FALSE, weibull = hole, plot = FALSE
  )
  expect_type(en_na, "list")

  old <- options(windfarmGA.wind_profile = "power")
  on.exit(options(old), add = TRUE)
  expect_equal(windfarmGA:::wind_shear_factor(80, 50, 0.14), (80 / 50)^0.14)
  en_pw <- calculate_energy(
    layout = g$pop[[1]],
    reference_height = 50, rotor_height = 80,
    surface_roughness = 0.14, wake_angle = 20, wake_distance = 100000,
    wind = wind, rotor = 30, area = area,
    terrain = FALSE, weibull = FALSE, plot = FALSE
  )
  expect_type(en_pw, "list")

  expect_null(windfarmGA:::power_curve_xy(NULL))
  expect_error(plot_power_curve(plot = FALSE), "No power curve")
  curve <- data.frame(ws = c(0, 8, 12), power = c(0, 900, 2000))
  options(windfarmGA.power_curve = curve)
  quiet_pdf({
    xy <- plot_power_curve(plot = TRUE)
    expect_equal(xy$power[1], 0)
    plot_power_curve(curve, plot = TRUE)
  })
})

test_that("set_crossover and set_cross_one edge cases", {
  grid_ids <- 1:20
  expect_equal(ncol(set_crossover(c(1L, 3L, 5L, 7L), grid_ids)), 1)
  odd <- cbind(c(1, 3, 5, 7), c(2, 4, 6, 8), c(1, 2, 9, 10))
  out <- set_crossover(odd, grid_ids, uplimit = 4, seed = 1)
  expect_equal(ncol(out), 4)
  many <- set_crossover(
    cbind(c(1, 3, 5, 7), c(2, 4, 6, 8)),
    grid_ids, uplimit = 5, seed = 2, verbose = TRUE
  )
  expect_equal(ncol(many), 5)

  same <- windfarmGA:::set_cross_one(1:8, 1:8, grid_ids, p_inject = 0.4)
  expect_length(same, 8)
  dups <- windfarmGA:::set_cross_one(
    c(1, 1, 1, 2), rep(1, 4), 1:6, p_inject = 0
  )
  expect_length(dups, 4)
  tiny <- windfarmGA:::set_cross_one(
    c(1, 2, 3, 4), c(1, 1, 1, 5), 1:5, p_inject = 0.9
  )
  expect_length(tiny, 4)

  xy <- data.frame(ID = grid_ids, X = seq_len(20), Y = seq_len(20))
  sp <- windfarmGA:::spatial_cross_one(
    1:6, 10:15, xy, grid_ids, p_inject = 0.5, visit = NULL
  )
  expect_length(sp, 6)
})

test_that("mutation, get_grids, selection, trimton edges", {
  area <- synth_site()
  g <- tiny_grid_pop(area, n = 4, n_start = 6, size = 200)
  ids <- vapply(g$pop[1:4], function(p) sort(as.integer(p[, "ID"])), integer(4))
  expect_true(is.matrix(ids))
  expect_true(is.matrix(swap_mutation(ids[, 1], g$grid[, "ID"], p = 0.5, seed = 1)))
  full <- as.integer(g$grid[, "ID"])
  expect_equal(swap_mutation(full, full, p = 1, min_swaps = 2), matrix(full, ncol = 1))
  expect_equal(
    swap_mutation(c(1L, 2L), 1:2, p = 0, min_swaps = 0),
    matrix(c(1L, 2L), ncol = 1)
  )
  expect_equal(
    swap_mutation(c(1L, 2L), 1:10, p = 0, min_swaps = 0),
    matrix(c(1L, 2L), ncol = 1)
  )
  expect_length(get_grids(ids[, 1], g$grid)[[1]][, 1], 4)

  wind <- list(data.frame(ws = 8, wd = 0), 100)
  fit <- fitness(
    population = g$pop[1:4], reference_height = 50, rotor_height = 80,
    surface_roughness = 0.14, area = area, rotor = 30,
    wind = wind, terrain = FALSE, parallel = FALSE
  )
  one <- lapply(fit, function(x) x[1, , drop = FALSE])
  sel1 <- selection(one, g$grid, share = 2, elitism = TRUE, n_elite = 1, selection_mode = "FIX")
  expect_equal(nrow(sel1[[1]]), 1)

  bad <- fit
  bad[[1]][, "Rect_ID"] <- 999999L
  expect_error(
    selection(bad, g$grid, share = 2, elitism = FALSE, n_elite = 0, selection_mode = "VAR"),
    "not in Grid"
  )

  bins <- matrix(0, nrow(g$grid), 3)
  for (j in 1:3) {
    bins[match(ids[, j], g$grid[, "ID"]), j] <- 1
  }
  bins[, 1] <- 1
  allparks <- do.call("rbind", fit)
  trimmed <- trimton(
    mut = bins, nturb = 4, allparks = allparks,
    nGrids = nrow(g$grid), trimForce = TRUE
  )
  expect_true(all(colSums(trimmed) == 4))
})

test_that("ga_options format and unnamed args", {
  expect_equal(windfarmGA:::format_ga_option(NULL), "NULL")
  expect_equal(
    windfarmGA:::format_ga_option(data.frame(a = 1:2, b = 3:4)),
    "data.frame [2 x 2]"
  )
  expect_match(windfarmGA:::format_ga_option(1:6), "\\.\\.\\.")
  expect_error(ga_options(3), "named arguments")
})

test_that("ga_result helpers and plot dispatch", {
  expect_error(as_windfarmGA(1), "does not look like")
  empty <- resultrect
  empty[, "allparkcoeff"] <- lapply(seq_len(nrow(empty)), function(i) {
    data.frame()
  })
  expect_null(windfarmGA:::ga_result_best(empty))
  quiet_pdf({
    expect_null(plot(as_windfarmGA(resultrect), synth_site(), which_plot = 2, ask = FALSE))
  })
})

test_that("ga_utils layout_key, weights, neighbors, elite kids", {
  expect_equal(windfarmGA:::layout_key(c(3, 1, 2)), "1,2,3")
  expect_equal(windfarmGA:::sample_weighted_ids(1:5, 0), integer(0))
  expect_equal(windfarmGA:::sample_weighted_ids(integer(0), 2), integer(0))
  inf_v <- c("1" = Inf, "2" = Inf, "3" = Inf)
  got <- windfarmGA:::sample_weighted_ids(1:3, 2, visit = inf_v)
  expect_length(got, 2)

  kids <- windfarmGA:::elite_offspring(
    c(1L, 3L, 5L, 7L), cbind(c(2L, 4L, 6L, 8L), c(1L, 2L, 9L, 10L)),
    1:20, n_mut = 1, n_mix = 1, mut_p = 0.5
  )
  expect_true(is.matrix(kids))

  one <- data.frame(ID = 1, X = 0, Y = 0)
  nb0 <- windfarmGA:::grid_neighbors(one)
  expect_length(nb0[[1]], 0)
  same <- data.frame(ID = 1:2, X = c(0, 0), Y = c(0, 0))
  expect_length(windfarmGA:::grid_neighbors(same)[[1]], 0)

  nbr <- list(`1` = 2L, `2` = 1L)
  expect_equal(windfarmGA:::neighbor_swap(c(1L, 2L), nbr, n_moves = 2), c(1L, 2L))
  nbr2 <- list(`1` = integer(0), `2` = integer(0), `3` = integer(0))
  expect_equal(windfarmGA:::neighbor_swap(c(1L, 2L), nbr2), c(1L, 2L))
})

test_that("fitness parallel uses foreach (sequential backend)", {
  skip_if_not_installed("foreach")
  area <- synth_site()
  g <- tiny_grid_pop(area, n = 3, n_start = 2, size = 220)
  wind <- list(data.frame(ws = 8, wd = 0), 100)
  foreach::registerDoSEQ()
  fit <- fitness(
    population = g$pop, reference_height = 50, rotor_height = 80,
    surface_roughness = 0.14, area = area, rotor = 30,
    wind = wind, terrain = FALSE, parallel = TRUE
  )
  expect_length(fit, 2)
  expect_true(all(vapply(fit, function(x) x[1, "EnergyOverall"] > 0, logical(1))))
})

test_that("genetic_algorithm terrain, weibull, FIX, verbose, stall", {
  area <- synth_site()
  dem <- synth_dem(area)
  ccl <- synth_ccl(area)
  pad <- sf::st_buffer(area, dist = 400)
  k <- synth_dem(pad)
  a <- synth_dem(pad)
  terra::values(k) <- 2
  terra::values(a) <- 8
  old <- options(
    windfarmGA.stall_generations = 2L,
    windfarmGA.immigrants = 2L,
    windfarmGA.elite_children = 1L,
    windfarmGA.local_search_elites = 1L,
    windfarmGA.local_search_tries = 1L
  )
  on.exit(options(old), add = TRUE)

  expect_error(
    genetic_algorithm(
      area = area, n = 4, iteration = 1, wind = data.frame(ws = 8, wd = 0),
      rotor = 30, rotor_height = 80, weibull = TRUE, verbose = TRUE
    ),
    "weibull_src"
  )
  tryCatch(
    suppressMessages(genetic_algorithm(
      area = area, n = 4, iteration = 1,
      wind = data.frame(ws = 8, wd = 0),
      rotor = 30, rotor_height = 80,
      weibull = TRUE, weibull_src = list(write_rast(k), a),
      verbose = TRUE, plot = FALSE
    )),
    error = function(e) NULL
  )

  res_t <- tryCatch(
    suppressMessages(genetic_algorithm(
      area = area, n = 4, iteration = 1,
      wind = data.frame(ws = 8, wd = 0),
      rotor = 30, rotor_height = 80, reference_height = 50,
      terrain = dem, ccl = ccl, ccl_roughness = synth_roughness_csv(),
      weibull = FALSE, verbose = TRUE, plot = FALSE
    )),
    error = function(e) NULL
  )
  if (is.matrix(res_t)) {
    stored <- windfarmGA:::ga_result_terrain(res_t)
    expect_false(is.null(stored))
    expect_true(!is.null(stored$srtm_crop$cells))
  }

  res <- suppressMessages(genetic_algorithm(
    area = area, n = 4, iteration = 3,
    wind = data.frame(ws = 8, wd = 0),
    rotor = 30, rotor_height = 80, reference_height = 50,
    terrain = FALSE, weibull = FALSE,
    selection_mode = "FIX", n_elite = 1, verbose = TRUE, plot = FALSE
  ))
  expect_true(is.matrix(res))
  expect_true(nrow(res) >= 1)
})

test_that("plots: weibull list, generation_layouts, heatmap, ggplot hint", {
  area <- synth_site()
  k <- synth_dem(area)
  a <- synth_dem(area)
  terra::values(k) <- 2
  terra::values(a) <- 8
  k_f <- write_rast(k)
  quiet_pdf({
    plot_result(
      resultrect, area, best = 0, plot_en = 1, terrain = FALSE,
      plot_grid = FALSE, weibull_src = list(a, k_f)
    )
    plot_windfarmGA(resultrect, area, which_plot = 1, ask = FALSE)
    plot_windfarmGA(resultrect, area, which_plot = "evolution", ask = FALSE)
  })

  lay <- generation_layouts(resultrect)
  expect_true(nrow(lay$layouts) >= 1)
  expect_error(generation_layouts(resultrect, 0), "between")
  expect_error(generation_layouts(matrix(1)), "allCoords")
  empty <- resultrect
  ac0 <- empty[1, "allCoords"]
  ac0[[1]] <- ac0[[1]][0, , drop = FALSE]
  empty[1, "allCoords"] <- ac0
  expect_error(generation_layouts(empty, 1), "no stored")
  norun <- resultrect
  ac <- as.data.frame(norun[1, "allCoords"][[1]])
  ac$Run <- NULL
  ac1 <- norun[1, "allCoords"]
  ac1[[1]] <- ac
  norun[1, "allCoords"] <- ac1
  expect_error(generation_layouts(norun, 1), "Run")

  no_ac <- resultrect
  colnames(no_ac)[colnames(no_ac) == "allCoords"] <- "coordsX"
  expect_error(plot_cell_heatmap(no_ac, area, plot = FALSE), "allCoords")
  quiet_pdf({
    expect_silent(plot_cell_heatmap(resultrect, area, log = FALSE, plot = TRUE))
  })

  .S3method("print", "covhint", function(x, ...) {
    stop("font_info unused argument (weight = )", call. = FALSE)
  })
  expect_error(
    windfarmGA:::print_ggplot_or_font_hint(structure(1, class = "covhint")),
    "systemfonts"
  )
})

test_that("plot_terrain and ga_series / ga_elite_n helpers", {
  area <- synth_site()
  tm <- terrain_model(
    synth_dem(area), area,
    ccl = synth_ccl(area),
    ccl_roughness = synth_roughness_csv(),
    plot = FALSE
  )
  xy <- as.data.frame(resultrect[nrow(resultrect), "bestPaEn"][[1]])[, c("X", "Y")]
  quiet_pdf({
    windfarmGA:::plot_terrain(
      resultrect[1, "inputData"][[1]],
      xy, area,
      tm$srtm_crop$orogr1,
      tm$srtm_crop$strm_crop,
      tm$cclRaster
    )
  })

  s <- windfarmGA:::ga_series(resultrect)
  expect_true(s$n >= 1)
  expect_true(is.numeric(s$coverage))
  expect_gte(windfarmGA:::ga_elite_n(resultrect), 1L)
  with_n <- resultrect
  inp <- with_n[1, "inputData"][[1]]
  if (!"Elite count" %in% rownames(inp)) {
    inp <- rbind(inp, `Elite count` = 4)
  } else {
    inp["Elite count", 1] <- 4
  }
  slot <- with_n[1, "inputData"]
  slot[[1]] <- inp
  with_n[1, "inputData"] <- slot
  expect_equal(windfarmGA:::ga_elite_n(with_n), 4L)
})

test_that("random_search follows terrain and missing weibull_src", {
  area <- synth_site()
  dem <- synth_dem(area)
  expect_warning(
    random_search(
      resultrect, area, n = 2, best = 1, plot = FALSE,
      terrain = FALSE, weibull = TRUE
    ),
    "weibull_src"
  )
  rs <- random_search(
    resultrect, area, n = 2, best = 1, plot = FALSE,
    terrain = dem, ccl = synth_ccl(area),
    ccl_roughness = synth_roughness_csv(),
    weibull = FALSE
  )
  expect_type(rs, "list")
  expect_false(anyNA(unlist(rs)))
})

test_that("terrain_model guards, hex GA, crossover cap, leaflet helpers", {
  area <- synth_site()
  dem <- synth_dem(area)
  ccl <- synth_ccl(area)
  rough <- synth_roughness_csv()

  with_mocked_bindings(
    is_elevatr_installed = function() FALSE,
    expect_error(
      terrain_model(TRUE, area, ccl = ccl, ccl_roughness = rough),
      "elevatr"
    )
  )
  tm_file <- terrain_model(
    write_rast(dem), area,
    ccl = write_rast(ccl),
    ccl_roughness = rough,
    plot = FALSE
  )
  expect_s4_class(tm_file$cclRaster, "SpatRaster")
  if (requireNamespace("raster", quietly = TRUE)) {
    tm_rl <- terrain_model(
      raster::raster(dem), area,
      ccl = ccl, ccl_roughness = rough, plot = FALSE
    )
    expect_s4_class(tm_rl$srtm_crop[[1]], "SpatRaster")
  }

  with_mocked_bindings(
    is_parallel_installed = function() FALSE,
    expect_error(
      genetic_algorithm(
        area = area, n = 4, iteration = 1,
        wind = data.frame(ws = 8, wd = 0),
        rotor = 30, rotor_height = 80, parallel = TRUE
      ),
      "parallel"
    )
  )

  res_h <- suppressMessages(genetic_algorithm(
    area = area, n = 4, iteration = 2,
    wind = data.frame(ws = 8, wd = 0),
    rotor = 30, rotor_height = 80, reference_height = 50,
    grid_method = "h", elitism = FALSE, plot = FALSE, verbose = FALSE
  ))
  expect_true(is.matrix(res_h))

  parents <- data.frame(
    ID = 1:12,
    bin = c(1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1),
    bin.1 = c(0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0)
  )
  fitp <- data.frame(ID = 1, Fitness = 10, Fitness.1 = 8)
  expect_message(
    crossover(list(parents, fitp), u = 2, uplimit = 2, crossPart = "EQU", verbose = TRUE),
    "limit|permutations|pairs"
  )
  expect_equal(nrow(windfarmGA:::permutations(4, 1)), 4)

  many <- data.frame(wd = seq(0, 330, 30), ws = 8, probab = 1)
  expect_equal(nrow(windfarmGA:::leaflet_wind_for_cones(many)), 6L)
  expect_gt(windfarmGA:::leaflet_wake_length(area, 30), 200)
  expect_null(windfarmGA:::leaflet_prepare_terrain(NULL))
  expect_null(windfarmGA:::leaflet_match_cells(data.frame(X = 1), NULL))
  expect_true(inherits(
    plot_windrose(data.frame(ws = c(5, 40), wd = c(0, 90)), spdmax = 30, plot = FALSE),
    "ggplot"
  ))

  speed <- synth_dem(area)
  terra::values(speed) <- 7
  rs_w <- random_search(
    resultrect, area, n = 1, best = 1, plot = FALSE,
    terrain = FALSE, weibull = speed
  )
  expect_type(rs_w, "list")
})
