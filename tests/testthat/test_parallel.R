
test_that("Test Parallelisation", {
  skip_on_cran()
  skip_on_ci()

  ## Inputs ##################
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)
    ))),
    crs = 3035
  ))
  wind <- data.frame(ws = 12, wd = 0)

  ## Mock Packages not installed ###########
  with_mocked_bindings(
    is_foreach_installed = function() FALSE,
    expect_error(
      genetic_algorithm(
        Polygon1 = Polygon1,
        n = 12, iteration = 2,
        vdirspe = wind,
        Rotor = 30,
        RotorHeight = 100, Parallel = TRUE
      )
    )
  )
  with_mocked_bindings(
    is_parallel_installed = function() FALSE,
    expect_error(
      genetic_algorithm(
        Polygon1 = Polygon1,
        n = 12, iteration = 2,
        vdirspe = wind,
        Rotor = 30,
        RotorHeight = 100, Parallel = TRUE
      )
    )
  )
  with_mocked_bindings(
    is_doparallel_installed = function() FALSE,
    expect_error(
      genetic_algorithm(
        Polygon1 = Polygon1,
        n = 12, iteration = 2,
        vdirspe = wind,
        Rotor = 30,
        RotorHeight = 100, Parallel = TRUE
      )
    )
  )

  ## genetic_algorithm ####################
  ## Default amount of Cluster
  res <- genetic_algorithm(
    Polygon1 = Polygon1,
    n = 12, iteration = 2,
    vdirspe = wind,
    Rotor = 30,
    RotorHeight = 100, Parallel = TRUE
  )
  expect_true(nrow(res) == 2)
  expect_true(is.matrix(res))
  expect_false(any(unlist(sapply(res, is.na))))

  ## Too many Cluster
  # skip("Too many clusters")
  res <- expect_warning(
    genetic_algorithm(
      Polygon1 = Polygon1,
      n = 12, iteration = 2,
      vdirspe = wind,
      Rotor = 30,
      RotorHeight = 100,
      Parallel = TRUE, numCluster = 100
    )
  )
  res <- suppressWarnings(
    genetic_algorithm(
      Polygon1 = Polygon1,
      n = 12, iteration = 2,
      vdirspe = wind,
      Rotor = 30,
      RotorHeight = 100,
      Parallel = TRUE, numCluster = 100
    )
  )
  expect_true(nrow(res) == 2)
  expect_true(is.matrix(res))
  expect_false(any(unlist(sapply(res, is.na))))
})
