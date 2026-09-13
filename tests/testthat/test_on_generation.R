test_that("on_generation is called once per finished generation", {
  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(0, 0, 2000, 2000, 0),
      c(0, 2000, 2000, 0, 0)
    ))),
    crs = 3035
  ))
  gens <- integer()
  energies <- numeric()
  res <- suppressMessages(genetic_algorithm(
    area = area,
    wind = data.frame(ws = 12, wd = 0),
    n = 4,
    rotor = 30,
    rotor_height = 80,
    iteration = 2,
    verbose = FALSE,
    on_generation = function(generation, iteration, energy, efficiency, fitness) {
      gens <<- c(gens, generation)
      energies <<- c(energies, as.numeric(energy))
    }
  ))
  expect_s3_class(res, "windfarmGA")
  expect_equal(gens, seq_len(nrow(res)))
  expect_true(all(is.finite(energies)))
})

test_that("wake_cones returns one polygon per turbine and direction", {
  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(0, 0, 2000, 2000, 0),
      c(0, 2000, 2000, 0, 0)
    ))),
    crs = 3035
  ))
  xy <- cbind(c(500, 1000), c(500, 1500))
  wind <- data.frame(ws = c(10, 8), wd = c(0, 90), probab = c(60, 40))
  cones <- wake_cones(xy, wind, rotor = 40, area = area)
  expect_s3_class(cones, "sf")
  expect_equal(nrow(cones), 4L)
  expect_true(all(c("turb", "wd", "prob", "farbe") %in% names(cones)))
})
