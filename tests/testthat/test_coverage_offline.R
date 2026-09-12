## Coverage that must run on CI (no elevatr / CLC download).

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
    resolution = res,
    crs = terra::crs(area)
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

test_that("package_installed helpers return a boolean", {
  expect_type(is_foreach_installed(), "logical")
  expect_type(is_parallel_installed(), "logical")
  expect_type(is_doparallel_installed(), "logical")
  expect_type(is_ggplot2_installed(), "logical")
  expect_type(is_leaflet_installed(), "logical")
  expect_type(is_elevatr_installed(), "logical")
  expect_type(is_plotly_installed(), "logical")
  expect_type(is_shiny_installed(), "logical")
})

test_that("explore_result stops without shiny", {
  with_mocked_bindings(
    is_shiny_installed = function() FALSE,
    expect_error(explore_result(resultrect, sp_polygon), "shiny")
  )
})

test_that("terrain_model works from synthetic rasters (no download)", {
  area <- synth_site()
  dem <- synth_dem(area)
  ccl <- synth_ccl(area)
  res <- terrain_model(dem, area, ccl = ccl, plot = TRUE, verbose = TRUE)
  expect_named(res, c("srtm_crop", "cclRaster"))
  expect_length(res$srtm_crop, 3)
  expect_s4_class(res$cclRaster, "SpatRaster")
  expect_s4_class(res$srtm_crop[[1]], "SpatRaster")

  na_dem <- dem
  terra::values(na_dem) <- NA
  expect_warning(terrain_model(na_dem, area, ccl = ccl, plot = FALSE))
})

test_that("calculate_energy terrain and weibull paths (offline)", {
  area <- synth_site()
  tm <- terrain_model(synth_dem(area), area, ccl = synth_ccl(area), plot = FALSE)
  grid <- grid_area(area, size = 180, prop = 1, plot_grid = FALSE)
  pop <- init_population(grid[[1]], n = 8, n_start = 4)
  wind <- data.frame(ws = 8, wd = 0)

  en_t <- calculate_energy(
    layout = pop[[1]],
    reference_height = 50,
    rotor_height = 80,
    surface_roughness = 0.14,
    wake_angle = 20,
    wake_distance = 100000,
    wind = wind,
    rotor = 30,
    area = area,
    elevation = tm$srtm_crop,
    terrain = TRUE,
    ccl_raster = tm$cclRaster,
    weibull = FALSE,
    plot = TRUE
  )
  expect_type(en_t, "list")
  df <- do.call(rbind, en_t)
  expect_true(all(df[, "Energy_Output_Red"] > 0))

  speed <- tm$srtm_crop[[1]]
  terra::values(speed) <- 7
  en_w <- calculate_energy(
    layout = pop[[1]],
    reference_height = 50,
    rotor_height = 80,
    surface_roughness = 0.14,
    wake_angle = 20,
    wake_distance = 100000,
    wind = wind,
    rotor = 30,
    area = area,
    terrain = FALSE,
    weibull = speed,
    park_center = c(4499200, 2668800),
    plot = TRUE
  )
  expect_type(en_w, "list")

  hole <- speed
  terra::values(hole) <- NA
  terra::values(hole)[seq_len(10)] <- 6
  en_na <- calculate_energy(
    layout = pop[[1]],
    reference_height = 50,
    rotor_height = 80,
    surface_roughness = 0.14,
    wake_angle = 20,
    wake_distance = 100000,
    wind = wind,
    rotor = 30,
    area = area,
    terrain = FALSE,
    weibull = hole,
    plot = FALSE
  )
  expect_type(en_na, "list")
})

test_that("wind helper error paths and CSV read", {
  expect_error(wind_from_uv(1:2, 1), "same length")
  expect_error(wind_from_uv(NA_real_, NA_real_), "No finite")
  expect_error(wind_from_series(1:3, 1:2), "same length")
  expect_error(wind_from_series(1, 10, dir_width = 0), "dir_width")
  expect_error(wind_from_series(NA_real_, NA_real_), "No finite")

  rose <- wind_from_series(c(5, 6, NA), c(10, 20, 30), dir_width = 30)
  expect_equal(sum(rose$probab), 100)

  tmp <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(`Wind Speed [m/s]` = c(3, 8), `Power [kW]` = c(0, 900), check.names = FALSE),
    tmp,
    row.names = FALSE
  )
  curve <- read_power_curve(tmp)
  expect_equal(curve$ws, c(3, 8))
  expect_error(read_power_curve(data.frame(foo = 1)), "wind-speed")
  expect_error(read_power_curve(data.frame()), "Empty")
})
