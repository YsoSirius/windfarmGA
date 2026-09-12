library(terra)
library(sf)

test_that("Test Viewshed Functions", {

  f <- system.file("ex/elev.tif", package = "terra")
  r <- rast(f)
  x <- project(r, "EPSG:2169")
  shape <- sf::st_as_sf(as.polygons(terra::boundaries(x)))
  st_crs(shape) <- 2169
  locs <- st_sample(shape, 10, type = "random")

  ## plot_viewshed #################
  plt <- plot_viewshed(x, locs, h1 = 0, h2 = 0, plot = TRUE)
  expect_s4_class(plt, "SpatRaster")
  expect_true(all(range(values(plt, na.rm = TRUE)) %in% c(0, 1)))

  locs <- st_sample(shape, 10, type = "random")
  locs <- st_coordinates(locs)
  plt <- plot_viewshed(x, locs, h1 = 0, h2 = 0, plot = TRUE)
  expect_s4_class(plt, "SpatRaster")
  expect_true(all(range(values(plt, na.rm = TRUE)) %in% c(0, 1)))

  locs <- st_sample(shape, 10, type = "random")
  locs <- as(locs, "Spatial")
  plt <- plot_viewshed(x, locs, h1 = 0, h2 = 0, plot = TRUE)
  expect_s4_class(plt, "SpatRaster")
  expect_true(all(range(values(plt, na.rm = TRUE)) %in% c(0, 1)))

  ## lon/lat DEM: terra::viewshed needs metres; project first
  r_ll <- terra::project(x, "EPSG:4326")
  locs_ll <- sf::st_transform(sf::st_as_sf(st_sample(shape, 5, type = "random")), 4326)
  plt_ll <- plot_viewshed(r_ll, locs_ll, h1 = 0, h2 = 0, plot = FALSE)
  expect_s4_class(plt_ll, "SpatRaster")
  expect_false(isTRUE(terra::is.lonlat(plt_ll)))

  ## filename + per-turbine observer height (same CRS as the file)
  f <- system.file("ex/elev.tif", package = "terra")
  r0 <- terra::rast(f)
  locs_f <- terra::xyFromCell(r0, c(50L, 120L, 200L))
  plt_f <- plot_viewshed(f, locs_f, h1 = c(0, 15, 30), h2 = 1.7, plot = FALSE)
  expect_s4_class(plt_f, "SpatRaster")

  ## southern hemisphere → UTM south
  r_s <- terra::rast(
    xmin = -71, xmax = -70, ymin = -34, ymax = -33,
    nrows = 12, ncols = 12, crs = "EPSG:4326"
  )
  terra::values(r_s) <- 200 + seq_len(terra::ncell(r_s))
  plt_s <- plot_viewshed(r_s, cbind(-70.5, -33.5), h1 = 10, h2 = 0, plot = FALSE)
  expect_s4_class(plt_s, "SpatRaster")
  expect_false(isTRUE(terra::is.lonlat(plt_s)))
})
