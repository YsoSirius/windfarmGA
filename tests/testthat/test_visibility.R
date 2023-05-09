context("Visibility Tests")
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
  expect_is(plt, "SpatRaster")
  expect_true(all(range(values(plt, na.rm = TRUE)) %in% c(0, 1)))

  locs <- st_sample(shape, 10, type = "random")
  locs <- st_coordinates(locs)
  plt <- plot_viewshed(x, locs, h1 = 0, h2 = 0, plot = TRUE)
  expect_is(plt, "SpatRaster")
  expect_true(all(range(values(plt, na.rm = TRUE)) %in% c(0, 1)))

  locs <- st_sample(shape, 10, type = "random")
  locs <- as(locs, "Spatial")
  plt <- plot_viewshed(x, locs, h1 = 0, h2 = 0, plot = TRUE)
  expect_is(plt, "SpatRaster")
  expect_true(all(range(values(plt, na.rm = TRUE)) %in% c(0, 1)))
})
