context("Visibility Tests")
library(testthat)
library(sp)
# library(windfarmGA)
library(raster)
library(sf)

test_that("Test Viewshed Functions", {
  skip_on_cran()
  
  ## Input Data #####################
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4496482, 4496482, 4499991, 4499991, 4496482),
      c(2666272, 2669343, 2669343, 2666272, 2666272)))),
    crs = 3035
  ))
  # plot(Polygon1)

  ## getDEM #################
  DEM_meter <- expect_warning(getDEM(Polygon1))
  expect_is(DEM_meter, "list")
  expect_true(class(DEM_meter[[1]]) == "RasterLayer")
  expect_true(class(DEM_meter[[2]])[[1]] == "sf")
  
  DEM_noclip <- expect_warning(getDEM(Polygon1, clip = FALSE))
  expect_is(DEM_noclip, "list")
  expect_true(class(DEM_noclip[[1]]) == "RasterLayer")
  expect_true(is.null(DEM_noclip[[2]]))
  
  ## getISO3 ###############
  points <- cbind(c(4488182.26267016, 4488852.91748256),
                  c(2667398.93118627, 2667398.93118627))
  res <- getISO3(pp = points, crs_pp = 3035)
  expect_true(res == "AUT")
  expect_true(nrow(res) == 1)
  expect_true(ncol(res) == 1)
  expect_is(res, "data.frame")
  
  points <- as.data.frame(points)
  colnames(points) <- c("x","y")
  points <- st_as_sf(points, coords = c("x","y"))
  st_crs(points) <- 3035
  ressf <- getISO3(pp = points, crs_pp = 3035)
  expect_true(ressf == "AUT")
  expect_true(nrow(ressf) == 1)
  expect_true(ncol(ressf) == 1)
  expect_is(ressf, "data.frame")

  ressf <- getISO3(pp = points, crs_pp = 3035, col = c("ABBREV", "ADMIN", "LON", "LAT"))
  expect_true("ADMIN" %in% colnames(ressf))
  expect_true(ressf$ADMIN == "Austria")
  expect_true(nrow(ressf) == 1)
  expect_true(ncol(ressf) == 4)
  expect_is(ressf, "data.frame")
  
  
  ## viewshed #################
  turbloc <- st_sample(DEM_meter[[2]], 10, type = "random")
  res <- viewshed(r = DEM_meter[[1]], shape = DEM_meter[[2]],
                  turbine_locs = turbloc,  h1 = 1.8, h2 = 50)
  expect_is(res, "list")
  expect_true(length(res) == 5)
  expect_true(is.logical(as.vector(res[[1]])))
  expect_false(anyNA(res[[2]]))
  expect_true(class(res[[3]])[[1]] == "sf")
  expect_true(class(res[[4]]) == "RasterLayer")
  expect_false(anyNA(res[[5]]))
  
  turblocdf <- sp::coordinates(turbloc)
  sf_shape <- st_as_sf(DEM_meter[[2]])
  res <- suppressWarnings(viewshed(r = DEM_meter[[1]], shape = sf_shape,
                  turbine_locs = turblocdf,  h1 = 1.8, h2 = 50))
  expect_is(res, "list")
  expect_true(length(res) == 5)
  expect_true(is.logical(as.vector(res[[1]])))
  expect_false(anyNA(res[[2]]))
  expect_true(class(res[[3]])[[1]] == "sf")
  expect_true(class(res[[4]]) == "RasterLayer")
  expect_false(anyNA(res[[5]]))
  
  res <- suppressWarnings(viewshed(r = DEM_meter[[1]], shape = sf_shape,
                  turbine_locs = turblocdf[1:2,], 
                  h1 = runif(nrow(turblocdf[1:2,]), 0, 100),
                  h2 = 50, plot = TRUE))
  expect_is(res, "list")
  expect_true(length(res) == 5)
  expect_true(is.logical(as.vector(res[[1]])))
  expect_false(anyNA(res[[2]]))
  expect_true(class(res[[3]])[[1]] == "sf")
  expect_true(class(res[[4]]) == "RasterLayer")
  expect_false(anyNA(res[[5]]))
  
  res <- suppressWarnings(viewshed(r = DEM_meter[[1]], shape = sf_shape,
                  turbine_locs = turblocdf, 
                  h1 = runif(20, 0, 100),
                  h2 = 50, plot = FALSE))
  expect_is(res, "list")
  expect_true(length(res) == 5)
  expect_true(is.logical(as.vector(res[[1]])))
  expect_false(anyNA(res[[2]]))
  expect_true(class(res[[3]])[[1]] == "sf")
  expect_true(class(res[[4]]) == "RasterLayer")
  expect_false(anyNA(res[[5]]))
  
  res <- suppressWarnings(viewshed(r = DEM_meter[[1]], shape = sf_shape,
                  turbine_locs = turblocdf, 
                  h1 = runif(4, 0, 100),
                  h2 = 50, plot = FALSE))
  expect_is(res, "list")
  expect_true(length(res) == 5)
  expect_true(is.logical(as.vector(res[[1]])))
  expect_false(anyNA(res[[2]]))
  expect_true(class(res[[3]])[[1]] == "sf")
  expect_true(class(res[[4]]) == "RasterLayer")
  expect_false(anyNA(res[[5]]))
  
  res <- suppressWarnings(viewshed(r = DEM_meter[[1]], shape = sf_shape,
                  turbine_locs = turblocdf, 
                  h1 = NA,
                  h2 = NULL,
                  plot = FALSE))
  expect_is(res, "list")
  expect_true(length(res) == 5)
  expect_true(is.logical(as.vector(res[[1]])))
  expect_false(anyNA(res[[2]]))
  expect_true(class(res[[3]])[[1]] == "sf")
  expect_true(class(res[[4]]) == "RasterLayer")
  expect_false(anyNA(res[[5]]))
  
  
  ## plot_viewshed #################
  plt <- plot_viewshed(res)
  expect_true(is.null(plt))
  plt <- plot_viewshed(res, legend = T)
  expect_true(is.null(plt))
  
  turbloc <- suppressWarnings(spsample(DEM_meter[[2]], 1, type = "random"))
  res <- suppressWarnings(viewshed(r = DEM_meter[[1]], shape = DEM_meter[[2]],
                  turbine_locs = turbloc,  h1 = 1.8, h2 = 50))
  plt <- plot_viewshed(res, legend = T)
  expect_true(is.null(plt))
  
  ## rasterprofile ##################
  sample_POI <- suppressWarnings(spsample(DEM_meter[[2]], n = ncell(DEM_meter[[1]]), type = "regular"))
  sample_xy <- coordinates(sample_POI)

  n <- 10
  saplms <- sample(1:nrow(sample_xy), size = n, F)
  reso = min(res(DEM_meter[[1]]))
  reslist <- list()
  for (i in seq(1,n,2)) {
    reslist[[i]] <- rasterprofile(r = DEM_meter[[1]],
                                  xy1 = sample_xy[i, ], 
                                  xy2 = sample_xy[i + 1,], reso, F)
  }
  resdf <- do.call(rbind, reslist[seq(1,n,2)])
  expect_false(anyNA(resdf))
  expect_true(ncol(resdf) == 3)
  
  
  rppl <- rasterprofile(r = DEM_meter[[1]], xy1 = sample_xy[10, ], 
                xy2 = sample_xy[26,], reso, T)
  expect_false(anyNA(rppl))
  expect_true(ncol(rppl) == 3)
  
  
  
  ## viewTo ##################
  viwres <- viewTo(DEM_meter[[1]], xy1 = turblocdf[1,], sample_xy, 
         h1 = 1.5, h2 = 10, reso)
  # expect_true(is.character(names(viwres)))
  expect_true(is.logical(viwres))
  
  ## cansee ##################
  canrs <- cansee(DEM_meter[[1]], turblocdf[1,], sample_xy[3,], 
                  h1 = 0, h2 = 0, reso, plot=TRUE, interpolate = TRUE)
  expect_true(is.logical(canrs))
  expect_true(length(canrs) == 1)
  
  canrs <- cansee(DEM_meter[[1]], turblocdf[2,], sample_xy[3,], 
                  h1 = 200, h2 = 2, reso, plot=TRUE, interpolate = TRUE)
  expect_true(is.logical(canrs))
  expect_true(length(canrs) == 1)
  
  ## Create Warning (complete.cases)
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(4498482, 4498482, 4499991, 4499991, 4498482),
      c(2668272, 2669343, 2669343, 2668272, 2668272)))),
    crs = 3035
  ))
  r <- getDEM(polygon = Polygon1, ISO3 = "AUT", clip=T)
  
  xy1 <- st_sample(Polygon1, 1, "random")
  xy1 <- as.vector(st_coordinates(xy1))
  xy2 <- st_sample(Polygon1, 5, "random")
  xy2 <- st_coordinates(xy2)
  
  a <- t(apply(xy2, 1, function(d){
    cansee(r[[1]], xy1 = xy1, xy2 = d, h1=0, h2=0, reso)}))

  expect_true(is.logical(a))
  expect_false(anyNA(a))
  
  ## interpol_view ################
  turbloc <- suppressWarnings(spsample(DEM_meter[[2]], 10, type = "random"))
  res <- viewshed(r = DEM_meter[[1]], shape = DEM_meter[[2]],
                  turbine_locs = turbloc,  h1 = 1.8, h2 = 50)
  resinpl <- interpol_view(res, plotDEM = T, alpha = 0.5)
  expect_is(resinpl, "RasterLayer")
  
  resinpl <- interpol_view(res, plotDEM = T, alpha = 0.5, 
                           breakseq = seq(0,10,1), colNA = "black")
  expect_is(resinpl, "RasterLayer")
  resinpl <- interpol_view(res, alpha = 1, 
                           breakseq = seq(1,10,2))
  expect_is(resinpl, "RasterLayer")
  
  resinpl <- interpol_view(res, alpha = 1, 
                           breakseq = seq(1,4,2))
  expect_is(resinpl, "RasterLayer")
  
  resinpl <- interpol_view(res, alpha = 1.2, 
                           breakform = quantile)
  expect_is(resinpl, "RasterLayer")
  
  resinpl <- interpol_view(res, alpha = 1, 
                           breakform = factor)
  expect_is(resinpl, "RasterLayer")
  
})
