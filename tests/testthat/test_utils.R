context("Basic Functions")

library(testthat)
library(windfarmGA)
library(sp)
library(sf)
library(ggplot2)

test_that("Test Basic Functions", {
  
  ## euc.dist ################
  x=c(200,100); y=c(1000,2000)
  ed = euc.dist(x, y)
  expect_is(ed, "numeric")
  
  ## Control with stats::dist function
  ctrl = stats::dist(rbind(x, y))
  expect_true(round(ed) == round(ctrl))
  rm(x,y,ed,ctrl)
  
  x = cbind(runif(100, 1, 1000),runif(100, 1, 1000))
  y = cbind(runif(100, 100, 10000), runif(100, 100, 100000))
  z = cbind(x,y)
  ed = apply(z, MARGIN = 1, FUN = function(i) euc.dist(i[1:2], i[3:4]))
  expect_false(all(is.na(ed)))
  
  ## TODO
  # compare with cpp results
  
  ## PointToLine2 ################
  x <- c(100,100); y <- c(500,500);
  res <- PointToLine2(x,y,FALSE)
  expect_true(all(res[3:4] == x))
  expect_true(all(res[1:2] == y))
  expect_false(any(is.na(res)))
  rm(x, res)
  
  ## TODO
  # compare with cpp results
  
  
  ## splitAt #####################
  a <- splitAt(1:100,20)
  expect_true(length(a[[1]]) == 19)
  expect_false(any(is.na(unlist(a))))
  expect_true(all(unlist(a) %in% 1:100))
  rm(a)
  
  a <- splitAt(as.matrix(1:100),20)
  expect_true(length(a[[1]]) == 19)
  expect_false(any(is.na(unlist(a))))
  expect_true(all(unlist(a) %in% 1:100))
  rm(a)
  
  ## isSpatial ################
  ## test matrix, dataframes, spatialPolygons ad simpleFeatures with and
  ## without projection. SimpleFeatures to sp-object create PolygonsDataFrame
  xy_matrix <- rbind(c(4498482, 2668272), c(4498482, 2669343),
                     c(4499991, 2669343), c(4499991, 2668272))
  res0 <- isSpatial(xy_matrix)
  expect_true(class(res0)[1] == "SpatialPolygons")
  expect_true(is.na(proj4string(res0)))

  projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  res0 <- isSpatial(xy_matrix, projection)
  expect_true(class(res0)[1] == "SpatialPolygons")
  expect_true(as.character(proj4string(res0)) == projection)

  spatial_polygon <- Polygon(rbind(c(4498482, 2668272), c(4498482, 2669343),
                                   c(4499991, 2669343), c(4499991, 2668272)))
  spatial_polygon <- Polygons(list(spatial_polygon), 1)
  spatial_polygon <- SpatialPolygons(list(spatial_polygon))
  proj4string(spatial_polygon) <- CRS(projection)
  xy_dataframe <- ggplot2::fortify(spatial_polygon)
  res1 <- isSpatial(xy_dataframe)
  expect_true(class(res1)[1] == "SpatialPolygons")
  expect_true(is.na(proj4string(res1)))

  res1 <- isSpatial(xy_dataframe, projection)
  expect_true(class(res1)[1] == "SpatialPolygons")
  expect_true(as.character(proj4string(res1)) == projection)
  expect_true(identical(res0@bbox, res1@bbox)) 

  ## Data.frame with Random column order (long, lat)
  res1 <- isSpatial(xy_dataframe[, c(3,2,5,4,1)])
  expect_true(class(res1)[1] == "SpatialPolygons")
  expect_true(identical(res0@bbox, res1@bbox)) 
  expect_true(is.na(proj4string(res1)))

  res1 <- isSpatial(xy_dataframe[, c(3,2,5,4,1)], projection)
  expect_true(class(res1)[1] == "SpatialPolygons")
  expect_true(identical(res0@bbox, res1@bbox)) 
  expect_true(proj4string(res1) == proj4string(res0))

  ## with y/x
  coords_df <- cbind(xy_dataframe[, 2], xy_dataframe[, 1])
  colnames(coords_df) <- c("Y_Coords", "XX_Coords")
  res12 <- isSpatial(coords_df)
  expect_true(class(res12)[1] == "SpatialPolygons")
  expect_true(identical(res0@bbox, res12@bbox))

  coords_df <- cbind(xy_dataframe[, 1], xy_dataframe[, 2])
  colnames(coords_df) <- c("X____", "y_")
  res12 <- isSpatial(coords_df)
  expect_true(class(res12)[1] == "SpatialPolygons")
  expect_true(identical(res0@bbox, res12@bbox))

  ## Simple Feature
  simple_feature <- sf::st_as_sf(spatial_polygon)
  res2 <- isSpatial(simple_feature, projection)
  expect_true(class(res2)[1] == "SpatialPolygons" |
                class(res2)[1] == "SpatialPolygonsDataFrame")
  expect_true(identical(res0@bbox, res2@bbox))
  expect_true(proj4string(res2) == proj4string(res0))
})