context("Basic Functions")

library(testthat)
library(sp)
library(sf)
library(ggplot2)


test_that("Test Basic Functions", {
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

  projection <- "+init=epsg:3035"
  res0 <- isSpatial(xy_matrix, projection)
  expect_true(class(res0)[1] == "SpatialPolygons")
  # expect_true(as.character(proj4string(res0)) == projection)
  

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
  # expect_true(as.character(proj4string(res1)) == projection)
  expect_true(identical(res0@bbox, res1@bbox)) 

  ## Data.frame with Random column order (long, lat)
  res1 <- isSpatial(xy_dataframe[, c(3,2,5,4,1)])
  expect_true(class(res1)[1] == "SpatialPolygons")
  expect_true(identical(res0@bbox, res1@bbox)) 
  expect_true(is.na(proj4string(res1)))

  res1 <- isSpatial(xy_dataframe[, c(3,2,5,4,1)], projection)
  expect_true(class(res1)[1] == "SpatialPolygons")
  expect_true(identical(res0@bbox, res1@bbox)) 
  # expect_true(proj4string(res1) == proj4string(res0))

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
  
  colnames(coords_df) <- c("asd","fasf")
  res12 <- isSpatial(coords_df)
  expect_true(class(res12)[1] == "SpatialPolygons")
  expect_true(identical(res0@bbox, res12@bbox))

  ## Simple Feature
  simple_feature <- sf::st_as_sf(spatial_polygon)
  res2 <- isSpatial(simple_feature, projection)
  expect_true(class(res2)[1] == "SpatialPolygons" |
                class(res2)[1] == "SpatialPolygonsDataFrame")
  expect_true(identical(res0@bbox, res2@bbox))
  all.equal(wkt(res2), wkt(res0))
  
  ## windata_format #############
  wind_df <- data.frame(ws = c(12, 30, 45), 
                        wd = c(0, 90, 150),
                        probab = 30:32)
  a <- windata_format(wind_df)
  expect_false(all(sapply(a, anyNA)))
  expect_true(all(a[[1]][, "ws"] == wind_df$ws))
  expect_true(all(a[[1]][, "wd"] == unique(wind_df$wd)))
  expect_false(any(duplicated(a[[1]][, "wd"])))
  expect_true(sum(a[[2]]) == 100)

  
  wind_df <- data.frame(speed = c(12, 30, 45), 
                        direction = c(90, 90, 150)
                        ,probab = c(10, 20, 60)
  )
  a <- windata_format(wind_df)
  expect_false(all(sapply(a, anyNA)))
  expect_true(all(a[[1]][, "ws"] == wind_df$ws))
  expect_true(all(a[[1]][, "wd"] == unique(wind_df$wd)))
  expect_false(any(duplicated(a[[1]][, "wd"])))
  expect_true(sum(a[[2]]) == 100)


  wind_df <- data.frame(speed = c(12, 30, 45, 40), 
                        direction = c(92, 90, 94, 95)
                        ,probab = c(10, 20, 140, 10)
  )
  a <- windata_format(wind_df)
  expect_false(all(sapply(a, anyNA)))
  expect_false(any(a[[1]][, "wd"] > 360))
  expect_false(all(a[[1]][, "ws"] == wind_df$speed))
  expect_false(all(a[[1]][, "wd"] == unique(wind_df$direction)))
  expect_false(any(duplicated(a[[1]][, "wd"])))
  expect_true(sum(a[[2]]) == 100)
  
  wind_df <- data.frame(speed = c(12, 30, 45), 
                        direction = c(400, 90, 150)
  )
  a <- windata_format(wind_df)
  expect_false(all(sapply(a, anyNA)))
  expect_false(any(a[[1]][, "wd"] > 360))
  expect_false(all(a[[1]][, "ws"] == wind_df$speed))
  expect_false(all(a[[1]][, "wd"] == unique(wind_df$direction)))
  expect_false(any(duplicated(a[[1]][, "wd"])))
  expect_true(sum(a[[2]]) == 100)
  
  wind_df <- data.frame(speed = c(12, 30, 45), 
                        direction = c(90, 90, 150)
  )
  a <- windata_format(wind_df)
  expect_false(all(sapply(a, anyNA)))
  expect_false(any(a[[1]][, "wd"] > 360))
  expect_false(all(a[[1]][, "ws"] %in% wind_df$speed))
  expect_true(all(a[[1]][, "wd"] %in% unique(wind_df$direction)))
  expect_false(any(duplicated(a[[1]][, "wd"])))
  expect_true(sum(a[[2]]) == 100)
  
  wind_df <- data.frame(c(12, 30, 45), 
                        c(0, 90, 150))
  a <- windata_format(wind_df)
  expect_false(all(sapply(a, anyNA)))
  expect_false(any(a[[1]][, "wd"] > 360))
  expect_true(all(a[[1]][, "ws"] == wind_df[, 1]))
  expect_true(all(a[[1]][, "wd"] %in% unique(wind_df[, 2])))
  expect_false(any(duplicated(a[[1]][, "wd"])))
  expect_true(sum(a[[2]]) == 100)
  
  wind_df <- data.frame(c(12, 30, 45), 
                        c(0, 90, 150),
                        c(10, 20, 70))
  wind_df <- as.matrix(wind_df)
  a <- windata_format(wind_df)
  expect_false(all(sapply(a, anyNA)))
  expect_false(any(a[[1]][, "wd"] > 360))
  expect_true(all(a[[1]][, "ws"] == wind_df[, 1]))
  expect_true(all(a[[1]][, "wd"] %in% unique(wind_df[, 2])))
  expect_true(all(a[[1]][, "probab"] %in% unique(wind_df[, 3])))
  expect_false(any(duplicated(a[[1]][, "wd"])))
  expect_true(sum(a[[2]]) == 100)
  
  wind_df <- data.frame(c(12, 30, 45), 
                        c(0, 90, 150),
                        c(10, 20, 140))
  wind_df <- as.matrix(wind_df)
  a <- windata_format(wind_df)
  expect_false(all(sapply(a, anyNA)))
  expect_false(any(a[[1]][, "wd"] > 360))
  expect_true(all(a[[1]][, "ws"] == wind_df[,1]))
  expect_true(all(a[[1]][, "wd"] %in% unique(wind_df[, 2])))
  expect_false(all(a[[1]][, "probab"] %in% unique(wind_df[, 3])))
  expect_false(any(duplicated(a[[1]][, "wd"])))
  expect_true(sum(a[[2]]) == 100)
  
  colnames(wind_df) <- NULL
  a <- windata_format(wind_df)
  expect_false(all(sapply(a, anyNA)))
  expect_false(any(a[[1]][, "wd"] > 360))
  expect_true(all(a[[1]][, "ws"] == wind_df[, 1]))
  expect_true(all(a[[1]][, "wd"] %in% unique(wind_df[, 2])))
  expect_false(any(duplicated(a[[1]][, "wd"])))
  expect_true(sum(a[[2]]) == 100)
  
  wind_df <- wind_df[,1:2]
  a <- windata_format(wind_df)
  expect_false(all(sapply(a, anyNA)))
  expect_false(any(a[[1]][, "wd"] > 360))
  expect_true(all(a[[1]][, "ws"] == wind_df[, 1]))
  expect_true(all(a[[1]][, "wd"] %in% unique(wind_df[, 2])))
  expect_false(any(duplicated(a[[1]][, "wd"])))
  expect_true(sum(a[[2]]) == 100)
  
})
