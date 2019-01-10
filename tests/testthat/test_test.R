context("Windfarm Functions")
library(testthat)
library(windfarmGA)
library(sp)

test_that("All Functions", {
  Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
  Polygon1 <- Polygons(list(Polygon1),1);
  Polygon1 <- SpatialPolygons(list(Polygon1))
  Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  proj4string(Polygon1) <- CRS(Projection)


  ## BAROHOEHE ################################
  data <- matrix(seq(0,5000,500));
  expect_s3_class(BaroHoehe(data), "data.frame")


  ## GRIDFILTER ################################
  Grid <- GridFilter(shape = Polygon1,resol = 200,prop = 1)
  expect_is(Grid[[1]], "data.frame")
  expect_is(Grid[[2]], "SpatialPolygonsDataFrame")

  ## STARTGA ################################
  startsel <- StartGA(Grid[[1]], n = 10, nStart = 20);
  expect_is(startsel, "list")
  expect_true(all(sapply(startsel, class) == "data.frame"))
  expect_true(all(sapply(startsel, nrow) == 10))
  expect_true(all(sapply(startsel, length) == 4))
  expect_output(str(startsel), "List of 20")


  ## FITNESS ################################
  wind <- as.data.frame(cbind(ws=12,wd=0))
  fit <- fitness(selection = startsel,referenceHeight = 100, RotorHeight=100,
                 SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,rot=20,
                 dirspeed = wind, srtm_crop="",topograp=FALSE,cclRaster="")
  expect_is(fit, "list")
  expect_true(all(sapply(fit, class) == "data.frame"))

  fit1 <- fitness(selection = startsel, referenceHeight = 100, RotorHeight=100,
                 SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,rot=20,
                 dirspeed = wind, topograp=FALSE)
  expect_is(fit1, "list")
  expect_true(all(sapply(fit1, class) == "data.frame"))


  ## SELECTION ################################
  allparks <- do.call("rbind",fit);
  selec6best <- selection1(fit, Grid[[1]], 2, TRUE, 6, "VAR");
  expect_is(selec6best, "list")
  expect_true(all(sapply(selec6best, class) == "data.frame"))
  expect_output(str(selec6best), "List of 2")
  rm(selec6best)

  selec6best <- selection1(fit, Grid[[1]],2, TRUE, 6, "FIX");
  expect_is(selec6best, "list")
  expect_true(all(sapply(selec6best, class) == "data.frame"))
  expect_output(str(selec6best), "List of 2")
  rm(selec6best)

  selec6best <- selection1(fit, Grid[[1]],4, FALSE, 6, "FIX");
  expect_is(selec6best, "list")
  expect_true(all(sapply(selec6best, class) == "data.frame"))
  rm(selec6best)



})








