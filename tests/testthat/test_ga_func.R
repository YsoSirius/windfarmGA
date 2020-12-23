context("GA Functions")
library(sp)
library(spatstat)
# library(maptools)

## Function to suppress print/cat outputs
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

test_that("Test Genetic Algorithm Function", {
  ## Data ##############
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(0, 0, 2000, 2000, 0),
      c(0, 2000, 2000, 0, 0)))),
    crs = 3035
  ))
  
  Polygon2 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(0, 0, 1500, 2000, 0),
      c(0, 3500, 2000, 0, 0)))),
    crs = 3035
  ))
  
  ## BAROHOEHE ################################
  data <- matrix(seq(0,5000,500))
  res <- barometric_height(data)
  expect_false(anyNA(res))
  res1 <- barometric_height(data[,1])
  expect_false(anyNA(res1))
  expect_true(all.equal(res, res1))
  
  data <- data.frame(
    id = sample(1:10, length(seq(0,5000,500)), replace = TRUE),
    elev = seq(0,5000,500)
  )
  res2 <- barometric_height(data = data, "elev")
  expect_false(anyNA(res2))
  expect_true(all.equal(res2, res1))
  expect_error(barometric_height(data = data))
  rm(data, res, res1, res2)
  
  ## GRIDFILTER ################################
  Grid <- grid_area(shape = Polygon1, size = 200, prop = 1)
  expect_is(Grid[[1]], "matrix")
  expect_is(Grid[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid[[1]]))
  
  Grid <- grid_area(shape = Polygon1, size = 200, prop = 0.1)
  expect_is(Grid[[1]], "matrix")
  expect_is(Grid[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid[[1]]))
  
  Grid <- grid_area(shape = Polygon1,size = 500, prop = 0.1)
  expect_is(Grid[[1]], "matrix")
  expect_is(Grid[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid[[1]]))
  
  Grid <- grid_area(shape = Polygon1,size = 500, prop = 0)
  expect_is(Grid[[1]], "matrix")
  expect_is(Grid[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid[[1]]))
  
  Grid <- grid_area(shape = Polygon1,size = 300, prop = 0, plotGrid = TRUE)
  expect_is(Grid[[1]], "matrix")
  expect_is(Grid[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid[[1]]))
  
  ## too high resolution - error
  quiet(expect_error(grid_area(shape = Polygon1, size = 1e+06, prop = -1)))
  ## TODO - no check for too small size
  # expect_error(GridFilter(shape = Polygon1, size = 0.5, prop = -1))
  
  Grid <- grid_area(shape = Polygon2, size = 300, prop = 100)
  expect_is(Grid[[1]], "matrix")
  expect_is(Grid[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid[[1]]))
  # plot(Polygon2)
  # plot(Grid[[2]], add=T, col="red")
  # points(Grid[[1]][,2], Grid[[1]][,3], col="blue", pch=20)
  
  Grid1 <- grid_area(shape = Polygon2, size = 300, prop = 0.1)
  expect_is(Grid1[[1]], "matrix")
  expect_is(Grid1[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid1[[1]]))
  expect_true(nrow(Grid1[[1]]) > nrow(Grid[[1]]))
  # expect_false(identical(Grid[[2]]@bbox, Grid1[[2]]@bbox))
  # plot(Polygon2)
  # plot(Grid1[[2]], add=T, col="red")
  # points(Grid1[[1]][,2], Grid1[[1]][,3], col="blue", pch=20)
  
  Grid1 <- grid_area(shape = Polygon2, size = 300, prop = -100)
  expect_is(Grid1[[1]], "matrix")
  expect_is(Grid1[[2]], "sfc_POLYGON")
  expect_false(anyNA(Grid1[[1]]))
  expect_true(nrow(Grid1[[1]]) > nrow(Grid[[1]]))
  # expect_false(identical(Grid[[2]]@bbox, Grid1[[2]]@bbox))
  rm(Grid1, Polygon2)
  
  ## HEXATEX #################
  HexGrid <- hexa_area(Polygon1, 100, FALSE)
  expect_is(HexGrid[[1]], "matrix")
  expect_is(HexGrid[[2]], "sfc_POLYGON")
  expect_false(anyNA(HexGrid[[1]]))
  
  HexGrid <- hexa_area(Polygon1, 100, TRUE)
  expect_is(HexGrid[[1]], "matrix")
  expect_is(HexGrid[[2]], "sfc_POLYGON")
  expect_false(anyNA(HexGrid[[1]]))
  
  HexGrid <- hexa_area(Polygon1, 200, FALSE)
  expect_is(HexGrid[[1]], "matrix")
  expect_is(HexGrid[[2]], "sfc_POLYGON")
  expect_false(anyNA(HexGrid[[1]]))

  HexGrid <- hexa_area(Polygon1, 400.1, FALSE)
  expect_is(HexGrid[[1]], "matrix")
  expect_is(HexGrid[[2]], "sfc_POLYGON")
  expect_false(anyNA(HexGrid[[1]]))
  
  HexGrid <- hexa_area(Polygon1, 1000000000, FALSE)
  expect_is(HexGrid[[1]], "matrix")
  expect_is(HexGrid[[2]], "sfc_POLYGON")
  expect_false(anyNA(HexGrid[[1]]))
  rm(HexGrid)
  
  ## STARTGA ################################
  startsel <- init_population(Grid[[1]], n = 10, nStart = 20)
  expect_is(startsel, "list")
  # expect_true(all(sapply(startsel, class) == "matrix"))
  expect_true(all(sapply(startsel, nrow) == 10))
  expect_true(all(sapply(startsel, ncol) == 4))
  expect_output(str(startsel), "List of 20")
  expect_false(any(unlist(sapply(startsel, is.na))))

  # Produce Errors (quietly)
  quiet(expect_error(init_population(Grid[[1]][1:10,], n = 10, nStart = 20)))
  quiet(expect_error(init_population(Grid[[1]][1:10,], n = 7, nStart = 20)))
  
  startsel <- init_population(Grid[[1]], n = 20, nStart = 25)
  expect_is(startsel, "list")
  # expect_true(all(sapply(startsel, class) == "matrix"))
  expect_true(all(sapply(startsel, nrow) == 20))
  expect_true(all(sapply(startsel, ncol) == 4))
  expect_output(str(startsel), "List of 25")
  expect_false(any(unlist(sapply(startsel, is.na))))
  
  startsel <- init_population(Grid[[1]], n = 20, nStart = 100)
  expect_is(startsel, "list")
  # expect_true(all(sapply(startsel, class) == "matrix"))
  expect_true(all(sapply(startsel, nrow) == 20))
  expect_true(all(sapply(startsel, ncol) == 4))
  expect_output(str(startsel), "List of 100")
  expect_false(any(unlist(sapply(startsel, is.na))))
  
  startsel <- init_population(Grid[[1]], n = 20, nStart = 300)
  expect_is(startsel, "list")
  # expect_true(all(sapply(startsel, class) == "matrix"))
  expect_true(all(sapply(startsel, nrow) == 20))
  expect_true(all(sapply(startsel, ncol) == 4))
  expect_output(str(startsel), "List of 300")
  expect_false(any(unlist(sapply(startsel, is.na))))
  
  startsel <- init_population(Grid[[1]], n = 10, nStart = 20)
  expect_is(startsel, "list")
  # expect_true(all(sapply(startsel, class) == "matrix"))
  expect_true(all(sapply(startsel, nrow) == 10))
  expect_true(all(sapply(startsel, ncol) == 4))
  expect_output(str(startsel), "List of 20")
  expect_false(any(unlist(sapply(startsel, is.na))))
  
  ## FITNESS ################################
  wind <- data.frame(ws = 12, wd = 0)
  ## TODO - fitness now takes a list of windata. winddata and probability?
  wind <- list(wind, probab = 100)
  fit <- fitness(selection = startsel,referenceHeight = 100, RotorHeight=100,
                 SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,rot=20,
                 dirspeed = wind, srtm_crop="",topograp=FALSE,cclRaster="")
  expect_output(str(fit), "List of 20")
  expect_true(all(sapply(fit, nrow) == 10))
  expect_false(any(unlist(sapply(fit, is.na))))
  expect_false(any(unlist(do.call("rbind", fit)[,-c(1,2)] < 0)))
  
  fit1 <- fitness(selection = startsel, referenceHeight = 100, RotorHeight=100,
                  SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,rot=20,
                  dirspeed = wind, topograp=FALSE)
  expect_output(str(fit1), "List of 20")
  expect_true(all(sapply(fit1, nrow) == 10))
  expect_false(any(unlist(sapply(fit1, is.na))))
  expect_false(any(unlist(do.call("rbind", fit1)[,-c(1,2)] < 0)))
  rm(fit1)
  
  ## SELECTION ################################
  allparks <- do.call("rbind", fit)
  selec6best <- selection(fit, Grid[[1]], 2, TRUE, 6, "VAR")
  expect_output(str(selec6best), "List of 2")
  expect_false(any(unlist(sapply(selec6best, is.na))))
  expect_true(all(unlist(selec6best[[1]][,-1]) %in% c(0,1)))
  expect_true(all(selec6best[[2]][,-1] > 0))
  rm(selec6best)

  ## Produce error
  fitNA <- fit
  fitNA[[1]][, "Parkfitness"] <- NA
  a <- lapply(1:length(fitNA), function(i) {
    fitNA[[i]][, "Parkfitness"] <<- NA
  })
  rm(a)
  expect_error(selection(fitNA, Grid[[1]], 2, TRUE, 6, "VAR"))


  selec6best <- selection(fit, Grid[[1]], teil = 1, TRUE, 6, "FIX")
  expect_output(str(selec6best), "List of 2")
  expect_false(any(unlist(sapply(selec6best, is.na))))
  expect_true(all(unlist(selec6best[[1]][,-1]) %in% c(0,1)))
  expect_true(all(selec6best[[2]][,-1] > 0))
  rm(selec6best)

  selec6best <- selection(fit, Grid[[1]], 2, TRUE, 6, "FIX")
  expect_output(str(selec6best), "List of 2")
  expect_false(any(unlist(sapply(selec6best, is.na))))
  expect_true(all(unlist(selec6best[[1]][,-1]) %in% c(0,1)))
  expect_true(all(selec6best[[2]][,-1] > 0))
  rm(selec6best)

  selec6best <- selection(fit, Grid[[1]], 2, TRUE, 6, "FIX")
  expect_output(str(selec6best), "List of 2")
  expect_false(any(unlist(sapply(selec6best, is.na))))
  expect_true(all(unlist(selec6best[[1]][,-1]) %in% c(0,1)))
  expect_true(all(selec6best[[2]][,-1] > 0))
  rm(selec6best)

  selec6best <- quiet(selection(fit, Grid[[1]],4, FALSE, 6, selstate = "VAR",
                           verbose = TRUE))
  expect_output(str(selec6best), "List of 2")
  expect_false(any(unlist(sapply(selec6best, is.na))))
  expect_true(all(unlist(selec6best[[1]][,-1]) %in% c(0,1)))
  expect_true(all(selec6best[[2]][,-1] > 0))
  rm(selec6best)

  selec6best <- quiet(selection(fit, Grid[[1]],4, FALSE, 6, "FIX",
                           verbose = TRUE))
  expect_output(str(selec6best), "List of 2")
  expect_false(any(unlist(sapply(selec6best, is.na))))
  expect_true(all(unlist(selec6best[[1]][,-1]) %in% c(0,1)))
  expect_true(all(selec6best[[2]][,-1] > 0))

  selec6best <- quiet(selection(fit, Grid[[1]],4, TRUE, 6, "FIX",
                                 verbose = TRUE))
  expect_output(str(selec6best), "List of 2")
  expect_false(any(unlist(sapply(selec6best, is.na))))
  expect_true(all(unlist(selec6best[[1]][,-1]) %in% c(0,1)))
  expect_true(all(selec6best[[2]][,-1] > 0))
  
  ## CROSSOVER #####################
  crossOut <- quiet(crossover(selec6best, 2, uplimit = 300, crossPart = "RAN",
                         verbose = TRUE))
  expect_output(str(crossOut), "num")
  expect_false(any(is.na(crossOut)))
  expect_true(all(crossOut %in% c(0, 1)))
  rm(crossOut)

  crossOut <- crossover(selec6best, 7, uplimit = 500, crossPart = "RAN")
  expect_output(str(crossOut), "num")
  expect_false(any(is.na(crossOut)))
  expect_true(all(crossOut %in% c(0, 1)))
  rm(crossOut)

  crossOut <- quiet(crossover(se6 = selec6best, u = 6, uplimit = 100,
                         crossPart = "EQU", seed = 105, verbose = TRUE))
  expect_output(str(crossOut), "num")
  expect_false(any(is.na(crossOut)))
  expect_true(all(crossOut %in% c(0, 1)))

  crossOut1 <- crossover(se6 = selec6best, u = 3, uplimit = 300,
                          crossPart = "EQU", seed = 105)
  crossOut2 <- crossover(se6 = selec6best, u = 3, uplimit = 300,
                          crossPart = "EQU", seed = 105)
  ## TODO Why is mean difference sometimes 2?? seed not working correctly?
  expect_true(all.equal(crossOut1, crossOut2, tolerance = 2))

  expect_output(str(crossOut1), "num")
  expect_false(any(is.na(crossOut1)))
  expect_true(all(crossOut1 %in% c(0, 1)))
  rm(crossOut, crossOut1)

  crossOut <- crossover(se6 = selec6best, u = 7, uplimit = 500,
                         crossPart = "RAN", seed = 105)
  expect_output(str(crossOut), "num")
  expect_false(any(is.na(crossOut)))
  expect_true(all(crossOut %in% c(0, 1)))
  rm(crossOut)

  ## Produce error
  expect_error(crossover(se6 = selec6best, u = 7, uplimit = 500,
                         crossPart = "something"))

  crossOut <- crossover(selec6best, 3, uplimit = 300, crossPart = "EQU")
  expect_output(str(crossOut), "num")
  expect_false(any(is.na(crossOut)))
  expect_true(all(crossOut %in% c(0, 1)))


  ## MUTATION #####################
  ## Variable Mutation Rate is activated if more than 2 individuals represent the
  ## current best solution.
  mut <- mutation(a = crossOut, p = 0.3)
  expect_output(str(mut), "num")
  expect_false(any(is.na(mut)))
  expect_true(all(mut %in% c(0, 1)))

  mut <- mutation(a = crossOut, p = 0.1)
  expect_output(str(mut), "num")
  expect_false(any(is.na(mut)))
  expect_true(all(mut %in% c(0, 1)))

  mut <- mutation(a = crossOut, p = 1)
  expect_output(str(mut), "num")
  expect_false(any(is.na(mut)))
  expect_true(all(mut %in% c(0, 1)))

  mut <- mutation(a = crossOut, p = 100)
  expect_output(str(mut), "num")
  expect_false(any(is.na(mut)))
  expect_true(all(mut %in% c(0, 1)))

  mut <- mutation(a = crossOut, p = -1)
  expect_output(str(mut), "num")
  expect_false(any(is.na(mut)))
  expect_true(all(mut %in% c(0, 1)))

  mut <- mutation(a = crossOut, p = -1, seed = 104)
  mut1 <- mutation(a = crossOut, p = -1, seed = 104)
  expect_true(identical(mut, mut1))

  mut <- mutation(a = crossOut, p = 0.0005)
  expect_output(str(mut), "num")
  expect_false(any(is.na(mut)))
  expect_true(all(mut %in% c(0, 1)))

  ## TRIMTON #####################
  ## After Crossover and Mutation, the amount of turbines in a windpark change
  ## and have to be corrected to the required amount of turbines.
  mut1 <- trimton(mut = mut, nturb = 1, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = FALSE)
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == 1))
  expect_true(all(dim(mut) == dim(mut1)))
  rm(mut1)

  mut1 <- trimton(mut = mut, nturb = min(colSums(mut)), allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = FALSE)
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == min(colSums(mut))))
  expect_true(all(dim(mut) == dim(mut1)))
  rm(mut1)

  mut1 <- trimton(mut = mut, nturb = 10, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = FALSE)
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == 10))  
  expect_true(all(dim(mut) == dim(mut1)))

  mut1 <- trimton(mut = mut, nturb = 5, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = TRUE)
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == 5))
  expect_true(all(dim(mut) == dim(mut1)))
  rm(mut1)

  mut1 <- trimton(mut = mut, nturb = 1, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = TRUE)
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == 1))
  expect_true(all(dim(mut) == dim(mut1)))
  rm(mut1)

  mut1 <- trimton(mut = mut, nturb = min(colSums(mut)), allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = TRUE)
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == min(colSums(mut))))
  expect_true(all(dim(mut) == dim(mut1)))
  rm(mut1)

  mut1 <- trimton(mut = mut, nturb = 20, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = TRUE)
  expect_output(str(mut1), "num")
  expect_false(any(is.na(mut1)))
  expect_true(all(mut1 %in% c(0, 1)))
  expect_true(all(colSums(mut1) == 20))  
  expect_true(all(dim(mut) == dim(mut1)))
  
  mut1 <- trimton(mut = mut, nturb = 20, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = TRUE, seed = 104)
  mut2 <- trimton(mut = mut, nturb = 20, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = TRUE, seed = 104)
  expect_true(identical(mut1, mut2))

  mut1 <- trimton(mut = mut, nturb = 20, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = FALSE, seed = 234)
  mut2 <- trimton(mut = mut, nturb = 20, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = FALSE, seed = 234)
  expect_true(identical(mut1, mut2))

  mut1 <- trimton(mut = mut, nturb = 5, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = TRUE, seed = 300)
  mut2 <- trimton(mut = mut, nturb = 5, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = TRUE, seed = 300)
  expect_true(identical(mut1, mut2))

  mut1 <- trimton(mut = mut, nturb = 5, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = FALSE, seed = 234)
  mut2 <- trimton(mut = mut, nturb = 5, allparks = allparks,
                  nGrids = nrow(Grid[[1]]), trimForce = FALSE, seed = 234)
  expect_true(identical(mut1, mut2))
  
  ## GETRECTV #####################
  getRectV <- get_grids(mut1, Grid[[1]])
  expect_is(getRectV, "list")
  # expect_true(all(sapply(getRectV, class) == "matrix"))
  expect_true(all(sapply(getRectV, ncol) == 3))
  expect_false(any(unlist(sapply(getRectV, is.na))))
  expect_true(all(sapply(getRectV, colnames) %in% c("ID","X","Y")))


  ## FITNESS AGAIN #####################
  fit <- fitness(selection = getRectV,referenceHeight = 100, RotorHeight = 100,
                 SurfaceRoughness = 0.3, Polygon = Polygon1, resol1 = 200, rot = 20,
                 dirspeed = wind, srtm_crop = "", topograp = FALSE, cclRaster = "")
  expect_is(fit, "list")
  expect_true(length(fit) == length(getRectV))
  expect_false(any(unlist(sapply(fit, is.na))))
  expect_false(any(unlist(do.call("rbind", fit)[,-c(1,2)] < 0)))

})
