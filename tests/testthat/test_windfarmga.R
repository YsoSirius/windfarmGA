context("Test windfarmGA")

## Function to suppress print/cat outputs
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

test_that("Test windfarmGA", {
  # skip("")
  skip_on_cran()
  
  ## Data ##############
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(0, 0, 2000, 2000, 0),
      c(0, 2000, 2000, 0, 0)))),
    crs = 3035
  ))
  data.in <- data.frame(ws = 12, wd = 0)
  
  ## SF-Polygon Input #####################
  options(windfarmGA.connection = stdin())
  f <- file()
  options(windfarmGA.connection = f)
  ans <- paste(rep(c(" "),100), collapse = "\n")
  write(ans, f)
  resultSP <- quiet(windfarmGA(Polygon1 = Polygon1,
                               n = 20, iteration = 5,
                               vdirspe = data.in, 
                               selstate = "FIX", crossPart1 = "EQU",
                               Rotor = 35, Proportionality = 1,
                               RotorHeight = 100, plotit = TRUE))
  expect_true(nrow(resultSP) == 5)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))))
  
  ## Use DNS and layer (Shapfile from Source) #######################
  dns <- system.file("extdata/shape.shp", package = "windfarmGA")
  resultSP <- quiet(windfarmGA(dns = dns, layer = "shape",
                               n = 20, iteration = 3,
                               vdirspe = data.in, 
                               selstate = "FIX", crossPart1 = "EQU",
                               Rotor = 35, Proportionality = 1,
                               RotorHeight = 100))
  expect_true(nrow(resultSP) == 3)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))))
  
  ## SF-Polygon - Other Projection #####################
  Polygon1 <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(0, 0, 2000, 2000, 0),
      c(0, 2000, 2000, 0, 0)))),
    crs = 3857
  ))
  resultSP <- quiet(windfarmGA(Polygon1 = Polygon1, Projection = 3857,
                               n = 20, iteration = 5,
                               vdirspe = data.in, GridMethod = "h",
                               selstate = "FIX", crossPart1 = "EQU",
                               Rotor = 30, Proportionality = 1,
                               RotorHeight = 100, plotit = TRUE))
  expect_true(nrow(resultSP) == 5)
  expect_is(resultSP, "matrix")
  expect_false(any(unlist(sapply(resultSP, is.na))))
  
  
  ## Errors ############
  ## SF-Polygon - No Projection
  st_crs(Polygon1) <- NA
  expect_error(expect_warning(quiet(
    windfarmGA(Polygon1 = Polygon1,
               n = 20, iteration = 5, Projection = 4326,
               vdirspe = data.in, GridMethod = "h",
               selstate = "FIX", crossPart1 = "EQU",
               Rotor = 50, Proportionality = 1,
               RotorHeight = 100, plotit = TRUE)
  )))
  
  ## no winddata
  st_crs(Polygon1) <- 3035
  expect_error(quiet(
    windfarmGA(Polygon1 = Polygon1,
               n = 20, iteration = 5,
               # vdirspe = data.in, 
               selstate = "FIX", crossPart1 = "EQU",
               Rotor = 35, Proportionality = 1,
               RotorHeight = 100, plotit = FALSE)
  ))
  
  ## n/iteration should be numeric
  expect_error(quiet(
    windfarmGA(Polygon1 = Polygon1,
               n = "20", iteration = "5",
               vdirspe = data.in,
               selstate = "FIX", crossPart1 = "EQU",
               Rotor = 35, Proportionality = 1,
               RotorHeight = 100, plotit = FALSE)
  ))
  
  ## elitism should be boolean
  expect_error(quiet(
    windfarmGA(Polygon1 = Polygon1,
               n = 20, iteration = 5,
               vdirspe = data.in,
               selstate = "FIX", crossPart1 = "EQU",
               Rotor = 35, Proportionality = 1,
               RotorHeight = 100, 
               elitism = "asd")
  ))
  
  ## weibullsrc in wrong format
  expect_error(quiet(
    windfarmGA(Polygon1 = Polygon1,
               n = 12,
               vdirspe = data.in,
               selstate = "FIX", crossPart1 = "EQU",
               Rotor = 60, weibull = TRUE,
               weibullsrc = data.frame(x=1,y=2),
               RotorHeight = 100)
  ))
  expect_error(quiet(
    windfarmGA(Polygon1 = Polygon1,
               n = 12,
               vdirspe = data.in,
               selstate = "FIX", crossPart1 = "EQU",
               Rotor = 60, weibull = TRUE,
               weibullsrc = list(x=1,y=2),
               RotorHeight = 100)
  ))
  
  # reset connection
  options(windfarmGA.connection = stdin())
  close(f)
  
})
