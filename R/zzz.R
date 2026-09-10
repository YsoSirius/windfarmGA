# nocov start
utils::globalVariables(
  c(
    "X", "Y",
    "a_weibull", "k_weibull",
    "var1.pred", "x", "y",
    "element_rect", "element_line",
    "unit",
    "srtm_crop", "cclRaster", "weibullsrc",
    "cl",
    "k"
  )
)

## Used only or random_search_single for testing user-input
.onLoad <- function(libname, pkgname) {
  options(
    windfarmGA.connection = stdin(),
    windfarmGA.cT = 0.88,
    windfarmGA.air_rh = 1.225,
    windfarmGA.k = 0.075,
    windfarmGA.Cp = 0.45,
    windfarmGA.cut_in = 0,
    windfarmGA.rated_ws = Inf,
    windfarmGA.cut_out = Inf,
    windfarmGA.wind_profile = "log",
    windfarmGA.fitness_efficiency_weight = 1,
    windfarmGA.max_angle = 20,
    windfarmGA.max_distance = 100000,
    windfarmGA.max_population = 300,
    windfarmGA.max_selection = 100
  )
}

## Is this still necessary?
.onUnload <- function(libpath) {
  library.dynam.unload("windfarmGA", libpath)
}

# nocov end
