# nocov start
utils::globalVariables(
  c(
    "X","Y",
    "a_weibull", "k_weibull",
    "var1.pred","x","y",
    "element_rect","element_line",
    "unit", 
    "srtm_crop", "cclRaster", "weibullsrc",
    "cl",
    "k" 
  ))

## Used only or random_search_single for testing user-input
.onLoad <- function(libname, pkgname){
  options(windfarmGA.connection = stdin())
}

## Is this still necessary?
.onUnload <- function(libpath) {
  library.dynam.unload("windfarmGA", libpath)
}

# nocov end
