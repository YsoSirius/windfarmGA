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

.onUnload <- function(libpath) {
  library.dynam.unload("windfarmGA", libpath)
}
# .onAttach <- function(){
#   options(windfarmGA.connection = stdin())
# }
