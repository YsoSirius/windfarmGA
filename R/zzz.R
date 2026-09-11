# nocov start
utils::globalVariables(
  c(
    "X", "Y",
    "a_weibull", "k_weibull",
    "var1.pred", "x", "y",
    "element_rect", "element_line",
    "unit",
    "srtm_crop", "cclRaster", "weibull_src",
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
    windfarmGA.power_curve = NULL,
    windfarmGA.wind_profile = "log",
    windfarmGA.fitness_efficiency_weight = 1,
    windfarmGA.max_angle = 20,
    windfarmGA.max_distance = 100000,
    windfarmGA.max_population = 300,
    windfarmGA.max_selection = 300,
    windfarmGA.crossover_inject = 0.25,
    windfarmGA.min_swaps = 1L,
    windfarmGA.immigrants = 3L,
    windfarmGA.stall_generations = 40L,
    windfarmGA.refine_after = 12L,
    windfarmGA.refine_min_gen = 18L,
    windfarmGA.refine_hold = 25L,
    windfarmGA.explore_pulse = 10L,
    windfarmGA.spatial_crossover = 0.5,
    windfarmGA.local_search_elites = 5L,
    windfarmGA.local_search_tries = 6L,
    windfarmGA.elite_children = 3L,
    windfarmGA.elite_mix = 2L
  )
}

## Is this still necessary?
.onUnload <- function(libpath) {
  library.dynam.unload("windfarmGA", libpath)
}

# nocov end
