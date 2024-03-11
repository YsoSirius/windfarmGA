##' @name package_installed
##' @rdname package_installed
##'
##' @title Is the package installed or not
##'
##' @return An invisible boolean value, indicating if the package is
##'   installed or not.
NULL

##' @rdname package_installed
##' @export
is_foreach_installed <- function() {
  requireNamespace("foreach", quietly = TRUE)
}

##' @rdname package_installed
##' @export
is_parallel_installed <- function() {
  requireNamespace("parallel", quietly = TRUE)
}

##' @rdname package_installed
##' @export
is_doparallel_installed <- function() {
  requireNamespace("doParallel", quietly = TRUE)
}

##' @rdname package_installed
##' @export
is_ggplot2_installed <- function() {
  requireNamespace("ggplot2", quietly = TRUE)
}

##' @rdname package_installed
##' @export
is_leaflet_installed <- function() {
  requireNamespace("leaflet", quietly = TRUE)
}

##' @rdname package_installed
##' @export
is_gstat_installed <- function() {
  requireNamespace("gstat", quietly = TRUE)
}

##' @rdname package_installed
##' @export
is_elevatr_installed <- function() {
  requireNamespace("elevatr", quietly = TRUE)
}
