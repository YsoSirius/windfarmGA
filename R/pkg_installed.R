

is_foreach_installed <- function() {
  requireNamespace("foreach", quietly = TRUE)
}
is_parallel_installed <- function() {
  requireNamespace("parallel", quietly = TRUE)
}
is_doParallel_installed <- function() {
  requireNamespace("doParallel", quietly = TRUE)
}
is_ggplot2_installed <- function() {
  requireNamespace("ggplot2", quietly = TRUE)
}
is_leaflet_installed <- function() {
  requireNamespace("leaflet", quietly = TRUE)
}
is_gstat_installed <- function() {
  requireNamespace("gstat", quietly = TRUE)
}
is_elevatr_installed <- function() {
  requireNamespace("elevatr", quietly = TRUE)
}
