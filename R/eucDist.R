#' @title Euclidian Distance between two Points
#' @name euc.dist
#' @description Calculates the euclidian distance between two points.
#'
#' @export
#'
#' @param x A numeric value with X and Y coordinates for Point 1 (numeric)
#' @param y A numeric value with X and Y coordinates for Point 2 (numeric)
#'
#' @return Returns a numeric value indicating the euclidian distance between
#' two Points. (numeric)
#'
#' @author Sebastian Gatscha
euc.dist          <- function(x,y) {
  round(sqrt(sum((x - y) ^ 2)),4)
}
