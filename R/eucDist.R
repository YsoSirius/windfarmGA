#' @title Euclidian Distance between two Points
#' @name euc.dist
#' @description Calculates the euclidian distance between two points.
#'
#' @export
#'
#' @param x A numeric value with X and Y coordinates for Point 1
#' @param y A numeric value with X and Y coordinates for Point 2
#'
#' @return Returns a numeric value indicating the euclidian distance between
#' two Points.
#'
#' @examples
#' x = c(200, 100)
#' y = c(1000, 2000)
#' euc.dist(x, y)
#'
#' @author Sebastian Gatscha
euc.dist <- function(x,y){sqrt(sum((x - y) ^ 2))}
