#' @title Divide matrices or integer at certain locations
#' @name splitAt
#' @description  Required function for the crossover method, to
#' split a genetic code at random intervals. See also \code{\link{crossover1}}.
#' @export
#' @param x A numeric variable representing the binary genetic code of an
#' individual (numeric)
#' @param pos A numeric value, which shows at which position the
#' genetic code is cut (numeric)
#'
#' @return Returns a list of the splitted genetic code.
#'
#' @examples \donttest{
#' splitAt(1:100,20)
#' splitAt(as.matrix(1:100),20)
#'}
#' @author Sebastian Gatscha
splitAt           <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

