#' @title Plot a heatmap of selected grid cells
#' @name plot_heatmap
#' @description  Plot a heatmap of selected grid cells. Green grid cells
#'   have been selected more often than red grid cells.
#'
#' @export
#'
#' @family Plotting Functions
#' @param result The output of \code{\link{windfarmGA}} or
#'   \code{\link{genetic_algorithm}}
#' @param si A numeric value that is used for the sizing of the resolution of
#'   the heatmap. Default is 2
#' @param idistw The inverse distance weighting power. Default is the rotor
#'   radius from the 'result' values
#'
#' @return Invisibly returns a list with the result of the inverse distance
#'   weighting and an aggregated dataframe of all grid cells
#' @examples \donttest{
#' ## Add some data examples from the package
#' load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
#'
#' ## Plot the results of a hexagonal grid optimization
#' plot_heatmap(resulthex)
#'
#' ## Plot the heatmap with different settings
#' plot_heatmap(resulthex, si = 6, idistw = 2)
#' plot_heatmap(resulthex, si = 6, idistw = 100)
#' plot_heatmap(resulthex, si = 20, idistw = 10)
#' }
plot_heatmap <- function(result, si = 2, idistw){
  parheat <- par(ask = FALSE, no.readonly = TRUE)
  on.exit(par(parheat))
  par(mfrow = c(1, 1))
  
  bpe <- do.call("rbind", result[, "allCoords"])
  rownames(bpe) <- NULL
  bpe <- data.frame(bpe[, 1:2])
  
  sizingidw <- as.integer(result[, "inputData"][[1]][, 1]["Rotorradius"])
  sizing <- as.integer(result[, "inputData"][[1]][, 1]["Resolution"]) / si
  
  # dupco <- geoR::dup.coords(bpe, simplify = TRUE)
  dupco <- dup_coords(bpe, simplify = TRUE)
  
  bpe$Ids <- as.integer(rownames(bpe))
  
  dupco <- lapply(dupco, function(x) as.integer(x))
  dupcosum <- lapply(dupco, function(x) length(x))
  bpenew <- vector("list", length(dupco))
  for (i in 1:length(dupco)) {
    bpenew[[i]] <- bpe[bpe$Ids == dupco[[i]][1], ]
    bpenew[[i]]$Sum <- dupcosum[[i]][1]
  }
  bpenew <- do.call("rbind", bpenew)
  bpenew <- bpenew[-3]
  
  polo <- sp::SpatialPoints(sp::coordinates(cbind(bpenew$X, bpenew$Y)))
  
  extra_margin <- 50
  x_range <-   range(bpenew$X)
  y_range <-   range(bpenew$Y)
  grd <- expand.grid(x = seq(from = x_range[1] - extra_margin,
                             to = x_range[2] + extra_margin, by = sizing),
                     y = seq(from = y_range[1] - extra_margin,
                             to = y_range[2] + extra_margin, by = sizing))
  
  ## TODO - Need global variable for NSE-values (x,y,var1.pred,X,Y). How to avoid it?
  ## convert grid to SpatialPixel class
  sp::coordinates(grd) <- ~ x + y
  sp::gridded(grd) <- TRUE
  
  if (missing(idistw)) {
    idistw <- sizingidw
  } else {
    idistw <- idistw
  }
  
  ## Calculate IDW
  idwout <- data.frame(gstat::idw(formula = bpenew$Sum ~ 1,
                                  locations = polo, newdata = grd,
                                  idp = idistw))
  
  ## Plot heatmap
  plot1 <- ggplot2::ggplot(data = idwout,
                           mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_tile(data = idwout, ggplot2::aes(fill = var1.pred),
                       show.legend = TRUE) +
    ggplot2::labs(
      title = "Inverse Distance Weighting for Grid Cell Selection") +
    ggplot2::geom_point(data = bpenew, mapping = ggplot2::aes(x = X, y = Y),
                        show.legend = TRUE, size = sqrt(sqrt(bpenew$Sum)),
                        alpha = 0.6)
  
  plot1 <- plot1 +
    ggplot2::scale_fill_gradient(low = "red", high = "green") +
    ggplot2::coord_equal()
  
  print(plot1)
  
  invisible(list("idw" = idwout,
                 "GA_grids" = bpenew))
}


#' @title Splits duplicated coords (copy of geoR::dup.coords)
#' @name dup_coords
#' @description This function takes an object with 2-D coordinates and returns
#' the positions of the duplicated coordinates. Also sets a method for
#' duplicated. Helper function for \code{\link{plot_heatmap}}
#'
#' @export
#'
#' @param x Two column numeric matrix or data frame
#' @param ... passed to sapply. If simplify = TRUE (default) results are
#' returned as an array if possible (when the number of replicates are the
#' same at each replicated location)
#' 
#' @family Helper Functions
#' @return Function and methods returns NULL if there are no duplicates 
#' locations. Otherwise, the default method returns a list where each component
#' is a vector with the positions or the rownames, if available, of the
#' duplicates coordinates. The method for geodata returns a data-frame with
#' rownames equals to the positions of the duplicated coordinates, the first
#' column is a factor indicating duplicates and the remaining are output of
#' as.data.frame.geodata.
#' 
#' @author Paulo Justiniano Ribeiro Jr. \email{paulojus@@leg.ufpr.br}
#' Peter J. Diggle \email{p.diggle@@lancaster.ac.uk}
dup_coords <- function(x, ...) {
  ap1 <- unclass(factor(paste("x", x[, 1], "y", x[, 2], sep = "")))
  ap2 <- table(ap1)
  ap2 <- ap2[ap2 > 1]
  takecoords <- function(n){
    if (!is.null(rownames(x))) rownames(x[ap1 == n, ])
  }
  res <- sapply(as.numeric(names(ap2)), takecoords, ...)
  if (!is.null(res)) class(res) <- "duplicated.coords"
  return(res)
}
