#' @title Plot heatmap of fit grid cells
#' @name heatmapGA
#' @description  Plot a heatmap of the selected grid cells. Green grid
#' cells have been selected more often than red grid cells.
#'
#' @export
#'
#' @importFrom geoR dup.coords
#' @importFrom sp SpatialPoints coordinates gridded
#' @importFrom gstat idw
#' @importFrom rgl open3d rgl.surface rgl.snapshot
#' @importFrom ggplot2 ggplot geom_tile geom_point aes scale_fill_gradient
#' coord_equal labs
#'
#'
#' @param result The output matrix of \code{\link{windfarmGA}} or
#' \code{\link{genAlgo}}, which has stored all relevant information
#' @param si A numeric value that is used for the sizing of the
#' resolution of the heatmap. Default is 2 
#' @param idistw The inverse distance weighting power. Default is the
#' rotor radius from the 'result' values 
#'
#' @return NULL
#' @examples \donttest{
#' ## Add some data examples from the package
#' load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
#'
#' ## Plot the results of a hexagonal grid optimization
#' result <- resulthex
#' heatmapGA(result)
#'
#' ## Plot the heatmap with different settings
#' heatmapGA(result, si = 6, idistw = 2)
#' heatmapGA(result, si = 6, idistw = 100)
#' heatmapGA(result, si = 20, idistw = 10)
#' }
#' @author Sebastian Gatscha
heatmapGA <- function(result, si = 2, idistw){
  parheat <- par(ask = FALSE, no.readonly = TRUE)
  on.exit(par(parheat))
  par(mfrow = c(1, 1))

  bpe <- do.call("rbind", result[, "allCoords"]);
  rownames(bpe) <- NULL
  bpe <- data.frame(bpe[, 1:2])

  sizingidw <- as.integer(result[, "inputData"][[1]][, 1]["Rotorradius"])
  sizing <- as.integer(result[, "inputData"][[1]][, 1]["Resolution"]) / si

  dupco <- geoR::dup.coords(bpe, simplify = TRUE)

  bpe$Ids <- as.integer(rownames(bpe))

  dupco <- lapply(dupco, function(x) as.integer(x))
  dupcosum <- lapply(dupco, function(x) length(x))
  bpenew <- vector("list", length(dupco))
  for (i in 1:length(dupco)){
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
  ## convert grid to SpatialPixel class
  sp::coordinates(grd) <- ~ x + y
  sp::gridded(grd) <- TRUE


  if (missing(idistw)){
    idistw <- sizingidw
  } else {
    idistw <- idistw
  }

  idwout <- as.data.frame(gstat::idw(formula = bpenew$Sum ~ 1,
                                     locations = polo, newdata = grd,
                                     idp = idistw))


  plot1 <- ggplot2::ggplot(data = idwout,
                           mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_tile(data = idwout, ggplot2::aes(fill = var1.pred),
                       show.legend = TRUE) +
    ggplot2::labs(
      title = "Inverse Distance Weighting for Grid Cell Selection") +
    ggplot2::geom_point(data = bpenew, mapping = ggplot2::aes(x = X, y = Y),
                        show.legend = TRUE, size = sqrt(sqrt(bpenew$Sum)),
                        alpha = 0.6)

  plot1 +
    ggplot2::scale_fill_gradient(low = "red", high = "green") +
    ggplot2::coord_equal()
}