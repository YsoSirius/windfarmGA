#' windfarmGA
#'
#'
#' @description
#' \if{html}{\figure{windfarmGA.png}{options: width="25\%" alt="Figure: windfarmGA.png"}}
#' \if{latex}{\figure{windfarmGA.png}{options: width=0.2in}}
#' A package to optimize small wind farms with irregular shapes
#' using a genetic algorithm. It requires a fixed amount of turbines, a fixed
#' rotor radius and an average wind speed value for each incoming wind
#' direction. A terrain effect model can be included which downloads a digital
#' elevation model and a Corine Land Cover raster to approximate surface
#' roughness. Further information can be found at the description of the
#' function \code{\link{genetic_algorithm}}.
#'
#' @seealso
#' Useful links:
#'  \itemize{
#'    \item \href{https://ysosirius.github.io/windfarmGA/}{Documentation Github.io}
#'    \item \href{https://github.com/YsoSirius/windfarmGA}{Documentation}
#'    \item \href{https://homepage.boku.ac.at/jschmidt/TOOLS/Masterarbeit_Gatscha.pdf}{Master Thesis}
#'    \item \href{https://windfarmga.shinyapps.io/windga_shiny}{Shiny App}
#'    \item \href{https://github.com/YsoSirius/windfarmGA/issues}{Report Issues}
#'  }
#'
#' @importFrom terra xyFromCell extract plot res resample lapp rast crop crs
#'   project terrain app values global classify mosaic viewshed sprc add_legend
#' @importFrom sf st_as_sf st_geometry st_coordinates st_transform st_crs
#'   st_make_grid st_intersection st_area sf_extSoftVersion st_centroid st_bbox
#'   st_cast st_combine `st_crs<-` st_distance st_is_valid
#' @importFrom grDevices colorRampPalette topo.colors
#' @importFrom graphics plot.new text plot par points abline title lines grid
#'   layout axis legend mtext boxplot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom calibrate textxy
#' @importFrom stats runif smooth.spline sd aggregate median dist complete.cases
#'   quantile
#' @importFrom utils download.file unzip read.csv globalVariables
#' @importFrom methods as is
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr %>%
#'
#' @useDynLib windfarmGA, .registration = TRUE
#' @docType package
#' @name windfarmGA_
#' @author \strong{Maintainer}: Sebastian Gatscha \email{sebastian_gatscha@@gmx.at}
NULL
