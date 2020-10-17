#' windfarmGA
#' 
#' 
#' @description 
#' \if{html}{\figure{windfarmGA.png}{options: width="25\%" align=right alt="Figure: windfarmGA.png"}} 
#' \if{latex}{\figure{windfarmGA.png}{options: width=0.2in}} 
#' A package to optimize small wind farms with irregular shapes
#' using a genetic algorithm. It requires a fixed amount of turbines, a fixed
#' rotor radius and an average wind speed value for each incoming wind
#' direction. A terrain effect model can be included that downloads an 'SRTM'
#' elevation model and loads a Corine Land Cover raster to approximate surface
#' roughness. Further information can be found in the description of the
#' function \code{\link{windfarmGA}}.
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
#' @importFrom raster crs getData crop mask projectRaster reclassify extent
#'   rasterize res rasterToPolygons plot area intersect raster calc extract
#'   cellStats terrain resample overlay cellFromXY ncell projection values wkt
#' @importFrom sp spTransform proj4string Polygon Polygons SpatialPolygons CRS
#'   coordinates SpatialPoints gridded spsample over bbox
#' @importFrom sf st_as_sf st_geometry st_coordinates st_transform st_crs
#' @importFrom rgdal readOGR
#' @importFrom grDevices colorRampPalette topo.colors
#' @importFrom graphics plot.new text plot par points abline title lines grid
#'   layout axis legend mtext
#' @importFrom RColorBrewer brewer.pal
#' @importFrom calibrate textxy
#' @importFrom ggplot2 ggplot geom_tile geom_point geom_bar aes
#'   scale_fill_gradient coord_equal labs  scale_x_discrete waiver coord_polar
#'   scale_fill_manual theme element_blank ylim
#' @importFrom leaflet colorFactor iconList makeIcon leaflet addTiles
#'   addProviderTiles popupOptions addMarkers addCircleMarkers hideGroup
#'   addPolygons addLegend labelFormat addLayersControl layersControlOptions
#'   addPopups
#' @importFrom stats runif smooth.spline sd aggregate median dist complete.cases
#'   quantile
#' @importFrom utils download.file unzip read.csv globalVariables
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom foreach foreach %dopar% 
#' @importFrom methods as slot slot<-
#' @importFrom gstat idw
#' @importFrom spatstat is.tess hextess hexgrid owin spatstat.options
#' @importFrom magrittr %>% 
#' @importFrom rworldmap getMap
#' @importFrom Rcpp sourceCpp
#' 
#' @useDynLib windfarmGA, .registration = TRUE
#' @docType package
#' @name windfarmGA_
#' @author \strong{Maintainer}: Sebastian Gatscha \email{sebastian_gatscha@@gmx.at}
NULL
