#' @title Get the Grid-IDs from binary matrix
#' @name getRects
#' @description  Get the grid IDs from the trimmed binary matrix, where the
#' binary code indicates which grid cells are used in the current
#' wind farm constellation.
#'
#' @export
#'
#' @param trimtonOut Input matrix with binary values. (matrix)
#' @param Grid Grid of the considered area (data.frame)
#'
#' @return Returns a list of all individuals with X and Y coordinates
#' and the grid cell ID. (list)
#' @examples \donttest{
#' ## Create a random rectangular shapefile
#' library(sp)
#' Polygon1 <- Polygon(rbind(c(0, 0), c(0, 2000), c(2000, 2000), c(2000, 0)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' plot(Polygon1,axes=TRUE)
#'
#' ## Calculate a Grid and an indexed data.frame with coordinates and grid cell Ids.
#' Grid1 <- GridFilter(shape = Polygon1,resol = 200,prop = 1);
#' Grid <- Grid1[[1]]
#' AmountGrids <- nrow(Grid)
#'
#' startsel <- StartGA(Grid,10,20);
#' wind <- as.data.frame(cbind(ws=12,wd=0))
#' fit <- fitness(selection = startsel,referenceHeight = 100, RotorHeight=100,
#'                SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,rot=20,
#'                dirspeed = wind, srtm_crop="",topograp=FALSE,cclRaster="")
#' allparks <- do.call("rbind",fit);
#'
#' ## SELECTION
#' ## print the amount of Individuals selected.
#' ## Check if the amount of Turbines is as requested.
#' selec6best <- selection1(fit, Grid,2, TRUE, 6, "VAR");
#' selec6best <- selection1(fit, Grid,2, TRUE, 6, "FIX");
#' selec6best <- selection1(fit, Grid,4, FALSE, 6, "FIX");
#'
#' ## CROSSOVER
#' ## u determines the amount of crossover points,
#' ## crossPart determines the method used (Equal/Random),
#' ## uplimit is the maximum allowed permutations
#' crossOut <- crossover1(selec6best, 2, uplimit = 300, crossPart="RAN");
#' crossOut <- crossover1(selec6best, 7, uplimit = 500, crossPart="RAN");
#' crossOut <- crossover1(selec6best, 3, uplimit = 300, crossPart="EQU");
#'
#' ## MUTATION
#' ## Variable Mutation Rate is activated if more than 2 individuals represent the
#' ## current best solution.
#' mut <- mutation(a = crossOut, p = 0.3);
#' mut==crossOut
#'
#' ## TRIMTON
#' ## After Crossover and Mutation, the amount of turbines in a windpark change
#' ## and have to be corrected to the required amount of turbines.
#' mut1 <- trimton(mut = mut, nturb = 10, allparks = allparks,
#'                nGrids = AmountGrids, trimForce=FALSE)
#' colSums(mut1)
#'
#' ## Get the new Grid-Ids and run a new fitness run.
#' getRectV <- getRects(mut1, Grid)
#' fit <- fitness(selection = getRectV,referenceHeight = 100, RotorHeight=100,
#'                SurfaceRoughness=0.3,Polygon = Polygon1, resol1 = 200,rot=20,
#'                dirspeed = wind, srtm_crop="",topograp=FALSE,cclRaster="")
#' head(fit)
#'
#'}
#' @author Sebastian Gatscha
getRects          <- function(trimtonOut, Grid){

  childli <- list();
  len1 <- dim(trimtonOut)[2]

  for (i in 1:len1) {
    childli[[i]] <- trimtonOut[,i]
  }

  rectidli <- list();
  for (u in 1:len1){
    rectidli[[u]] <- which(childli[[u]]==1, arr.ind = T)
  }

  childnew <- list()
  for (z in 1:len1) {
    childnew[[z]] <- Grid[rectidli[[z]],];
  }

  return(childnew)
}
