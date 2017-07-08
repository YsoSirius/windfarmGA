#' @title Leaflet Plot of a Wind Park
#' @name leafPlot
#' @description  Plot a resulting wind farm on a leaflet map.
#'
#' @export
#'
#' @importFrom leaflet colorFactor iconList makeIcon leaflet addTiles
#' addProviderTiles popupOptions addMarkers addCircleMarkers hideGroup
#' addPolygons addLegend labelFormat addLayersControl layersControlOptions
#' @importFrom sp proj4string SpatialPoints CRS spTransform coordinates
#' @importFrom grDevices colorRampPalette
#' @importFrom dplyr %>%
#'
#' @param result The resulting matrix of the function 'genAlgo' or
#' 'windfarmGA'. (matrix)
#' @param Polygon1 The Polygon for the wind farm area. (SpatialPolygons)
#' @param which A numeric value, indicating which best indidvual to plot.
#' The default is 1 (the best resulting wind farm). (numeric)
#'
#' @return Returns a leaflet map. (leaflet)
#'
#' @examples \donttest{
#' library(sp)
#' ## Create a random Polygon and plot it
#' Polygon1 <- Polygon(rbind(c(4651704, 2692925), c(4651704, 2694746),
#'                           c(4654475, 2694746), c(4654475, 2692925)))
#' Polygon1 <- Polygons(list(Polygon1),1);
#' Polygon1 <- SpatialPolygons(list(Polygon1))
#' Projection <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' proj4string(Polygon1) <- CRS(Projection)
#' plot(Polygon1,axes=TRUE)
#'
#' ## Uniform wind speed and single wind direction
#' data.in <- as.data.frame(cbind(ws=12,wd=0))
#'
#' ## Run an optimization run with hexagonal grid cells.
#' result_hex <- genAlgo(Polygon1 = Polygon1, GridMethod ="h", n=12, Rotor=50,
#'                      fcrR=10,iteration=6, vdirspe = data.in,crossPart1 = "EQU",
#'                      selstate="FIX",mutr=0.8, Proportionality = 1,
#'                      SurfaceRoughness = 0.3, topograp = FALSE,
#'                      elitism=TRUE, nelit = 7, trimForce = TRUE,
#'                      referenceHeight = 50,RotorHeight = 100)
#'
#' ## Plot the resulting 'best' wind farm on a leaflet map.
#' leafPlot(result = result_hex, Polygon1 = Polygon1, which = 1)
#' }
#' @author Sebastian Gatscha
leafPlot <- function(result,Polygon1,which=1){
  if (which > nrow(result)){
    cat(paste("Maximum possible number for 'which': ",nrow(result)))
    which <- nrow(result)
  }

  result <- result[,'bestPaEn'][[which]]
  ProjectionLonLat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  projPol <- sp::proj4string(Polygon1)
  xysp <- sp::SpatialPoints(cbind(result$X,result$Y), proj4string = CRS(projPol))
  # sp::proj4string(xysp) <- CRS(projPol)
  resultxy <- sp::spTransform(xysp, CRSobj = ProjectionLonLat)
  ## Transform to matrix after transformation.
  resultxy <- sp::coordinates(resultxy)
  result$X <- resultxy[,1]
  result$Y <- resultxy[,2]
  Polygon1 <- sp::spTransform(Polygon1,CRSobj = ProjectionLonLat)

  headLo <- c(mean(result[,1]), max(result[,2]))

  colCir <-   grDevices::colorRampPalette(c('green','yellow','red','darkred'));
  br = length(levels(factor(result$AbschGesamt)))
  if (br > 1) {
    ColC1 <- colCir(br)[as.numeric(cut(result$AbschGesamt,breaks = br))]
  } else {
    ColC1 <- "green"
  }
  Rad =  round(result$AbschGesamt,2)/10;
  ## Assign sorted color palette for legend
  pal <- leaflet::colorFactor(ColC1, domain = result$AbschGesamt)

  ## Assign turbine Icons
  turbine_icon <- leaflet::iconList(
    turbine_icon = leaflet::makeIcon(
      # iconUrl = paste0(system.file(package = "windfarmGA"), "/extdata/windtur.png"),
      iconUrl = paste0(system.file(package = "windfarmGA"), "/extdata/windturdk.png"),
      # iconUrl = paste0(getwd(),"/inst/extdata/windturdk.png"),
      iconWidth = 30, iconHeight = 50))
  listPopup <- paste("Total Wake Effect: ", as.character(result$AbschGesamt),
    "% </dd>")
  ## Start a Leaflet Map with OSM background and another Tile.
  map <- leaflet::leaflet() %>%
    leaflet::addTiles(group = "OSM (default)") %>%
    leaflet::addProviderTiles("Stamen.Toner", group = "Toner") %>%
    ## Write a Popup with the energy output
    leaflet::addPopups(headLo[1], (headLo[2]+0.0043), group = "Title",
         popup = paste("<b>Best Wind Farm with: ",
        round(result$EnergyOverall[[1]],2),"kWh</b>"),
        options = leaflet::popupOptions(closeButton = T,
        closeOnClick = T)) %>%
    ## Add the turbine symbols
    leaflet::addMarkers(lng=result[,1], lat=result[,2],
               icon= turbine_icon[1], popup=listPopup, group="Turbines") %>%
    ## Create Circles in Map
    leaflet::addCircleMarkers(lng=result[,1],
                     lat=result[,2],
                     radius = Rad,
                     color = ColC1,
                     stroke = T, fillOpacity = 0.8,
                     group="Wake_Circles") %>%
    # hideGroup(group="Wake_Circles") %>%

    ## Add the Polygon
    leaflet::addPolygons(data = Polygon1, group = "Polygon",
                fill=TRUE,fillOpacity = 0.4) %>%
    ## Add legend of Wake effects
    leaflet::addLegend(position = "topleft",  pal=pal, values =
                result$AbschGesamt,
              labFormat = leaflet::labelFormat(suffix = "%"),
              opacity = 1, title = "Total Wake Effect", layerId = "Legend",
              labels="Wake") %>%

    ## Layers control
    leaflet::addLayersControl(baseGroups = c("OSM","Toner"),
                    overlayGroups = c("Wake_Circles","Title","Polygon","Turbines"),
                    options = leaflet::layersControlOptions(collapsed = T)
  )
  # Plot the map
  map
}




