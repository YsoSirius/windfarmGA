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
#' @param orderitems A logical value indicating whether the results should
#' be ordered by energy values (TRUE) or chronologically (FALSE). (logical)
#'
#' @return Returns a leaflet map. (leaflet)
#'
#' @examples \donttest{
#' load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
#'
#' ## Plot the best wind farm on a leaflet map (ordered by energy values)
#' leafPlot(result = resulthex, Polygon1 = polygon, which = 1)
#'
#' ## Plot the last wind farm (ordered by chronology).
#' leafPlot(result = resulthex, Polygon1 = polygon, orderitems = F,
#'          which = 1)
#' }
#' @author Sebastian Gatscha
leafPlot <- function(result,Polygon1,which=1,orderitems=TRUE){

  if (which > nrow(result)){
    cat(paste("Maximum possible number for 'which': ",nrow(result)))
    which <- nrow(result)
  }

  if (orderitems==TRUE){
    a <- sapply(result[,2], "[", "EnergyOverall")
    b <- data.frame(sapply(a, function(x) x[1]))
    order1 <- order(b, decreasing = T)
    result <- result[order1,]
    beste <- which
  } else {
    beste <- ""
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

  # extent(Polygon1)[4]
  # headLo <- c(mean(result[,1]), max(result[,2]))
  headLo <- c(mean(extent(Polygon1)[1:2]), max(extent(Polygon1)[4]))

  colCir <- grDevices::colorRampPalette(c('green','yellow','red','darkred'));
  br = length(levels(factor(result$AbschGesamt)))
  if (br > 1) {
    ColC1 <- colCir(br)
  } else {
    ColC1 <- "green"
  }

  Rad =  round(result$AbschGesamt,2)/10;
  ## Assign sorted color palette for legend
  pal <- leaflet::colorFactor(ColC1, domain = result$AbschGesamt,
                              reverse = F)
  result$Rad = Rad
  result$farbe = pal(result$AbschGesamt)

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
    leaflet::addPopups(headLo[1], (headLo[2]+0.0002), group = "Title",
         popup = paste(beste,"<b>Best Wind Farm with: ",
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
                     # color = sort(pal(result$AbschGesamt), decreasing = T),
                     # color = sort(result$farbe, decreasing = T),
                     color = result$farbe,
                     stroke = T, fillOpacity = 0.8,
                     group="Wake_Circles") %>%
    # hideGroup(group="Wake_Circles") %>%


    ## Add the Polygon
    leaflet::addPolygons(data = Polygon1, group = "Polygon",
                fill=TRUE,fillOpacity = 0.4) %>%

    ## Add legend of Wake effects
    # leaflet::addLegend(position = "topleft",
    #                      colors = pal(sort(unique(result$AbschGesamt))),
    #                    labels = sort(unique(result$AbschGesamt),decreasing = T),
    #           labFormat = leaflet::labelFormat(suffix = "%"),
    #           opacity = 1, title = "Total Wake Effect", layerId = "Legend") %>%

    leaflet::addLegend(position = "topleft",
                       # colors = sort(unique(result$farbe)),
                       colors=ColC1,
                       labels = sort(unique(result$AbschGesamt)),
                       labFormat = leaflet::labelFormat(suffix = "%"),
                       opacity = 1, title = "Total Wake Effect", layerId = "Legend") %>%

    ## Layers control
    leaflet::addLayersControl(baseGroups = c("OSM","Toner"),
                    overlayGroups = c("Wake_Circles","Title","Polygon","Turbines"),
                    options = leaflet::layersControlOptions(collapsed = T)
  )
  # Plot the map
  map
}
