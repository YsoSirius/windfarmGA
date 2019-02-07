#' @title Leaflet Plot of a Wind Park
#' @name leafPlot
#' @description  Plot a resulting wind farm on a leaflet map.
#'
#' @export
#' 
#' @importFrom leaflet colorFactor iconList makeIcon leaflet addTiles
#' addProviderTiles popupOptions addMarkers addCircleMarkers hideGroup
#' addPolygons addLegend labelFormat addLayersControl layersControlOptions
#' addPopups
#' 
#' @importFrom sp proj4string SpatialPoints CRS spTransform coordinates
#' SpatialPolygons Polygon Polygons
#' @importFrom grDevices colorRampPalette
#' @importFrom raster extent
#' @importFrom magrittr %>%
#'
#' @param result The resulting matrix of the function 'genAlgo' or
#' 'windfarmGA'. (matrix)
#' @param Polygon1 The Polygon for the wind farm area. (SpatialPolygons)
#' @param which A numeric value, indicating which best indidvual to plot.
#' The default is 1 (the best resulting wind farm). (numeric)
#' @param orderitems A logical value indicating whether the results should
#' be ordered by energy values (TRUE) or chronologically (FALSE). (logical)
#' @param GridPol The output grid polygon of the \code{\link{GridFilter}} or
#' \code{\link{HexaTex}} functions. (SpatialPolygons)
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
#'          
#' load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
#' ## Plot the best wind farm on a leaflet map with the rectangular Grid
#' Grid <- GridFilter(polygon, resol = 150, prop = 0.4)
#' leafPlot(result = resultrect, Polygon1 = polygon, which = 1, 
#'          GridPol = Grid[[2]])
#'
#' ## Plot the last wind farm with hexagonal Grid
#' Grid <- HexaTex(polygon, size = 75)
#' leafPlot(result = resulthex, Polygon1 = polygon, which = 1, 
#'          GridPol = Grid[[2]])
#' }
#' @author Sebastian Gatscha
leafPlot <- function(result, Polygon1, which = 1, orderitems = TRUE, GridPol){
  
  if (which > nrow(result)){
    cat(paste("Maximum possible number for 'which': ",nrow(result)))
    which <- nrow(result)
  }
  
  if (orderitems){
    a <- sapply(result[,2], FUN = function(i) subset.matrix(i, subset = c(T, rep(F, nrow(i)-1)),
                                                            select = "EnergyOverall"))
    b <- data.frame(cbind(a), stringsAsFactors = FALSE)
    order1 <- order(b, decreasing = T)
    result <- result[order1,]
    beste <- which
  } else {
    beste <- ""
  }
  ProjectionLonLat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  if (!missing(GridPol)){
    GridPol <- sp::spTransform(GridPol, CRSobj = ProjectionLonLat)
    overGroups <- c("Wake Circles","Title","Polygon","Turbines", "Grid")
    opaC <- 0.4
  } else {
    overGroups <- c("Wake Circles","Title","Polygon","Turbines", "Grid")
    xses <- rep(Polygon1@bbox[1],4); yses <- rep(Polygon1@bbox[4],4)
    Sr1 <- sp::SpatialPolygons(list(sp::Polygons(list(
      sp::Polygon(cbind(xses,yses))), ID = "a")), pO = 1:1)
    GridPol <- Sr1
    sp::proj4string(GridPol) <- sp::proj4string(Polygon1)
    GridPol <- sp::spTransform(GridPol, CRSobj = ProjectionLonLat)
    opaC <- 0
  }
  
  result <- result[,'bestPaEn'][[which]]
  projPol <- sp::proj4string(Polygon1)
  xysp <- sp::SpatialPoints(cbind(result[,'X'], result[,'Y']), proj4string = sp::CRS(projPol))
  resultxy <- sp::spTransform(xysp, CRSobj = ProjectionLonLat)
  resultxy <- sp::coordinates(resultxy)
  
  Polygon1 <- sp::spTransform(Polygon1, CRSobj = ProjectionLonLat)
  
  headLo <- c(mean(raster::extent(Polygon1)[1:2]), max(raster::extent(Polygon1)[4]))
  
  colCir <- grDevices::colorRampPalette(c('green','yellow','red','darkred'));
  br = length(levels(factor(result[,'AbschGesamt'])))
  if (br > 1) {
    ColC1 <- colCir(br)
  } else {
    ColC1 <- "green"
  }
  
  Rad =  round(result[,'AbschGesamt'],2)/10;
  names(Rad) <- NULL
  result <- data.frame(result, stringsAsFactors = FALSE)
  result$X <- resultxy[,1]
  result$Y <- resultxy[,2]
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
  map <- leaflet() %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.Terrain", group="Terrain") %>%
    addProviderTiles("Esri.WorldImagery", group="Satellite") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    ## Write a Popup with the energy output
    leaflet::addPopups(headLo[1], (headLo[2]+0.0002), group = "Title",
                       popup = paste(beste,"<b>Best Wind Farm with: ",
                                     round(as.numeric(result[,'EnergyOverall'][[1]]),2),"kWh</b>"),
                       options = popupOptions(closeButton = T,
                                                       closeOnClick = T)) %>%
    ## Add the Polygon
    addPolygons(data = Polygon1, group = "Polygon",
                fill = TRUE,fillOpacity = 0.4) %>%

    ## Add the Genetic Algorithm Space
    addPolygons(data = GridPol, group = "Grid", weight = 1,
                # color="#222760",
                opacity = opaC,
                fill = TRUE, fillOpacity = opaC) %>%

    ## Create Circles in Map
    addCircleMarkers(lng = result$X, lat = result$Y,
                              radius = Rad,
                              color = result$farbe,
                              stroke = T, fillOpacity = 0.8,
                              group="Wake Circles") %>%
    ## Add the turbine symbols
    addMarkers(lng=result[,1], lat=result[,2],
                        icon= turbine_icon[1], popup=listPopup, group="Turbines") %>%
    addLegend(position = "topleft",
              colors = sort(unique(result$farbe)),
              values = as.character(result$AbschGesamt), 
              # colors=ColC1,
              labels = sort(unique(result$AbschGesamt)),
              labFormat = labelFormat(suffix = "%"),
              opacity = 1, title = "Total Wake Effect", layerId = "Legend")  %>%     
  ## Layers control
    addLayersControl(baseGroups = c(
      "OSM",
      "Terrain",
      "Satellite",
      "Toner"),
      overlayGroups = overGroups,
      options = layersControlOptions(collapsed = T)
    )
 
  
  
  # Plot the map
  map
}
