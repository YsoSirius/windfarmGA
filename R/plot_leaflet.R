#' @title Plot a wind warm with leaflet
#' @name plot_leaflet
#' @description  Plot a resulting wind farm on a leaflet map.
#'
#' @export
#'
#' @inheritParams genetic_algorithm
#' @param result The resulting matrix of the function \code{\link{genetic_algorithm}}
#' or \code{\link{windfarmGA}}
#' @param which A numeric value, indicating which individual to plot. The
#'   default is 1. Combined with \code{orderitems = TRUE} this will show the 
#'   best performing wind farm.
#' @param orderitems A logical value indicating whether the results should be
#'   ordered by energy values \code{TRUE} or chronologically \code{FALSE}
#' @param GridPol By default, the grid will be calculated based on the inputs 
#'  of \code{result} and the \code{Polygon1}. But another spatial object or the 
#'  output of the  \code{\link{grid_area}} or \code{\link{hexa_area}} functions 
#'  can also be
#'
#' @return Returns a leaflet map.
#'
#' @examples \donttest{
#' ## Plot the best wind farm on a leaflet map (ordered by energy values)
#' plot_leaflet(result = resulthex, Polygon1 = sp_polygon, which = 1)
#'
#' ## Plot the last wind farm (ordered by chronology).
#' plot_leaflet(result = resulthex, Polygon1 = sp_polygon, orderitems = FALSE,
#'          which = 1)
#'          
#' ## Plot the best wind farm on a leaflet map with the rectangular Grid
#' Grid <- grid_area(sp_polygon, size = 150, prop = 0.4)
#' plot_leaflet(result = resultrect, Polygon1 = sp_polygon, which = 1, 
#'              GridPol = Grid[[2]])
#'
#' ## Plot the last wind farm with hexagonal Grid
#' Grid <- hexa_area(sp_polygon, size = 75)
#' plot_leaflet(result = resulthex, Polygon1 = sp_polygon, which = 1, 
#'              GridPol = Grid[[2]])
#' }
plot_leaflet <- function(result, Polygon1, which = 1, orderitems = TRUE, GridPol){
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("The package 'leaflet' is required for this function, but it is not installed.\n",
         "Please install it with `install.packages('leaflet')`")
  }
  
  poly1 <- isSpatial(shape = Polygon1)
  if (is.na(st_crs(poly1))) {
    projection <- result[, "inputData"][[1]]["Projection", ][[1]]
    projection <- tryCatch(as.numeric(projection),
                           warning = function(e) projection,
                           error = function(e) projection)
    st_crs(poly1) <- st_crs(projection)
  }
  proj_pol <- st_crs(poly1)
  
  if (which > nrow(result)) {
    cat(paste("Maximum possible number for 'which': ", nrow(result)))
    which <- nrow(result)
  }
  
  if (orderitems) {
    a <- sapply(result[, 2], FUN = function(i) {
      subset.matrix(i, subset = c(TRUE, rep(FALSE, nrow(i) - 1)),
                    select = "EnergyOverall")
    })
    b <- data.frame(cbind(a), stringsAsFactors = FALSE)
    order1 <- order(b, decreasing = TRUE)
    result <- result[order1, ]
    beste <- which
  } else {
    beste <- ""
  }
  
  PROJ6 <- utils::compareVersion(sf::sf_extSoftVersion()[[3]], "6") > 0
  if (PROJ6) {
    proj_longlat <- 4326
  } else {
    proj_longlat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }
  
  if (!missing(GridPol)) {
    if (is.na(st_crs(GridPol))) {
      st_crs(GridPol) <- st_crs(proj_pol)
    }
  } else {
    cellsize <- as.numeric(result[, "inputData"][[1]]["Resolution",][[1]])
    if (result[, "inputData"][[1]]["Grid Method",][[1]] != "h") {
      GridPol <- grid_area(poly1, cellsize, 
                           prop = as.numeric(result[, "inputData"][[1]]["Percentage of Polygon",][[1]]),
                           plotGrid = FALSE)[[2]]
    } else {
      GridPol <- hexa_area(poly1, cellsize,  plotGrid = FALSE)[[2]]
    }
  }
  GridPol <- st_transform(GridPol, st_crs(proj_longlat))
  overlay_group <- c("Wake Circles", "Title", "Polygon", "Turbines", "Grid")
  opaycity <- 0.4
  
  result <- result[, "bestPaEn"][[which]]
  xysp <- st_as_sf(data.frame(result), coords=c("X","Y"))
  st_crs(xysp) <- proj_pol
  resultxy <- st_transform(xysp, proj_longlat)
  resultxy <- st_coordinates(resultxy)
  
  poly1 <- st_transform(poly1, proj_longlat)
  
  bbx <- st_bbox(poly1)
  title_locat <- c(mean(bbx[c(1,3)]),
                   max(bbx[c(2,4)]))
  
  col_cir <- grDevices::colorRampPalette(c("green", "yellow",
                                           "red", "darkred"))
  br <- length(levels(factor(result[, "AbschGesamt"])))
  if (br > 1) {
    color_pal <- col_cir(br)
  } else {
    color_pal <- "green"
  }
  
  wake_radius <- round(result[, "AbschGesamt"], 2) / 10
  names(wake_radius) <- NULL
  result <- data.frame(result, stringsAsFactors = FALSE)
  result$X <- resultxy[, 1]
  result$Y <- resultxy[, 2]
  ## Assign sorted color palette for legend
  pal <- leaflet::colorFactor(color_pal, domain = sort(result$AbschGesamt), 
                              ordered = TRUE,
                              reverse = FALSE)
  
  result$wake_radius <- wake_radius
  result$farbe <- pal(result$AbschGesamt)
  
  ## Assign turbine Icons
  turbine_icon <- leaflet::iconList(
    turbine_icon = leaflet::makeIcon(
      iconUrl = paste0(system.file(package = "windfarmGA"),
                       "/extdata/windturdk.png"),
      # iconUrl = paste0(getwd(),"/inst/extdata/windturdk.png"),
      iconWidth = 30, iconHeight = 50))
  list_popup <- paste("Total Wake Effect: ", as.character(result$AbschGesamt),
                      "% </dd>")
  
  ## Start a Leaflet Map with OSM background and another Tile.
  map <- leaflet() %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    ## Write a Popup with the energy output
    leaflet::addPopups(title_locat[1], (title_locat[2] + 0.0002),
                       group = "Title",
                       popup = paste(beste, "<b>Best Wind Farm with: ",
                                     round(as.numeric(
                                       result[, "EnergyOverall"][[1]]), 2),
                                     "kWh</b>"),
                       options = popupOptions(
                         closeButton = TRUE, closeOnClick = FALSE)) %>%
    ## Add the Polygon
    addPolygons(data = poly1, group = "Polygon",
                fill = TRUE, fillOpacity = 0.4) %>%
    
    ## Add the Genetic Algorithm Space
    addPolygons(data = GridPol, group = "Grid", weight = 1,
                opacity = opaycity,
                fill = TRUE, fillOpacity = 0) %>%
    
    ## Create Circles in Map
    addCircleMarkers(lng = result$X, lat = result$Y,
                     radius = wake_radius,
                     color = result$farbe,
                     stroke = TRUE, fillOpacity = 0.8,
                     group = "Wake Circles") %>%
    ## Add the turbine symbols
    addMarkers(lng = result[, 1], lat = result[, 2],
               icon = turbine_icon[1], popup = list_popup,
               group = "Turbines") %>%
    addLegend(position = "topleft",
              pal = pal,
              values = result$AbschGesamt,
              labFormat = labelFormat(suffix = "%"),
              opacity = 1, title = "Total Wake Effect",
              layerId = "Legend") %>%
    ## Layers control
    addLayersControl(baseGroups = c(
      "OSM",
      "Terrain",
      "Satellite",
      "Toner"),
      overlayGroups = overlay_group,
      options = layersControlOptions(collapsed = TRUE)
    )
  
  # Plot the map
  map
}