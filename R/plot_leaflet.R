#' @title Plot a Wind Farm with leaflet
#' @name plot_leaflet
#' @description  Plot a resulting wind farm on a leaflet map.
#'
#' @export
#'
#' @param result The resulting matrix of the function \code{\link{genetic_algorithm}}
#' or \code{\link{windfarmGA}}
#' @param Polygon1 The Polygon for the wind farm area.
#' @param which A numeric value, indicating which best individual to plot. The
#'   default is 1 (the best resulting wind farm).
#' @param orderitems A logical value indicating whether the results should be
#'   ordered by energy values (TRUE) or chronologically (FALSE).
#' @param GridPol The output grid polygon of the \code{\link{grid_area}} or
#'   \code{\link{hexa_area}} functions.
#'
#' @return Returns a leaflet map.
#'
#' @examples \donttest{
#' load(file = system.file("extdata/resulthex.rda", package = "windfarmGA"))
#' load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
#'
#' ## Plot the best wind farm on a leaflet map (ordered by energy values)
#' plot_leaflet(result = resulthex, Polygon1 = polygon, which = 1)
#'
#' ## Plot the last wind farm (ordered by chronology).
#' plot_leaflet(result = resulthex, Polygon1 = polygon, orderitems = FALSE,
#'          which = 1)
#'          
#' load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
#' ## Plot the best wind farm on a leaflet map with the rectangular Grid
#' Grid <- grid_area(polygon, resol = 150, prop = 0.4)
#' plot_leaflet(result = resultrect, Polygon1 = polygon, which = 1, 
#'          GridPol = Grid[[2]])
#'
#' ## Plot the last wind farm with hexagonal Grid
#' Grid <- hexa_area(polygon, size = 75)
#' plot_leaflet(result = resulthex, Polygon1 = polygon, which = 1, 
#'          GridPol = Grid[[2]])
#' }
plot_leaflet <- function(result, Polygon1, which = 1, orderitems = TRUE, GridPol){
  poly1 <- isSpatial(shape = Polygon1)

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
    proj_longlat <- CRS(SRS_string = "EPSG:4326")
  } else {
    proj_longlat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }

  if (!missing(GridPol)) {
    if (PROJ6) {
      if (is.na(slot(GridPol, "proj4string"))) {
        slot(GridPol, "proj4string") <- CRS(result[, "inputData"][[1]]["Projection", ][[1]])
      }
    } else {
      if (is.na(sp::proj4string(GridPol))) {
        sp::proj4string(GridPol) <- result[, "inputData"][[1]]["Projection", ][[1]]
      }
    }
    GridPol <- sp::spTransform(GridPol, CRSobj = proj_longlat)
    overlay_group <- c("Wake Circles", "Title", "Polygon", "Turbines", "Grid")
    opaycity <- 0.4
  } else {
    overlay_group <- c("Wake Circles", "Title", "Polygon", "Turbines",  "Grid")
    xses <- rep(poly1@bbox[1], 4); yses <- rep(poly1@bbox[4], 4)
    Sr1 <- sp::SpatialPolygons(list(sp::Polygons(list(
      sp::Polygon(cbind(xses, yses))), ID = "a")), pO = 1:1)
    GridPol <- Sr1

    if (PROJ6) {
      if (!is.na(slot(poly1, "proj4string"))) {
        slot(GridPol, "proj4string") <- slot(poly1, "proj4string")
      } else {
        slot(GridPol, "proj4string") <- CRS(result[, "inputData"][[1]]["Projection", ][[1]])
      }
    } else {
      if (!is.na(sp::proj4string(poly1))) {
        sp::proj4string(GridPol) <- sp::proj4string(poly1)
      } else {
        sp::proj4string(GridPol) <- result[, "inputData"][[1]]["Projection", ][[1]]
      }
    }
    
    GridPol <- sp::spTransform(GridPol, CRSobj = proj_longlat)
    opaycity <- 0
  }

  if (PROJ6) {
    if (is.na(slot(poly1, "proj4string"))) {
      slot(poly1, "proj4string") <- CRS(result[, "inputData"][[1]]["Projection", ][[1]])
    }
    proj_pol <- slot(poly1, "proj4string")
  } else {
    if (is.na(sp::proj4string(poly1))) {
      sp::proj4string(poly1) <- result[, "inputData"][[1]]["Projection", ][[1]]
    }
    proj_pol <- CRS(sp::proj4string(poly1))
  }
  result <- result[, "bestPaEn"][[which]]
  xysp <- sp::SpatialPoints(cbind(result[, "X"],
                                  result[, "Y"]), proj4string = proj_pol)
  resultxy <- sp::spTransform(xysp, CRSobj = proj_longlat)
  resultxy <- sp::coordinates(resultxy)

  poly1 <- sp::spTransform(poly1, CRSobj = proj_longlat)

  title_locat <- c(mean(raster::extent(poly1)[1:2]),
                   max(raster::extent(poly1)[4]))

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
                fill = TRUE, fillOpacity = opaycity) %>%

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