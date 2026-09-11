## Draw a site polygon in Leaflet (GitHub only, not on CRAN).
## source("experimental/draw_shape.R")

draw_shape <- function(crs = 3035) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("install.packages('leaflet')")
  }
  if (!requireNamespace("mapedit", quietly = TRUE)) {
    stop("install.packages('mapedit')")
  }

  map <- leaflet::leaflet() |>
    leaflet::addTiles(group = "OSM") |>
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    leaflet::setView(12, 49, 4) |>
    leaflet::addLayersControl(
      baseGroups = c("OSM", "Satellite"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
  if (requireNamespace("leaflet.extras", quietly = TRUE)) {
    map <- leaflet.extras::addSearchOSM(map)
  }

  drawn <- mapedit::editMap(map)
  if (is.null(drawn$finished)) {
    return(NULL)
  }
  polygon <- sf::st_geometry(drawn$finished)
  if (!is.null(crs)) {
    polygon <- sf::st_transform(polygon, crs)
  }
  sf::st_as_sf(sf::st_union(polygon))
}

# polygon <- draw_shape()
# result <- genetic_algorithm(
#   area = polygon, n = 20, wind = data.frame(ws = 12, wd = 0),
#   rotor = 30, rotor_height = 100, iteration = 40
# )
# plot(result, polygon)
