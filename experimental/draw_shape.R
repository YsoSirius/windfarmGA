## Draw a site polygon in Leaflet (GitHub only, not on CRAN).
## source("experimental/draw_shape.R")
##
## mapedit 0.8 dropped leaflet.extras (off CRAN) and only supports
## editor = "leafpm". Older mapedit still calls dplyr::select_(),
## which current dplyr treats as an error.

draw_shape <- function(crs = 3035) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("install.packages('leaflet')")
  }
  if (!requireNamespace("mapedit", quietly = TRUE)) {
    stop("install.packages(c('mapedit', 'leafpm'))")
  }
  if (!requireNamespace("leafpm", quietly = TRUE)) {
    stop(
      "mapedit >= 0.8 draws with leafpm (leaflet.extras is off CRAN).\n",
      "Install with install.packages('leafpm')"
    )
  }

  map <- leaflet::leaflet() |>
    leaflet::addTiles(group = "OSM") |>
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    leaflet::setView(12, 49, 4) |>
    leaflet::addLayersControl(
      baseGroups = c("OSM", "Satellite"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )

  drawn <- tryCatch(
    mapedit::editMap(map, editor = "leafpm"),
    error = function(e) {
      stop(
        "mapedit::editMap() failed. Use current mapedit + leafpm:\n",
        "  install.packages(c('mapedit', 'leafpm'))\n",
        "Older mapedit breaks on dplyr (select_() is gone).\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  finished <- drawn$finished
  if (is.null(finished) && inherits(drawn, "sf")) {
    finished <- drawn
  }
  if (is.null(finished) || !nrow(finished)) {
    return(NULL)
  }
  polygon <- sf::st_geometry(finished)
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
