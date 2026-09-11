# Get terrainhic rasters

Calculate the SpatRasters needed for the terrain model.

## Usage

``` r
terrain_model(
  terrain = TRUE,
  area,
  ccl,
  ccl_roughness,
  plot = FALSE,
  verbose = FALSE
)
```

## Arguments

- terrain:

  Terrain model (elevation + land cover).

- area:

  Site polygon (`sf`, SpatialPolygons, or coordinate matrix). Must be
  projected in metres.

- ccl:

  Path to a Corine Land Cover raster when `terrain` is on.

- ccl_roughness:

  Path to the CLC legend CSV (`Rauhigkeit_z` column).

- plot:

  Plot the elevation and roughness rasters

- verbose:

  Print a line per generation.

## Value

A list of SpatRasters

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4651704, 4651704, 4654475, 4654475, 4651704),
    c(2692925, 2694746, 2694746, 2692925, 2692925)
  ))),
  crs = 3035
))
Polygon_wgs84 <- sf::st_transform(area, st_crs(4326))
srtm <- elevatr::get_elev_raster(locations = Polygon_wgs84, z = 11)
res <- terrain_model(srtm, area)
} # }
```
