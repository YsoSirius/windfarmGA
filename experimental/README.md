# Experimental (GitHub only)

Scripts here are **not** in the CRAN tarball (see `.Rbuildignore`).
They are not loaded with the package. Open them with `source()` after
installing any extra packages they need.

| File | What | Extra packages |
|---|---|---|
| `draw_shape.R` | Draw a site polygon in Leaflet, then run the GA. Needs mapedit >= 0.8 and leafpm (`leaflet.extras` is off CRAN; old mapedit still calls `dplyr::select_()`). | leaflet, mapedit (>= 0.8), leafpm |
| `circle_overlap_app.R` | Shiny slider for `circle_intersection()` | shiny, ggplot2, ggforce |
| `climate_helpers.R` | `wind_from_breeze()`, `wind_from_era5()`, `nrel_fetch_curve()`, `gwa_download_country()` | bReeze / ecmwfr optional; internet for NREL and GWA |
| `download_ERA5_historic.R` | `get_era5_wind(polygon)` — CDS 100 m u/v at the nearest grid point. Then `wind_from_era5(era5)` | ecmwfr, ncdf4, dplyr, lubridate; env `COPERNICUS_CLIMATE_DATA` |
| `profile_energy.R` | `profile_calculate_energy()` / `print_energy_profile()` | none beyond the package |
| `noise.R` | ISO 9613-2 sketch (`noise_map`, `noise_from_result`). Downwind Adiv+Aatm+Agr plus optional upwind extra from the wind rose. Not a legal study, not in fitness. | terra, sf |
| `rayshader.R` | `plot_farm_3d` / `plot_farm_3d_from_result` — buffered DEM, wind arrow, wake cones, pins colored by `AbschGesamt`. Heavy install. | rayshader, rgl, elevatr |
| `plot_mapgl.R` | `plot_mapgl_from_result()` — 3D MapLibre DEM, wake cones colored by wake (0% green), OBJ turbines (`wind_turbine_v1.obj`). Basemaps: Carto, Esri satellite, OpenTopoMap. Reuses `terrain_tiles/` unless `rebuild = TRUE`. | mapgl, terra, sf, elevatr, png, jsonlite, httpuv, htmlwidgets; Three.js r149 in `terrain_tiles/`; RSQLite for `.mbtiles` |

