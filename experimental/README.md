# Experimental (GitHub only)

Scripts here are **not** in the CRAN tarball (see `.Rbuildignore`).
They are not loaded with the package. Open them with `source()` after
installing any extra packages they need.

| File | What | Extra packages |
|---|---|---|
| `run_experiments.R` | Source this: `run_experiments()`, `viewshed_from_result()` | elevatr, terra, sf |
| `draw_shape.R` | Draw a site polygon in Leaflet, then run the GA. Needs mapedit >= 0.8 and leafpm (`leaflet.extras` is off CRAN; old mapedit still calls `dplyr::select_()`). | leaflet, mapedit (>= 0.8), leafpm |
| `circle_overlap_app.R` | Shiny slider for `circle_intersection()` | shiny, ggplot2, ggforce |
| `climate_helpers.R` | `wind_from_breeze()`, `wind_from_era5()`, `nrel_fetch_curve()`, `gwa_download_country()` | bReeze / ecmwfr optional; internet for NREL and GWA |
| `download_ERA5_historic.R` | `get_era5_wind(polygon)` — CDS 100 m u/v at the nearest grid point. Then `wind_from_era5(era5)` | ecmwfr, ncdf4, dplyr, lubridate; env `COPERNICUS_CLIMATE_DATA` |
| `profile_energy.R` | `profile_calculate_energy()` / `print_energy_profile()` | none beyond the package |
| `noise.R` | ISO 9613-2 sketch (`noise_map`, `noise_from_result`). Downwind Adiv+Aatm+Agr plus optional upwind extra from the wind rose. Not a legal study, not in fitness. | terra, sf |
| `rayshader.R` | `plot_farm_3d` / `plot_farm_3d_from_result` — buffered DEM, wind arrow, wake cones, pins colored by `AbschGesamt`. Heavy install. | rayshader, rgl, elevatr |

**Already in the package (do not duplicate here)**

- Viewsheds: `plot_viewshed()`
- Result explorer: `explore_result()`
- Power curve: `ga_options(power_curve = data.frame(ws, power))` or
  `read_power_curve()` / `wind_from_uv()` in the package

**Stay in local `_experiment/` (not on GitHub)**

- `test_climate_helpers.R` — walkthrough of the climate helpers (`source("_experiment/test_climate_helpers.R")`).
- `gwa/` — Global Wind Atlas country GeoTIFFs from `gwa_download_country()`.
- Old scratch copies of `noise.R` / `rayshader.R` (the rewritten ones are in `experimental/`).

Do not put these dependencies on the search loop.
