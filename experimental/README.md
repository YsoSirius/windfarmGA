# Experimental (GitHub only)

Scripts here are **not** in the CRAN tarball (see `.Rbuildignore`).
They are not loaded with the package. Open them with `source()` after
installing any extra packages they need.

| File | What | Extra packages |
|---|---|---|
| `run_experiments.R` | Source this: `run_experiments()`, `viewshed_from_result()` | elevatr, terra, sf |
| `draw_shape.R` | Draw a site polygon in Leaflet, then run the GA | leaflet, mapedit, leaflet.extras |
| `circle_overlap_app.R` | Shiny slider for `circle_intersection()` | shiny, ggplot2, ggforce |

**Already in the package (do not duplicate here)**

- Viewsheds: `plot_viewshed()`
- Result explorer: `explore_result()`
- Power curve: `ga_options(power_curve = data.frame(ws, power))`

**Stay in local `_experiment/` (not on GitHub)**

- `rayshader.R` / `plot_farm_3d` — heavy install (rayshader, rgl). Do not add to Suggests.
- `noise.R` — ISO 9613 sketch on `raster`, not wired into fitness.

Do not put these dependencies on the search loop.
