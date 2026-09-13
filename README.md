# windfarmGA

<img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/windfarmGA.png" align="right" width="150"/>



<!-- badges: start -->
[![R build status](https://github.com/YsoSirius/windfarmGA/workflows/R-CMD-check/badge.svg)](https://github.com/YsoSirius/windfarmGA/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/windfarmGA)](https://CRAN.R-project.org/package=windfarmGA)
[![CRAN checks](https://badges.cranchecks.info/summary/windfarmGA.svg)](https://cran.r-project.org/web/checks/check_results_windfarmGA.html)
[![](https://cranlogs.r-pkg.org/badges/grand-total/windfarmGA)](https://cran.r-project.org/package=windfarmGA)
[![](https://cranlogs.r-pkg.org/badges/last-month/windfarmGA?color=blue)](https://cran.r-project.org/package=windfarmGA)
[![codecov](https://codecov.io/gh/YsoSirius/windfarmGA/branch/master/graph/badge.svg)](https://app.codecov.io/gh/YsoSirius/windfarmGA)

<!-- badges: end -->


A genetic algorithm to optimize the layout of wind farms.

Version 5.0.0 uses a combinatorial genome (`n` unique grid-cell IDs), adaptive operator rates, and a memetic neighbourhood search on elites.
The design, the north-wind gold layout, and the sensitivity results are documented in
[inst/reports/memetic-layout-ga.md](inst/reports/memetic-layout-ga.md).


# Installation
The latest version can be installed from GitHub with:
```R
devtools::install_github("YsoSirius/windfarmGA")
install.packages("windfarmGA")
```

# Description
The genetic algorithm is designed to optimize wind farms of any shape.
It requires a predefined number of turbines, a uniform rotor radius and an average wind speed per wind direction.
It can include a terrain effect model, which requires an elevation raster and a surface roughness raster. 
The elevation data is used to find mountains and valleys and to adjust the 
wind speeds accordingly by 'wind multipliers' and to determine the air densities at rotor heights. 
The surface roughness raster with an additional elevation roughness value is used to re-evaluate the surface roughness 
and to individually determine the wake-decay constant for each turbine.

To start an optimization use the function `genetic_algorithm`.
A complete path — draw a site, pick an open IEA/NREL turbine, turn u/v or mast data into a rose, then optimize and plot — is in
[Realistic workflow](#realistic-workflow-site-turbine-wind). 

<div>
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/result2.png" style="width: 49%;display: inline-block;"/>
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/result1.png"  style="width: 49%;display: inline-block;"/>
</div>
<div>
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/result3.png" style="width: 98.5%;display: inline-block;"/>
</div>

Since version 1.1, hexagonal grid cells are possible, with their center points being possible locations for wind turbines. 
Furthermore, rasters can be included, which contain information on the Weibull parameters (shape `k`, scale `a`). 
Country GeoTIFFs from the [Global Wind Atlas](https://globalwindatlas.info/) (~250 m) are a good
source; see `gwa_download_country()` in `experimental/climate_helpers.R`. 
    
## Create an input Polygon
- Input Polygon by source
```R
library(sf)
dsn <- "Path to the Shapefile"
layer <- "Name of the Shapefile"
area <- sf::st_read(dsn = dsn, layer = layer)
plot(area, col = "blue")
```

- Or create a random Polygon
```R
library(sf)
area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(0, 0, 2000, 2000, 0),
    c(0, 2000, 2000, 0, 0)))),
  crs = 3035
))
plot(area, col = "blue", axes = TRUE)
```

- Or draw a site on a map (GitHub clone; needs `leaflet`, `mapedit` >= 0.8, `leafpm`)
```R
source("experimental/draw_shape.R")
area <- draw_shape()   # draw a polygon, then close the viewer
plot(area, axes = TRUE)
```

## Create random Wind data 
- Exemplary input Wind data with *uniform* wind speed and *single* wind direction
```R
wind_df <- data.frame(ws = c(12, 12), wd = c(0, 0), probab = c(25, 25))
windrosePlot <- plot_windrose(data = wind_df, spd = wind_df$ws,
                              dir = wind_df$wd, dirres=10, spdmax = 20)
```

- Exemplary input Wind data with *random* wind speeds and *random* wind directions
```R
wind_df <- data.frame(ws = sample(1:25, 10), wd = sample(1:260, 10))
windrosePlot <- plot_windrose(data = wind_df, spd = wind_df$ws,
                              dir = wind_df$wd)
```

## Grid Spacing
### Rectangular Grid Cells
Verify that the grid spacing is appropriate. Adapt the following input variables if necessary:
- *rotor*: The rotor radius in meters.
- *fcr*: The grid spacing factor, which should at least be 2, so that a single grid covers at least the whole rotor diameter.
- *prop*: The proportionality factor used for grid calculation. It determines the minimum percentage that a grid cell must cover of the area.

*Make sure that the Polygon is projected in meters.*
```R
Rotor <- 20
fcr <- 9
Grid <- grid_area(area, size = (Rotor * fcr), prop = 1, plot_grid = TRUE)
str(Grid)
```
### Hexagonal Grid Cells
```R
Rotor <- 20
fcr <- 9
HexGrid <- hexa_area(area, size = (Rotor * fcr), plot_grid = TRUE)
str(HexGrid)
```
<p align="center">
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/grids.png" width="600"/>
</p>


## Terrain Effect Model
If **terrain** is `TRUE` in `genetic_algorithm`, terrain effects are taken into account. 
A digital elevation model and a CORINE Land Cover raster are downloaded automatically. 
Prefer `terrain = dem` (a SpatRaster) to skip the download. 
Per-cell height, wind multiplier, \(z_0\), \(k\) and air density are computed once; `plot_result` / `random_search` reuse the
stored `terrainModel` instead of downloading again.
([Download a CLC raster](https://www.eea.europa.eu/data-and-maps/data/clc-2006-raster-4)).


To use your own land cover raster, provide its path via **ccl**.
Surface roughness values are taken from **extdata/clc_legend.csv**. To use custom values, add a numeric **Rauhigkeit_z** column for all land cover classes, save the file with ; as delimiter, and provide its path via **ccl_roughness**.

`plot_result(..., terrain = TRUE)` shows the full chain:

- Elevation → wind-speed multiplier (DEM / mean DEM)
- Elevation → air density (barometric; labels vs 1.225 kg/m³)
- CORINE Land Cover → land-cover roughness \(z_0\)
- Elevation roughness (`terra::terrain`) × CLC \(z_0\) → modified \(z_0\)
- Modified \(z_0\) + hub height → wake decay \(k = 0.5 / \log(h/z_0)\)

The modified \(z_0\) also scales hub-height wind via the log profile
(not a separate `plot_result` panel).

<p align="center">
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/terrain_effect.png"/>
</p>   


## Realistic workflow (site, turbine, wind)

Clone the GitHub repo so `experimental/` is available (it is not in the CRAN tarball). 
Browse open reference turbines at the [NREL Turbine Archive](https://natlabrockies.github.io/turbine-models/).

ERA5 / Copernicus (env `COPERNICUS_CLIMATE_DATA`) is useful as a **directional rose** over time (`get_era5_wind()` → `wind_from_era5()`),
but the grid is too coarse (~31 km) as a spatial wind field for siting.
For mean speed per cell use Global Wind Atlas Weibull rasters (`weibull = TRUE`, `weibull_src = list(k, a)`). 
Hub-height correction uses `reference_height` (100 m for the ERA5 100 m u/v download) and `rotor_height` from the turbine. 
A met mast goes through `wind_from_breeze()` or `wind_from_series()`.

```R
library(windfarmGA)
library(sf)

source("experimental/draw_shape.R")
source("experimental/climate_helpers.R")
source("experimental/download_ERA5_historic.R")

## 1. Site: draw a polygon (or st_read a shapefile)
area <- draw_shape()
plot(area, axes = TRUE)

## 2. Turbine: IEA 3.4 MW, 130 m rotor, 110 m hub
nrel_curve_catalog()
curve <- nrel_fetch_curve("IEA_3.4MW_130")
plot_power_curve(curve)
ga_options(power_curve = curve)
rotor <- attr(curve, "rotor")           # radius in metres
hub <- attr(curve, "rotor_height")

## 3. Wind rose from ERA5 100 m u/v at the site (needs CDS key)
era5 <- get_era5_wind(area, "2021-01-01", "2025-12-31")
wind <- wind_from_era5(era5)
plot_windrose(wind, spd = "ws", dir = "wd")

##    Met mast instead of ERA5 (bReeze, or two numeric vectors):
##    library(bReeze)
##    data(winddata)
##    s40 <- set(height = 40, v.avg = winddata[, 2], dir.avg = winddata[, 14])
##    mast <- mast(timestamp(winddata[, 1]), s40)   # bReeze::mast
##    wind <- wind_from_breeze(mast)
##    wind <- wind_from_series(ws, wd)              # raw speed / direction

##    Spatial mean speed (GWA ~250 m); wind$ws is then ignored
gwa <- gwa_download_country("AUT", height = 100)  # ISO3 of the site country

## 4. Optimize
result <- genetic_algorithm(
  area = area,
  wind = wind,
  n = 12,
  rotor = rotor,
  rotor_height = hub,
  reference_height = 100,
  weibull = TRUE,
  weibull_src = gwa$weibull_src,
  terrain = TRUE,                   # DEM + CLC; needs elevatr
  fcr = 5,
  iteration = 40,
  plot = FALSE
)

## 5. Plot
print(result)
plot(result, area)
plot_result(result, area, terrain = TRUE)
plot_parkfitness(result)
plot_leaflet(result, area, which = 1)
explore_result(result, area)

source("experimental/rayshader.R")
plot_farm_3d_from_result(result, area, buffer = 5000, turbine_obj = "./experimental/wind_turbine_v1.obj")
source("experimental/plot_mapgl.R")
plot_mapgl_from_result(result, area, buffer = 8000, basemap = "satellite")


## 6. Optional: jitter turbines inside their cells (same physics as the GA)
##    Terrain is reused from result$terrainModel. Weibull rasters are not
##    stored — pass weibull_src again (that is enough, no weibull = TRUE).
refined <- random_search(
  result, area, runs = 20, best = 1, plot = FALSE,
  weibull_src = gwa$weibull_src
)
plot_random_search(refined, result, area, best = 1)
```

<p align="center">
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/realistic_example.png"/>
</p>

The screenshot above shows the optimized layout for a north-only wind rose. The best layout often sits in the first few upwind rows.
In 2D, the arrangement looks cramped; however, wakes are in 3D. A turbine on lower ground can sit below the hub-height wake cone of a turbine further uphill and therefore is not counted as shadowed. These front rows are also at higher elevation, so the wind multiplier remains high while air density decreases less.
The Leaflet map and the rayshader view (bottom right) show the same run in 2D and on the DEM.




## Start an Optimization
An optimization can be initiated with the function **genetic_algorithm**.
Search knobs that are not function arguments are session options
(`options(windfarmGA.*)`); see [Options](#options).

- without terrain effects
```R
result <- genetic_algorithm(
  area = area, n = 12, rotor = 20, fcr = 9, iteration = 10,
  wind = wind_df, rotor_height = 100
)
```

- with terrain effects
```R
ccl <- "Source of the CCL raster (TIF)"
ccl_roughness <- "Source of the Adapted CCL legend (CSV)"

result <- genetic_algorithm(
  area = area, n = 12, rotor = 20, fcr = 9, iteration = 10,
  wind = wind_df, rotor_height = 100, terrain = TRUE,
  ccl = ccl, ccl_roughness = ccl_roughness
)
```


```R
## Spatial mean speed from Global Wind Atlas (shape k, scale A).
## wind$ws is then ignored; wind$wd / wind$probab still weight directions.
source("experimental/climate_helpers.R")
gwa <- gwa_download_country("AUT", height = 100)
result_weibull <- genetic_algorithm(
  area = area, grid_method = "h", n = 12,
  fcr = 5, iteration = 10, wind = wind_df, rotor = 30,
  rotor_height = 100, weibull = TRUE, weibull_src = gwa$weibull_src
)
plot_windfarmGA(result = result_weibull, area = area)
```

# Options

There are two layers. Arguments of `genetic_algorithm()` describe the site, the turbines and which GA pieces to use. Session options
`options(windfarmGA.*)` control physics constants and the memetic search (inject, immigrants, seasons, neighbour local search). Set them **before**
the run; they apply for the whole R session until you change them again.

```R
ga_options()
ga_options(immigrants = 3, local_search_elites = 5, local_search_tries = 6)
```

The north-wind benchmark and why these search defaults exist are in
[inst/reports/memetic-layout-ga.md](inst/reports/memetic-layout-ga.md).

## Arguments of `genetic_algorithm()`

Required: `area`, `wind`, `n`, `rotor`, `rotor_height`.

### Site and turbines

| Argument | Default | Meaning |
|---|---|---|
| `area` | — | Area as sf polygon, SpatialPolygons, or coordinate matrix. Must be projected (metres). |
| `n` | — | Number of turbines (fixed; every individual has exactly `n` unique cell IDs). |
| `rotor` | — | Rotor radius in metres. |
| `fcr` | `5` | Grid spacing factor. Cell size is `fcr * rotor`. Use at least `2` so a cell covers the rotor diameter. |
| `grid_method` | `"rectangular"` | `"h"` / `"hexagon"` for hexagonal cells. |
| `proportionality` | `1` | Minimum fraction of a grid cell that must overlap the polygon (`prop` in `grid_area`). |
| `crs` | EPSG:3035 | CRS if the polygon has none (numeric EPSG or PROJ string). |
| `wind` | — | Wind data.frame (`ws`, `wd`, optional `probab`). See `windata_format`. |
| `reference_height` | `rotor_height` | Height at which `ws` was measured. |
| `rotor_height` | — | Hub height in metres. |
| `surface_roughness` | `0.3` | Roughness length \(z_0\) in metres. Ignored per cell when `terrain = TRUE`. |

### Terrain and Weibull

| Argument | Default | Meaning |
|---|---|---|
| `terrain` | `FALSE` | Terrain model: elevation (via `elevatr`) plus Corine Land Cover for roughness and wake decay. |
| `ccl` | auto / package | Path to a CLC raster (`.tif`) if you do not want the download. |
| `ccl_roughness` | `inst/extdata` | CSV legend with column `Rauhigkeit_z` (`;` separated). |
| `weibull` | `FALSE` | If `TRUE`, mean speed at each turbine comes from Weibull rasters; `ws` in `wind` is ignored. |
| `weibull_src` | `NULL` | `list(k, a)` shape and scale rasters (e.g. GWA `combined-Weibull-k` / `combined-Weibull-A`). |

### GA loop

The loop is **selection → set-crossover → swap-mutation → fitness**.

| Argument | Default | Meaning |
|---|---|---|
| `iteration` | `20` | Generation budget. Raise this for real searches (the gold-layout tests used 80–150). |
| `mutation_rate` | `2/n` | Mutation probability **per turbine** (swap with an unused cell). Adaptive rates still keep this as the mutate target. |
| `selection_mode` | `"VAR"` | `"VAR"`: parent share follows fitness progress. `"FIX"`: always 50 %. |
| `elitism` | `TRUE` | Archive the best layout unchanged; elites also spawn children and get local search. |
| `n_elite` | `3` | Base elite count. Grows in a long refine stall, drops by 1 during a disturbance pulse. |
| `parallel` | `FALSE` | Parallel fitness (`parallel` + `doParallel`). |
| `n_cluster` | `2` | Worker count if `parallel` is `TRUE` (capped at cores − 1). |
| `verbose` | `FALSE` | Print a line per generation. |
| `plot` | `FALSE` | Plot the current best layout each generation. |

## Session options (`options(windfarmGA.*)`)

Set with `options(windfarmGA.foo = …)`. Defaults are assigned in `.onLoad`.

### Physics (wake and power)

Fitness is \(E \times (\eta/100)^w\). These options change how \(E\) and \(\eta\) are computed.

| Option | Default | Meaning |
|---|---|---|
| `windfarmGA.Cp` | `0.45` | Power coefficient. |
| `windfarmGA.cT` | `0.88` | Thrust coefficient (wake). |
| `windfarmGA.k` | `0.075` | Wake decay constant (without terrain). |
| `windfarmGA.air_rh` | `1.225` | Air density (kg/m³). |
| `windfarmGA.wind_profile` | `"log"` | Hub-height profile. `"power"` restores the old power law. |
| `windfarmGA.cut_in` | `0` | Cut-in wind speed (m/s). `0` = no cut-in. |
| `windfarmGA.rated_ws` | `Inf` | Rated wind speed; above this, power stays at rated. |
| `windfarmGA.cut_out` | `Inf` | Cut-out wind speed. |
| `windfarmGA.power_curve` | `NULL` | Optional `data.frame(ws, power)` in kW. Park energy is the sum of interpolated turbine kW (not Cp * v^3). On the rated plateau, wakes may not change power. Build a table with `read_power_curve()` or `plot_power_curve()`. ERA5 u/v: `wind_from_uv()`. |
| `windfarmGA.fitness_efficiency_weight` | `1` | Preference weight \(w\): fitness is \(E \times (\eta/100)^w\). `0` = energy only; `>1` punishes wake more. Not auto-tuned. |
| `windfarmGA.max_angle` | `20` | Max wake angle (degrees) when assigning downstream turbines. |
| `windfarmGA.max_distance` | `100000` | Max wake distance (m). |

### Population caps

| Option | Default | Meaning |
|---|---|---|
| `windfarmGA.max_population` | `300` | Upper bound on individuals after crossover. |
| `windfarmGA.max_selection` | `300` | Upper bound on selected parents. |

### Crossover, mutation, immigrants

| Option | Default | Meaning |
|---|---|---|
| `windfarmGA.crossover_inject` | `0.25` | Share of child cells taken from **outside** both parents, so the search is not stuck in the parental union. Starting value; adaptive seasons move it. |
| `windfarmGA.spatial_crossover` | `0.5` | Probability that crossover splits the park with a half-plane instead of a random set mix. |
| `windfarmGA.min_swaps` | `1` | Minimum cell swaps per mutated individual. |
| `windfarmGA.immigrants` | `3` | Random new layouts each generation (rarely visited cells preferred). |

On the north-wind test, extra immigrants helped the coarse search but lost to stronger neighbour local search for the last percent.

### Elitism and neighbour local search

| Option | Default | Meaning |
|---|---|---|
| `windfarmGA.elite_children` | `3` | Mutated copies of each elite (inject 0, structure kept). Extra copies if the max has been flat for 10 generations. |
| `windfarmGA.elite_mix` | `2` | Crossovers of an elite with a weaker layout (inject 0). |
| `windfarmGA.local_search_elites` | `5` | How many elites get a neighbour slide each generation. `0` turns local search off. |
| `windfarmGA.local_search_tries` | `6` | Hill-climb tries per elite: move **one** turbine to a free rook/hex neighbour. Only keep if fitness rises. |

This is the memetic step. A random swap across the grid rarely fixes “one row too far inland”; a neighbour slide often does.

### Adaptive rates (explore / refine / pulse)

Same three operators; only the rates change. **Explore** raises inject and mutation while the record is stalling (VAR selection starts near 55 %). **Refine** damps them toward inject 0.15, mutation `2/n`, selection ~45 %. **Pulse** briefly raises them again so refine does not freeze.

| Option | Default | Meaning |
|---|---|---|
| `windfarmGA.refine_min_gen` | `18` | Earliest generation that may enter refine. |
| `windfarmGA.refine_after` | `12` | Stall generations (no new max) plus coverage ≥ 0.35 before refine. |
| `windfarmGA.refine_hold` | `25` | Generations spent in refine before a pulse. |
| `windfarmGA.explore_pulse` | `10` | Length of the disturbance pulse. |
| `windfarmGA.stall_generations` | `40` | Stop early only if this many generations in a row produce **no** new layout, **no** new cell and **no** new best. `0` disables early stop. A flat maximum while new sites are still tried is *not* a stop. |

### Other

| Option | Default | Meaning |
|---|---|---|
| `windfarmGA.connection` | `stdin()` | Where interactive prompts read from (tests redirect this). |

# Plotting

A result from `genetic_algorithm()` has class `windfarmGA`:
`print(result)` summarises the run, `plot(result, area)` is `plot_windfarmGA()`, and `explore_result(result, area)` opens a
one-page Shiny viewer with a Leaflet map of the best layout and a plotly figure (fitness, rates, population; click a New max marker to
jump to that generation). (Suggests: shiny, leaflet, plotly).

`plot_windfarmGA()` pages through the best layout, fitness and operator rates, the population census, and the cell heatmap. 
In an interactive session each page waits for Enter. Plotly hover is used only with `ask = FALSE` when the plotly package is installed.

```R
print(result)
plot(result, area)
explore_result(result, area)
plot_windfarmGA(result, area)

plot_result(result, area)                 # best layout
plot_parkfitness(result)                      # fitness + operator rates
plot_population(result)                       # individuals, elites, cells
plot_generation(result, area)             # last generation (omit generation, or pass an index)
plot_cell_heatmap(result, area)           # which cells were tried
plot_leaflet(result, area, which = 1)     # best on a map
plot_leaflet(result, area, orderitems = FALSE, which = 1)  # last generation
plot_evolution(result)                        # energy + efficiency
plot_development(result)                      # when the max improved
```

Package data `resulthex` / `resultrect` and `sp_polygon` work the same way (`plot_leaflet(resulthex, sp_polygon, which = 1)`).

A full documentation of the genetic algorithm is given in my [master thesis](https://homepage.boku.ac.at/jschmidt/TOOLS/Masterarbeit_Gatscha.pdf).
> **NOTE:** The implementation has changed substantially since then. Many functions and arguments have been renamed or optimized, and the internal structure of the algorithm has been reworked. As a result, the current version differs significantly from the implementation described in the thesis and calculates and converges considerably faster.

# Shiny Windfarm Optimization
I also made a [Shiny App](https://windfarmga.shinyapps.io/windga_shiny/) for the Genetic Algorithm. 
Unfortunately, as an optimization takes quite some time and the app is currently hosted by shinyapps.io under a public license, there is only 1 R-worker at hand. So only 1 optimization can be run at a time. 
