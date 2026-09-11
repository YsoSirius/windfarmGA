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

Version 5.0.0 uses a combinatorial genome (`n` unique grid-cell IDs),
adaptive operator rates, and a memetic neighbourhood search on elites.
The design, the north-wind gold layout, and the sensitivity results are
documented in
[inst/reports/memetic-layout-ga.md](inst/reports/memetic-layout-ga.md).


# Installation
The latest version can be installed from GitHub with:
```R
devtools::install_github("YsoSirius/windfarmGA")
install.packages("windfarmGA")
```

# Description
The genetic algorithm is designed to optimize wind farms of any shape.
It requires a predefined number of turbines, a uniform rotor radius and 
an average wind speed per wind direction.
It can include a terrain effect model, which requires an 
elevation raster and a surface roughness raster. The elevation 
data is used to find mountains and valleys and to adjust the 
wind speeds accordingly by 'wind multipliers' and to determine 
the air densities at rotor heights. The surface roughness raster with an additional elevation
roughness value is used to re-evaluate the surface roughness and to individually
determine the wake-decay constant for each turbine.

To start an optimization use the function `genetic_algorithm`. 

<div>
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/result2.png" style="width: 49%;display: inline-block;"/>
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/result1.png"  style="width: 49%;display: inline-block;"/>
</div>

Since version 1.1, hexagonal grid cells are possible, with 
their center points being possible locations for wind turbines. 
Furthermore, rasters can be included, which contain information on the Weibull 
parameters. For Austria this data is already included in the package. 
    
## Create an input Polygon
- Input Polygon by source
```R
library(sf)
dsn <- "Path to the Shapefile"
layer <- "Name of the Shapefile"
Polygon1 <- sf::st_read(dsn = dsn, layer = layer)
plot(Polygon1, col = "blue")
```

- Or create a random Polygon
```R
library(sf)
Polygon1 <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(0, 0, 2000, 2000, 0),
    c(0, 2000, 2000, 0, 0)))),
  crs = 3035
))
plot(Polygon1, col = "blue", axes = TRUE)
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
- *Rotor*: The rotor radius in meters.
- *fcrR*: The grid spacing factor, which should at least be 2, so that a single grid covers at least the whole rotor diameter.
- *prop*: The proportionality factor used for grid calculation. It determines the minimum percentage that a grid cell must cover of the area.

*Make sure that the Polygon is projected in meters.*
```R
Rotor <- 20
fcrR <- 9
Grid <- grid_area(Polygon1, size = (Rotor * fcrR), prop = 1, plotGrid = TRUE)
str(Grid)
```
### Hexagonal Grid Cells
```R
Rotor <- 20
fcrR <- 9
HexGrid <- hexa_area(Polygon1, size = (Rotor * fcrR), plotGrid = TRUE)
str(HexGrid)
```
<p align="center">
  <img src="https://raw.githubusercontent.com/YSoSirius/windfarmGA/master/inst/img/grids.png" width="300"/>
</p>


## Terrain Effect Model
If the input variable **topograp** for the functions `windfarmGA` or `genetic_algorithm` is TRUE, the genetic algorithm will take terrain effects into account. For this purpose an elevation model and a Corine Land Cover raster are downloaded automatically, but can also be given manually. ( [Download a CLC raster](https://www.eea.europa.eu/data-and-maps/data/clc-2006-raster-4) ).
            

If you want to include your own Land Cover Raster, you must assign the Raster Image path to the input variable **sourceCCL**. The algorithm uses an adapted version of the Raster legend ("clc_legend.csv"), which is stored in the package subdirectory (/extdata). To use own values for the land cover roughness lengths, insert a column named **Rauhigkeit_z** to the .csv file. Assign a surface roughness length to all land cover types. 
Be sure that all rows are filled with numeric values and save the .csv file with ";" delimiter. Assign the .csv file path to the input variable **sourceCCLRoughness**.


## Start an Optimization
An optimization can be initiated with the function **genetic_algorithm**.
Search knobs that are not function arguments are session options
(`options(windfarmGA.*)`); see [Options](#options).

- without terrain effects
```R
result <- genetic_algorithm(
  Polygon1 = Polygon1, n = 12, Rotor = 20, fcrR = 9, iteration = 10,
  vdirspe = wind_df, RotorHeight = 100
)
```

- with terrain effects
```R
sourceCCL <- "Source of the CCL raster (TIF)"
sourceCCLRoughness <- "Source of the Adapted CCL legend (CSV)"

result <- genetic_algorithm(
  Polygon1 = Polygon1, n = 12, Rotor = 20, fcrR = 9, iteration = 10,
  vdirspe = wind_df, RotorHeight = 100, topograp = TRUE,
  sourceCCL = sourceCCL, sourceCCLRoughness = sourceCCLRoughness
)
```


```R
## Run an optimization with your own Weibull parameter rasters. The shape and scale
## parameter rasters of the weibull distributions must be added to a list, with the first
## list item being the shape parameter (k) and the second list item being the scale
## parameter (a). Adapt the paths to your raster data and run an optimization.
kraster <- "/..pathto../k_param_raster.tif"
araster <- "/..pathto../a_param_raster.tif"
weibullrasters <- list(terra::rast(kraster), terra::rast(araster))

result_weibull <- genetic_algorithm(
  Polygon1 = Polygon1, GridMethod = "h", n = 12,
  fcrR = 5, iteration = 10, vdirspe = wind_df, Rotor = 30,
  RotorHeight = 100, weibull = TRUE, weibullsrc = weibullrasters
)
plot_windfarmGA(result = result_weibull, Polygon1 = Polygon1)
```

# Options

There are two layers. Arguments of `genetic_algorithm()` describe the site,
the turbines and which GA pieces to use. Session options
`options(windfarmGA.*)` control physics constants and the memetic search
(inject, immigrants, seasons, neighbour local search). Set them **before**
the run; they apply for the whole R session until you change them again.

```R
options(
  windfarmGA.immigrants = 3,
  windfarmGA.local_search_elites = 5,
  windfarmGA.local_search_tries = 6
)
```

The north-wind benchmark and why these search defaults exist are in
[inst/reports/memetic-layout-ga.md](inst/reports/memetic-layout-ga.md).

## Arguments of `genetic_algorithm()`

Required: `Polygon1`, `n`, `Rotor`, `vdirspe`, `RotorHeight`.

### Site and turbines

| Argument | Default | Meaning |
|---|---|---|
| `Polygon1` | — | Area as sf polygon, SpatialPolygons, or coordinate matrix. Must be projected (metres). |
| `n` | — | Number of turbines (fixed; every individual has exactly `n` unique cell IDs). |
| `Rotor` | — | Rotor radius in metres. |
| `fcrR` | `5` | Grid spacing factor. Cell size is `fcrR * Rotor`. Use at least `2` so a cell covers the rotor diameter. |
| `GridMethod` | rectangular | `"h"` / `"hexagon"` for hexagonal cells. |
| `Proportionality` | `1` | Minimum fraction of a grid cell that must overlap the polygon (`prop` in `grid_area`). |
| `Projection` | EPSG:3035 | CRS if the polygon has none (numeric EPSG or PROJ string). |
| `vdirspe` | — | Wind data.frame (`ws`, `wd`, optional `probab`). See `windata_format`. |
| `referenceHeight` | `RotorHeight` | Height at which `ws` was measured. |
| `RotorHeight` | — | Hub height in metres. |
| `SurfaceRoughness` | `0.3` | Roughness length \(z_0\) in metres. Ignored per cell when `topograp = TRUE`. |

### Terrain and Weibull

| Argument | Default | Meaning |
|---|---|---|
| `topograp` | `FALSE` | Terrain model: elevation (via `elevatr`) plus Corine Land Cover for roughness and wake decay. |
| `sourceCCL` | auto / package | Path to a CLC raster (`.tif`) if you do not want the download. |
| `sourceCCLRoughness` | `inst/extdata` | CSV legend with column `Rauhigkeit_z` (`;` separated). |
| `weibull` | `FALSE` | If `TRUE`, mean speed at each turbine comes from Weibull rasters; `ws` in `vdirspe` is ignored. |
| `weibullsrc` | Austria rasters | `list(k, a)` shape and scale rasters. Package data cover Austria only. |

### GA loop

The loop is **selection → set-crossover → swap-mutation → fitness**.
`crossPart1` and `trimForce` are unused here (legacy binary API).

| Argument | Default | Meaning |
|---|---|---|
| `iteration` | `20` | Generation budget. Raise this for real searches (the gold-layout tests used 80–150). |
| `mutr` | `2/n` | Mutation probability **per turbine** (swap with an unused cell). Adaptive rates still keep this as the mutate target. |
| `selstate` | `"VAR"` | `"VAR"`: parent share follows fitness progress. `"FIX"`: always 50 %. |
| `elitism` | `TRUE` | Archive the best layout unchanged; elites also spawn children and get local search. |
| `nelit` | `3` | Base elite count. Grows in a long refine stall, drops by 1 during a disturbance pulse. |
| `crossPart1` | `"EQU"` | Unused by set-crossover. Still accepted for the old `crossover()` helper (`EQU` / `RAN`). |
| `trimForce` | `FALSE` | Unused; the genome always has `n` turbines. Kept for `trimton()`. |
| `Parallel` | `FALSE` | Parallel fitness (`parallel` + `doParallel`). |
| `numCluster` | `2` | Worker count if `Parallel` is `TRUE` (capped at cores − 1). |
| `verbose` | `FALSE` | Print a line per generation. |
| `plotit` | `FALSE` | Plot the current best layout each generation. |

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
| `windfarmGA.fitness_efficiency_weight` | `1` | Exponent \(w\) on park efficiency. `0` optimises energy only. |
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

```R
## Best layout on a leaflet map (ordered by energy)
plot_leaflet(result = resulthex, Polygon1, which = 1)

## Last generation (chronological order)
plot_leaflet(result = resulthex, Polygon1, orderitems = FALSE, which = 1)
```

The useful plots after a run:
```R
plot_windfarmGA(result, Polygon1)                 # layout + progress + heatmap
plot_result(result, Polygon1)                     # best layout
plot_parkfitness(result)                          # fitness + operator rates
plot_generation(result, Polygon1, generation = 122)  # all layouts in one generation
plot_cell_heatmap(result, Polygon1)               # which cells were tried
plot_leaflet(result, Polygon1, which = 1)         # interactive map
plot_evolution(result)                            # energy + efficiency
plot_development(result)                          # when the max improved
```

A full documentation of the genetic algorithm is given in my [master thesis](https://homepage.boku.ac.at/jschmidt/TOOLS/Masterarbeit_Gatscha.pdf).

# Shiny Windfarm Optimization
I also made a [Shiny App](https://windfarmga.shinyapps.io/windga_shiny/) for the Genetic Algorithm. 
Unfortunately, as an optimization takes quite some time and the app is currently hosted by shinyapps.io under a public license, there is only 1 R-worker at hand. So only 1 optimization can be run at a time. 

# Full Optimization example:
```R
library(sf)
library(windfarmGA)

Polygon1 <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4651704, 4651704, 4654475, 4654475, 4651704),
    c(2692925, 2694746, 2694746, 2692925, 2692925)))), 
  crs = 3035
))
plot(Polygon1, col = "blue", axes = TRUE)

wind_df <- data.frame(ws = 12, wd = 0)
windrosePlot <- plot_windrose(data = wind_df, spd = wind_df$ws,
                             dir = wind_df$wd, dirres = 10, spdmax = 20)
Rotor <- 20
fcrR <- 9
Grid <- grid_area(shape = Polygon1, size = (Rotor*fcrR), prop = 1, plotGrid = TRUE)

result <- genetic_algorithm(Polygon1 = Polygon1,
                            n = 20,
                            Rotor = Rotor, fcrR = fcrR,
                            iteration = 50,
                            vdirspe = wind_df,
                            referenceHeight = 50, RotorHeight = 100)

plot_windfarmGA(result, Polygon1)
plot_result(result, Polygon1)
plot_parkfitness(result)
plot_generation(result, Polygon1, generation = 122)
plot_cell_heatmap(result, Polygon1)
plot_leaflet(result, Polygon1, which = 1)
```
