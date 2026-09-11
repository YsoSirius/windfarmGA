# Run a Genetic Algorithm to optimize a wind farm layout

Run a Genetic Algorithm to optimize the layout of wind turbines on a
given area. The algorithm works with a fixed amount of turbines, a fixed
rotor radius and a mean wind speed value for every incoming wind
direction.

## Usage

``` r
genetic_algorithm(
  area,
  wind,
  n,
  rotor,
  rotor_height,
  grid_method = "rectangular",
  fcr = 5,
  reference_height = rotor_height,
  surface_roughness = 0.3,
  proportionality = 1,
  iteration = 20,
  mutation_rate = NULL,
  terrain = FALSE,
  elitism = TRUE,
  n_elite = 3,
  selection_mode = "VAR",
  crs = NULL,
  ccl = NULL,
  ccl_roughness = NULL,
  weibull = FALSE,
  weibull_src = NULL,
  parallel = FALSE,
  n_cluster = 2,
  verbose = FALSE,
  plot = FALSE
)
```

## Arguments

- area:

  Site polygon (`sf`, SpatialPolygons, or coordinate matrix). Must be
  projected in metres.

- wind:

  Wind data.frame with `ws`, `wd` and optional `probab`. See
  [`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md).

- n:

  Number of turbines (fixed; every individual has `n` unique cell IDs).

- rotor:

  Rotor radius in metres.

- rotor_height:

  Hub height in metres.

- grid_method:

  `"rectangular"` or `"h"` / `"hexagon"`.

- fcr:

  Grid spacing factor. Cell size is `fcr * rotor`.

- reference_height:

  Height at which `wind$ws` was measured.

- surface_roughness:

  Roughness length in metres. Per-cell when `terrain` is on.

- proportionality:

  Minimum fraction of a grid cell that must overlap the site (`prop` in
  [`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md)).

- iteration:

  Generation budget.

- mutation_rate:

  Swap probability per turbine. `NULL` means `2/n`.

- terrain:

  Terrain model (elevation + land cover).

- elitism:

  Archive the best layout and breed elite children.

- n_elite:

  Base elite count (grows/shrinks with search phase).

- selection_mode:

  `"VAR"` (parent share follows fitness) or `"FIX"` (50 %).

- crs:

  CRS if `area` has none (EPSG code or PROJ string).

- ccl:

  Path to a Corine Land Cover raster when `terrain` is on.

- ccl_roughness:

  Path to the CLC legend CSV (`Rauhigkeit_z` column).

- weibull:

  If `TRUE`, hub-height speed comes from Weibull rasters; `wind$ws` is
  ignored.

- weibull_src:

  `list(k, a)` shape and scale rasters. Package data cover Austria.

- parallel:

  Parallel fitness (`parallel` + `doParallel`).

- n_cluster:

  Worker count when `parallel` is `TRUE`.

- verbose:

  Print a line per generation.

- plot:

  Plot the current best layout each generation.

## Value

The result is a matrix with aggregated values per generation; the best
individual regarding energy and efficiency per generation, some fuzzy
control variables per generation, a list of all fitness values per
generation, the amount of individuals after each process, a matrix of
all energy, efficiency and fitness values per generation, the selection
and crossover parameters, a matrix with the generational difference in
maximum and mean energy output, a matrix with the given inputs, a
dataframe with the wind information, the mutation rate per generation
and a matrix with all tested wind farm layouts.

## Details

A terrain effect model can be included in the optimization process.
Therefore, a digital elevation model will be downloaded automatically
via the
[`elevatr::get_elev_raster`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html)
function. A land cover raster can also downloaded automatically from the
EEA-website, or the path to a raster file can be passed to `ccl`. The
algorithm uses an adapted version of the Raster legend
("clc_legend.csv"), which is stored in the package directory
`~/inst/extdata`. To use other values for the land cover roughness
lengths, insert a column named **"Rauhigkeit_z"** to the .csv file,
assign a surface roughness length to all land cover types. Be sure that
all rows are filled with numeric values and save the file with **";"**
separation. Assign the path of the file to the input variable
`ccl_roughness` of this function.

Fitness is \\EnergyOverall \times (EfficAllDir/100)^w\\ with
`w = getOption("windfarmGA.fitness_efficiency_weight")`. Hub-height wind
speeds use a logarithmic profile unless
`options(windfarmGA.wind_profile = "power")` restores the legacy power
law. Power uses `options(windfarmGA.Cp)` (default 0.45) and optional
cut-in / rated / cut-out speeds. Layouts are encoded as `n` unique
grid-cell IDs (set crossover and swap mutation). Selection defaults to
`VAR` (percentage follows fitness progress). Mutation, immigrants and
unused-cell injection prefer rarely visited cells. Crossover is spatial
with probability `options(windfarmGA.spatial_crossover)` (default 0.5).
Evaluated layouts are cached. A flat global max is not a stop signal.
The run ends at `iteration`, or earlier only after
`options(windfarmGA.stall_generations)` consecutive generations with no
new layout, no newly visited cell and no new best fitness (set to `0` to
disable). Operator rates cycle like seasons, still only selection /
set-crossover / swap-mutation: explore (rates rise on stall) until
`options(windfarmGA.refine_min_gen)` (default 18) and
`options(windfarmGA.refine_after)` (default 12) generations without a
new max at coverage \\\ge 0.35\\; then refine (inject toward 0.15,
mutation toward `2/n`, selection toward about 45\\
`options(windfarmGA.refine_hold)` generations in refine (default 25), a
short disturbance pulse of `options(windfarmGA.explore_pulse)`
generations (default 10) raises the same rates even if new maxes are
still trickling in, then refine resumes. Elites get a short local search
each generation: one turbine slides to a neighbouring empty cell (not a
random cell anywhere on the grid).

## See also

Other Genetic Algorithm Functions:
[`crossover()`](https://YsoSirius.github.io/windfarmGA/reference/crossover.md),
[`fitness()`](https://YsoSirius.github.io/windfarmGA/reference/fitness.md),
[`init_population()`](https://YsoSirius.github.io/windfarmGA/reference/init_population.md),
[`mutation()`](https://YsoSirius.github.io/windfarmGA/reference/mutation.md),
[`selection()`](https://YsoSirius.github.io/windfarmGA/reference/selection.md),
[`set_crossover()`](https://YsoSirius.github.io/windfarmGA/reference/set_crossover.md),
[`swap_mutation()`](https://YsoSirius.github.io/windfarmGA/reference/swap_mutation.md),
[`trimton()`](https://YsoSirius.github.io/windfarmGA/reference/trimton.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## Create a random rectangular shapefile
library(sf)

area <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))

## Create a uniform and unidirectional wind data.frame and plot the
## resulting wind rose
data.in <- data.frame(ws = 12, wd = 0)
windrosePlot <- plot_windrose(
  data = data.in, spd = data.in$ws,
  dir = data.in$wd, dirres = 10, spdmax = 20
)

## Runs an optimization run for 20 iterations with the
## given shapefile (area), the wind data.frame (data.in),
## 12 turbines (n) with rotor radii of 30m and hub height of 100m.
result <- genetic_algorithm(
  area = area,
  n = 12,
  wind = data.in,
  rotor = 30,
  rotor_height = 100
)
plot_windfarmGA(result = result, area = area)
} # }
```
