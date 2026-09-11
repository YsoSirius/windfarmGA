# windfarmGA 5.0.0

Breaking release: the layout chromosome is no longer a 0/1 string.
`4.0.1` is skipped; this would not have been a patch on `4.0.0`.

## Breaking
* Each individual is `n` unique grid-cell IDs, not a binary vector over all
  cells. `genetic_algorithm` runs
  `selection` → `set_crossover` → `swap_mutation` → `get_grids` → `fitness`.
  `trimton` is not in the loop.
* `selection()` returns an ID matrix (`n` × selected) plus fitness. Code that
  piped `selection()` into `crossover()` must convert IDs to binary or switch
  to `set_crossover()`.
* `crossPart1` and `trimForce` are unused by the combinatorial genome
  (kept for the legacy helpers `crossover()` / `trimton()`).
* Defaults: `selstate = "VAR"`, mutation `2/n` (was often `0.8`), `nelit = 3`.
  New session options (inject, immigrants, seasons, neighbour local search)
  change search behaviour even if you call `genetic_algorithm()` the same way.
* Legacy `crossover()`, `mutation()` and `trimton()` still accept 0/1
  chromosomes. New exports: `set_crossover()`, `swap_mutation()`.

## Fixes
* CRAN tests for `plot_windrose()` failed on r-devel with ggplot2 >= 4.0.0.
  ggplot2 4.0 uses S7 plot objects, so they are no longer recursive lists and
  `class(.)[1]` is no longer `"gg"`. Checks now use
  `inherits(., c("ggplot", "ggplot2::ggplot"))`, which works with ggplot2 3.x
  and 4.x.
* `crossover`: parent fitness is now `(a + b) / 2` (operator precedence bug).
* `isSpatial`: assigned CRS is the given `proj`, not hardcoded EPSG:3035.
* Fitness is `EnergyOverall * (EfficAllDir/100)^w` with
  `options(windfarmGA.fitness_efficiency_weight)` (default 1).
* Elitism copies the best layouts into the next generation instead of
  multiplying their fitness by 10.
* Duplicate layouts after crossover/mutation (sorted ID keys) are dropped.
* Logarithmic hub-height wind profile (legacy power law via
  `options(windfarmGA.wind_profile = "power")`).
* Power coefficient `options(windfarmGA.Cp)` defaults to 0.45; optional
  cut-in / rated / cut-out speeds.
* Parallel clusters are always stopped via `on.exit`; dead Weibull crop
  call removed.
* Diagnostic plots no longer crash on negative leftover EQU values; rates
  are drawn as percentages, not as palette indices.
* `plot_windrose()` uses a white panel instead of the gray fill and thick
  minor rings.
* `plot_population()` was slow because `population_census()` rescanned
  every layout with `Run == r` per individual. Counts now come from `nindiv`
  (`cells`, `cells_elite` stored during the run); the fallback uses `split()`.
  Elite offspring is green (`#27AE60`), elites stay orange.

## Features
* Combinatorial genome: each individual is `n` unique grid-cell IDs, not a
  0/1 string over all cells. `genetic_algorithm` now runs
  `selection` → `set_crossover` → `swap_mutation` → `get_grids` → `fitness`.
  `trimton` is no longer in the loop (always exactly `n` turbines).
* `plot_windfarmGA()`, `plot_parkfitness()`, `plot_population()` and
  `plot_generation()` wait for Enter between every page in an interactive
  session (Plots pane, so pages are not overwritten). plotly is only used
  when `ask = FALSE`. `plot_population()` has a bottom legend, an elite
  cell-count line (the population can sit at the full grid size, e.g. 70,
  while elites shrink), and a park-efficiency page.
* `plot_generation(result, Polygon, generation = 122)` shows every layout
  evaluated in that generation (cell occupancy + all turbines, best in
  black, plus the top distinct maps). `generation_layouts()` returns the
  table. `plot_parkfitness()` is ggplot2 with the legend outside; rates
  are ordered Selection / Crossover inject / Mutation. Hoverable via
  plotly in an interactive session.
* Fitness cache: identical layouts (sorted cell IDs) are not re-evaluated.
  Early stop is not "max unchanged". The run uses the full `iteration`
  budget unless `options(windfarmGA.stall_generations)` consecutive
  generations produce no new layout, no new cell and no new best
  (set to 0 to never stop early). A flat maximum while new sites are
  still tried is treated as ongoing search.
* Weak elitism: the current best layout is archived unchanged. Each elite
  then produces several mutated copies
  (`options(windfarmGA.elite_children)`, default 3) and mixes with weaker
  layouts (`windfarmGA.elite_mix`, default 2, inject 0 so elite structure
  stays). Extra mutants when the max has been flat for 10 generations.
  Elite count starts at `nelit` (default 3), grows by 2–3 during a long
  refine stall, and drops by 1 in a disturbance pulse.
  Operator rates follow seasons with the same three operators.
  Explore: inject in `[0.20, 0.40]` and mutation rise while the max is
  stalling (VAR selection starts near 55%). Refine: after
  `options(windfarmGA.refine_min_gen)` (18) and
  `options(windfarmGA.refine_after)` (12) generations without a new max
  at cell coverage ≥ 0.35, inject decays toward 0.15, mutation toward
  `2/n`, selection toward ~45%. Refine is not a trap: after
  `options(windfarmGA.refine_hold)` (25) generations a disturbance pulse
  of `options(windfarmGA.explore_pulse)` (10) gens raises inject/mutation
  again even if new maxes still appear, then refine resumes. Local search
  slides one elite turbine to a neighbouring empty cell (rook / hex),
  not to a random cell anywhere on the grid.
* Mutation and immigrants prefer rarely visited cells. Crossover is spatial
  (half-plane) with probability `options(windfarmGA.spatial_crossover)`
  (default 0.5). Elites get a memetic local search
  (`windfarmGA.local_search_elites` default 5 /
  `local_search_tries` default 6).
* Default `selstate` is `VAR` (selection share follows fitness). Mutation
  and crossover inject rates adapt each generation from max fitness and
  the top quartile (immigrants no longer freeze the controller).
* `options(windfarmGA.max_selection)` default is 300 (was 100), matching
  `max_population`.
* Default mutation rate is `2/n` per turbine, with at least one swap
  (`options(windfarmGA.min_swaps)`). Set-crossover injects unused cells
  (`options(windfarmGA.crossover_inject)`, default 0.25) so the search is
  not trapped in the parental union. Each generation adds random immigrant
  layouts (`options(windfarmGA.immigrants)`, default 3). Default elite
  count is 3.
* Report `inst/reports/memetic-layout-ga.md`: short English note on the
  combinatorial genome, neighbourhood local search, and the north-wind
  benchmark (defaults after that: LS 5×6). Figure: `fig-north-gold.png`.
* README documents all `genetic_algorithm()` arguments and
  `options(windfarmGA.*)` (physics, inject, immigrants, seasons, neighbour
  local search). Examples no longer pass leftover `FIX` / `mutr = 0.8` /
  `trimForce` as if they were still the recommended setup.
* `selection()` returns an ID matrix (`n` × selected) plus fitness; it no
  longer expands layouts to a binary grid.
* `get_grids()` accepts ID matrices and still accepts legacy binary matrices.
* Exported legacy API kept: `crossover()`, `mutation()`, `trimton()` still
  work on 0/1 chromosomes. New exports: `set_crossover()`, `swap_mutation()`.

## Open / Todos
* Submit 5.0.0 to CRAN (ggplot2 4.x tests plus combinatorial genome).
* Parallel and terrain tests remain skipped on CRAN (`skip_on_cran`).
* Consider splitting the large `test_plots.R` block so a single assertion
  failure does not hide later plot checks.
* `plot_heatmap()` was never reimplemented; use `plot_cell_heatmap()`.
* Callers that piped `selection()` into `crossover()` must convert IDs to
  binary or switch to `set_crossover()`.
* Tune `iteration` as the real runtime budget. `stall_generations` now
  only fires when the search is idle (no new layouts/cells), not when
  the record is merely flat. If rates freeze at the refine floor while
  the max still creeps, the disturbance pulse should lift them; shorten
  `windfarmGA.refine_hold` or lengthen `explore_pulse` if it still feels
  stuck. Local search now slides to a neighbour cell.
* Spatial EQU/RAN crossover is unused; set-crossover can use a half-plane
  split when coordinates are passed.

## Ideas
* Census could also show immigrants, cache hits and local-search tries if
  those counts are stored per generation.
* Pin or document ggplot2 compatibility in `Suggests` if further S7 class
  cleanup removes the legacy `"ggplot"` S3 class.
* Full manufacturer power curve as input, not only cut-in / rated / cut-out.

# windfarmGA 4.0.0
- Depends on R 4.1.0
- Removed package dependencies `rgdal` and `rgeos`
- Replaced dependency `raster` with `terra`
- Moved dependencies `foreach`, `parallel`, `doParallel`, `elevatr` to Suggests
- 3D-circle intersection calculation. Especially relevant if the terrain model is activated.
- Fix `grid_area` and `hexa_area` functions for sf-1.0 and s2
- Removed most visibility functions in favor of `terra::viewshed`
- Removed the function `windfarmGA`, as it was redundant and just difficult to test.
- Changed most `cat()` and `print()` calls with `message()`
- Reset old `par` settings

# windfarmGA 3.0.0
* The dependencies `sp`, `spatstat` were removed and replaced by `sf`. All spatial outputs are now **Simple Features**. A Shapefile Polygon can still be passed as input to `genetic_algorithm` / `windfarmGA`, but more underlying functions now require the Polygon to be of type Simple Feature.
* The functions `grid_area` & `hexa_area` are now calculated with `sf::st_make_grid`.
* The new dependency `elevatr` has been added because it provides elevation data with a higher resolution compared to `raster::getData`.
* Several `dependencies` that are not essential for the algorithm were moved to `Suggests`.
* Bugfix for the calculation of the visibility analysis. [#17](https://github.com/YsoSirius/windfarmGA/issues/17)
* The `viewshed` parameter `h1` can now also be a numeric vector with different height offsets. [#18](https://github.com/YsoSirius/windfarmGA/issues/18)
* The `grid_area` argument `resol` changed to `size`.
* The arguments for `get_dist_angles` have changed to match the arguments of `turbine_influences`.

# windfarmGA 2.3.0
* Due to the changes in the PROJ 6 library and its handling of coordinate reference systems, some adjustments were necessary. Attempts were made to ensure backward compatibility. However, warnings like the following are now increasingly displayed:
  ```sh
  Warning message:
  In showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj = prefer_proj) :
    Discarded datum European_Terrestrial_Reference_System_1989 in CRS definition
  ```

# windfarmGA 2.2.3
* Fix tests for R 3.4.0, as class(matrix) is of length 2.
* Expose more options which can be set with `options(windfarmGA.cT = 0.75)`:  

  Options = Default value           | Description
  ----------------------------------| ------------------------  
  windfarmGA.cT = 0.88              | Wind Turbine Thrust coefficient  
  windfarmGA.air_rh = 1.225         | Air Density Value  
  windfarmGA.k = 0.075              | Wake expansion coefficient. It is assumed to be 0.075 for onshore windfarms and 0.05 for offshore windfarms
  windfarmGA.max_angle = 20         | Maximum angle (in degrees) to search for potentially influencing turbines
  windfarmGA.max_distance = 100000  | Maximum distance (in meters) to search for potentially influencing turbines
  windfarmGA.max_population = 300   | Maximum number of individuals per generation
  windfarmGA.max_selection = 100    | Maximum number of selected individuals

# windfarmGA 2.2.2

#### Pkgdown
* A [pkgdown documentation](https://ysosirius.github.io/windfarmGA/) site is now available.

#### Renaming Functions
* Almost all functions have been renamed to have a consistent appearance and a clearer meaning.
The old functions still exist, but are deprecated now.

  <details>
    <summary>Renaming Overview</summary>
    <p>
  
  Old names             | **New names**
  --------------------- | ---------------------
  StartGA               | **init_population**
  selection1            | **selection**
  crossover1            | **crossover**
  VekWinkelCalc         | **get_dist_angles**
  calculateEn           | **calculate_energy**
  getRects              | **get_grids**
  BaroHoehe             | **barometric_height**
  GridFilter            | **grid_area**
  HexaTex               | **hexa_area**
  InfluPoints           | **turbine_influences**
  genAlgo               | **genetic_algorithm**
  RandomSearch          | **random_search**
  RandomSearchTurb      | **random_search_single**
  RandomSearchPlot      | **plot_random_search**
  leafPlot              | **plot_leaflet**
  heatmapGA             | **plot_heatmap**
  plotbeorwor           | **plot_development**
  plotCloud             | **plot_cloud**
  plotEvolution         | **plot_evolution**
  plotfitnessevolution  | **plot_fitness_evolution**
  plotparkfitness       | **plot_parkfitness**
  plotResult            | **plot_result**
  PlotWindfarmGA        | **plot_windfarmGA**
  plotWindrose          | **plot_windrose**
  
  </p>
  
  </details>

#### Bugfixes / Other Changes 
* The legend of `plot_leaflet` now works correctly.

* Some general linting / spell checking / performance optimization was done.

* The Weibull Raster (for Austria) are now in a separate [Github-repository](https://github.com/YsoSirius/windfarm_data), instead of
being stored in the package as .rda file. I guess this never worked except on my computer.

* The Corine Land Cover .tif file is also stored in that repository, as the EEA webpage did restrict
downloads sometimes, which resulted in an error.

* The `plot_farm_3d` function has temporarily been removed from the package.

* Most functions that required user-input previously used `readline` which is now changed to `readLines` as it allows to read from a file instead. This can be set via `options(windfarmGA.connection = file())`.

* A whole lot of tests were written.

# windfarmGA 2.2.1

#### Performance Tuning / Restructuring
* Switch to **matrices** instead of data.frames and a lot of restructuring and 
performance optimization of the whole algorithm.

#### Viewshed Analysis
* New set of functions, to analyze the visual impact of a wind farm.
    + `cansee`,
    + `viewTo`, 
    + `rasterprofile`, 
    + `viewshed`, 
    + `plot_viewshed`, 
    + `interpol_view`, 
    + `getISO3`, 
    + `getDEM`

#### Other Changes 
* The function `genAlgo`/`windfarmGA` and the plotting functions now accept SimpleFeature Polygons or coordinates in table format with long, lat or x, y column names. The terrain effect model can now be activated only by setting **topograp** to TRUE and it will attempt to download the land cover raster from the European Environment Agency website.

* `plot_farm_3d` Experimental rayshader function

# windfarmGA 1.2.1

#### Randomization
The output of `genAlgo` or `windfarmGA` can be further randomized/optimized with the following
functions:
- RandomSearch
- RandomSearchTurb

**`RandomSearch`** is used to randomize all turbines of the layout

**`RandomSearchTurb`** is used to randomize a single turbine

**`RandomSearchPlot`** is used to plot the output of those functions, comparing them with the 
original result.

```sh
load(file = system.file("extdata/resultrect.rda", package = "windfarmGA"))
load(file = system.file("extdata/polygon.rda", package = "windfarmGA"))
Res = RandomSearchTurb(result = resultrect, Polygon1 = polygon, n=10)
RandomSearchPlot(resultRS = Res, result = resultrect, Polygon1 = polygon, best=2)
```

# windfarmGA 1.2
#### Parallel Processing
```sh
## Runs the same optimization, but with parallel processing and 3 cores.
result_par <- genAlgo(Polygon1 = Polygon1, GridMethod ="h", n=12, Rotor=30,
                 fcrR=5,iteration=10, vdirspe = data.in,crossPart1 = "EQU",
                 selstate="FIX",mutr=0.8, Proportionality = 1,
                 SurfaceRoughness = 0.3, topograp = FALSE,
                 elitism=TRUE, nelit = 7, trimForce = TRUE,
                 referenceHeight = 50,RotorHeight = 100,
                 Parallel = TRUE, numCluster = 3)
PlotWindfarmGA(result = result_par, GridMethod = "h", Polygon1 = Polygon1)
```

# windfarmGA 1.1


#### Optimization with Hexagonal Grid Cells
```sh
result_hex <- genAlgo(Polygon1 = Polygon1, GridMethod ="h", n=12, Rotor=30,
                  fcrR=5,iteration=10, vdirspe = data.in,crossPart1 = "EQU",
                  selstate="FIX",mutr=0.8, Proportionality = 1,
                  SurfaceRoughness = 0.3, topograp = FALSE,
                  elitism=TRUE, nelit = 7, trimForce = TRUE,
                  referenceHeight = 50,RotorHeight = 100)
PlotWindfarmGA(result = result_hex, GridMethod = "h", Polygon1 = Polygon1)
```
