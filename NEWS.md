# windfarmGA 5.0.0

Breaking release: layouts are `n` unique grid-cell IDs, not a 0/1 string.

## Breaking
* Genome is `n` cell IDs. The loop is
  `selection` → `set_crossover` → `swap_mutation` → `get_grids` → `fitness`.
  `trimton` is not in the loop. `selection()` returns an ID matrix;
  pipe it into `set_crossover()`, not the old binary `crossover()`.
* Public API is snake_case: `area`, `wind`, `rotor`, `mutation_rate`,
  `selection_mode`, `crs`. `plotit` is now `plot`. Unused loop args
  `crossPart1` and `trimForce` are gone. Invalid `selection_mode` errors
  (no interactive prompt).
* Defaults that change a run: `selection_mode = "VAR"`, mutation `2/n`
  (was often `0.8`), `n_elite = 3`. Session options (inject, immigrants,
  seasons, local search) also change search behaviour.
* Legacy `crossover()`, `mutation()` and `trimton()` still accept 0/1
  chromosomes. New exports: `set_crossover()`, `swap_mutation()`.
* `plot_fitness_evolution()` is gone (it only called `plot_parkfitness()`).

## New
* Results have class `windfarmGA` (`print` / `plot`). `ga_options()`
  lists or sets `options(windfarmGA.*)`.
* `explore_result()`: Shiny map of a generation, wind rose, downwind
  wake cones, optional terrain layers + turbine popups. Click a new
  fitness max to jump the slider.
* Fitness is \(E \times (\eta/100)^w\) (`windfarmGA.fitness_efficiency_weight`,
  default 1). Log hub-height profile (power law via
  `windfarmGA.wind_profile = "power"`). Optional manufacturer curve
  (`ga_options(power_curve = …)`, `read_power_curve()`, `plot_power_curve()`).
  `Cp` default 0.45; optional cut-in / rated / cut-out.
* Elitism archives the best layout (no fitness × 10). Elite children,
  neighbour local search, fitness cache, immigrants, spatial crossover.
  Early stop only after `stall_generations` idle gens (no new layout,
  cell, or best).
* Terrain: pass a DEM (`terrain = dem`) to skip the download. Per-cell
  height / \(z_0\) / \(k\) / air density are computed once and stored
  as `terrainModel` for `plot_result` and `random_search`.
* `random_search()` jitters turbines inside their cells (`runs`).
  Terrain is reused from `result`; pass `weibull_src` again (not
  stored). `plot_random_search()` draws both panels on the same grid.
* Wake search and circle overlap run in C++. Turbines exactly upwind
  (`alpha = 0`) are kept (old triangle test dropped them).
* Wind helpers: `wind_from_uv()`, `wind_from_series()`. GitHub-only
  extras in `experimental/` (ERA5/GWA, noise, rayshader, mapgl) are
  not in the CRAN tarball. `plot_mapgl_from_result()` reuses
  `terrain_tiles/` / `terrain.mbtiles`, draws wake cones colored by
  `AbschGesamt` (0% green → max red) and the rayshader OBJ
  (`wind_turbine_v1.obj`). Three.js is r149 UMD from localhost.
  TileJSON Terrarium encoding; `maxzoom` is the highest tile folder.
  Basemaps: `satellite`, `topo`, Carto. Port 8000 leftover servers
  are freed (`rebuild = TRUE` remakes the tiles).
* Plots wait for Enter between pages when interactive. `plot_generation()`
  shows every layout in a generation. `plot_viewshed()` projects lon/lat
  DEMs first.
* `genetic_algorithm(on_generation = ...)` calls
  `function(generation, iteration, energy, efficiency, fitness)` after
  each generation so a Shiny app can show progress without forking the
  GA. `wake_cones()` is the public name for the downwind search-cone
  polygons (`plot_leaflet`, MapGL). Internal geometry is
  `wake_cone_polys()`.

## Fixes
* Weibull speed no longer becomes all-NA after masking (fitness crash).
* Power-curve energy is the park sum, not the first turbine.
* `clc_legend.csv` is in the tarball again (`.Rbuildignore` had dropped
  every CSV).
* `crossover` parent fitness is `(a + b) / 2`. `isSpatial` uses the
  given CRS, not hardcoded 3035.
* `plot_windrose()` works with ggplot2 4.x (S7). If ggplot2 4 meets an
  old `systemfonts`, update `systemfonts`.
* Parallel clusters always stop via `on.exit`.
* `plot_result(terrain = dem)` no longer errors on a SpatRaster.
  Weibull file paths get a CRS before crop. PROJ < 6 branches are gone.
* `with_mocked_bindings()` in `test_coverage_branches.R` wraps the
  ggplot2-missing checks in `code = { … }` (testthat requires named
  bindings). `random_search()` test uses `runs`, not the removed `n`.
* `resultrect` is the first 50 generations of the old 200-iteration
  run, saved with `xz` (~90 KB). A new 50-gen GA is larger because
  `allCoords` now stores a bigger population.
* `plot_cell_heatmap()` / `ga_result_grid()` no longer assume a missing
  CRS is lon/lat. Meter coordinates get the result projection (same as
  `plot_leaflet()`). The 4326 round-trip broke the grid on macOS.
* Roxygen: unused `@inheritParams` on `trimton()` and
  `turbine_influences()` removed (params were already documented).
* Rd help is ASCII-only (`≈`, `×`, em-dash broke the CRAN PDF manual).
  README no longer links to the archived check-results page (404).
* Parallel: `n_cluster` is capped at 2 under `_R_CHECK_LIMIT_CORES_`
  (R errors on `makeCluster(detectCores() - 1)`). Terrain tests no
  longer need a local `g100_06.tif`.
* `random_search_single(turbine = …)` skips the interactive prompt.
  User-input tests no longer write to a fake `stdin`.

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
* The function `genAlgo`/`windfarmGA` and the plotting functions now accept SimpleFeature Polygons or coordinates in table format with long, lat or x, y column names. The terrain effect model can now be activated only by setting **terrain** to TRUE and it will attempt to download the land cover raster from the European Environment Agency website.

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
Res = RandomSearchTurb(result = resultrect, area = polygon, n=10)
RandomSearchPlot(resultRS = Res, result = resultrect, area = polygon, best=2)
```

# windfarmGA 1.2
#### parallel Processing
```sh
## Runs the same optimization, but with parallel processing and 3 cores.
result_par <- genAlgo(area = area, grid_method ="h", n=12, Rotor=30,
                 fcr=5,iteration=10, wind = data.in,crossPart1 = "EQU",
                 selection_mode="FIX",mutation_rate=0.8, proportionality = 1,
                 surface_roughness = 0.3, terrain = FALSE,
                 elitism=TRUE, n_elite = 7, trimForce = TRUE,
                 reference_height = 50,rotor_height = 100,
                 parallel = TRUE, n_cluster = 3)
PlotWindfarmGA(result = result_par, grid_method = "h", area = area)
```

# windfarmGA 1.1.0


#### Optimization with Hexagonal Grid Cells
```sh
result_hex <- genAlgo(area = area, grid_method ="h", n=12, Rotor=30,
                  fcr=5,iteration=10, wind = data.in,crossPart1 = "EQU",
                  selection_mode="FIX",mutation_rate=0.8, proportionality = 1,
                  surface_roughness = 0.3, terrain = FALSE,
                  elitism=TRUE, n_elite = 7, trimForce = TRUE,
                  reference_height = 50,rotor_height = 100)
PlotWindfarmGA(result = result_hex, grid_method = "h", area = area)
```



# windfarmGA 1.0.0

Initial release
