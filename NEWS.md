# windfarmGA (development version)

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
