# Package index

## Package Documentation

- [`windfarmGA`](https://YsoSirius.github.io/windfarmGA/reference/windfarmGA-package.md)
  [`windfarmGA-package`](https://YsoSirius.github.io/windfarmGA/reference/windfarmGA-package.md)
  : windfarmGA: Genetic Algorithm for Wind Farm Layout Optimization

## Run an Optimization

- [`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)
  : Run a Genetic Algorithm to optimize a wind farm layout

## Genetic Algorithm Components

Main components of the genetic algorithm

- [`init_population()`](https://YsoSirius.github.io/windfarmGA/reference/init_population.md)
  : Create a random initial Population
- [`selection()`](https://YsoSirius.github.io/windfarmGA/reference/selection.md)
  : Selection Method
- [`fitness()`](https://YsoSirius.github.io/windfarmGA/reference/fitness.md)
  : Evaluate the Individual Fitness values
- [`crossover()`](https://YsoSirius.github.io/windfarmGA/reference/crossover.md)
  : Crossover Method
- [`mutation()`](https://YsoSirius.github.io/windfarmGA/reference/mutation.md)
  : Mutation Method
- [`trimton()`](https://YsoSirius.github.io/windfarmGA/reference/trimton.md)
  : Adjust the amount of turbines per windfarm

## Topographic Model

- [`terrain_model()`](https://YsoSirius.github.io/windfarmGA/reference/terrain_model.md)
  : Get topographic rasters

## Wind Energy Calculation Functions

- [`barometric_height()`](https://YsoSirius.github.io/windfarmGA/reference/barometric_height.md)
  : Calculates Air Density, Air Pressure and Temperature according to
  the Barometric Height Formula
- [`get_dist_angles()`](https://YsoSirius.github.io/windfarmGA/reference/get_dist_angles.md)
  : Calculate distances and angles of possibly influencing turbines
- [`turbine_influences()`](https://YsoSirius.github.io/windfarmGA/reference/turbine_influences.md)
  : Find potentially influencing turbines
- [`calculate_energy()`](https://YsoSirius.github.io/windfarmGA/reference/calculate_energy.md)
  : Calculate Energy Outputs of Individuals
- [`circle_intersection()`](https://YsoSirius.github.io/windfarmGA/reference/circle_intersection.md)
  : Get area of intersecting circles

## Randomization

- [`random_search()`](https://YsoSirius.github.io/windfarmGA/reference/random_search.md)
  : Randomize the output of the Genetic Algorithm
- [`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)
  : Randomize the location of a single turbine
- [`plot_random_search()`](https://YsoSirius.github.io/windfarmGA/reference/plot_random_search.md)
  : Plot the result of a randomized output.

## Plotting Functions

- [`plot_cloud()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cloud.md)
  : Plot outputs of all generations with standard deviations
- [`plot_development()`](https://YsoSirius.github.io/windfarmGA/reference/plot_development.md)
  : Plot the progress of populations
- [`plot_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_evolution.md)
  : Plot the evolution of fitness values
- [`plot_fitness_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_fitness_evolution.md)
  : Plot the changes of min/mean/max fitness values
- [`plot_leaflet()`](https://YsoSirius.github.io/windfarmGA/reference/plot_leaflet.md)
  : Plot a wind warm with leaflet
- [`plot_parkfitness()`](https://YsoSirius.github.io/windfarmGA/reference/plot_parkfitness.md)
  : Plot the genetic algorithm results
- [`plot_random_search()`](https://YsoSirius.github.io/windfarmGA/reference/plot_random_search.md)
  : Plot the result of a randomized output.
- [`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md)
  : Plot the best results
- [`plot_viewshed()`](https://YsoSirius.github.io/windfarmGA/reference/plot_viewshed.md)
  : Plot visibility
- [`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md)
  : Plot the results of an optimization run
- [`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md)
  : Plot a Windrose

## Viewshed Analysis Functions

- [`plot_viewshed()`](https://YsoSirius.github.io/windfarmGA/reference/plot_viewshed.md)
  : Plot visibility

## Helper Functions

- [`get_grids()`](https://YsoSirius.github.io/windfarmGA/reference/get_grids.md)
  : Get the Grid-IDs from binary matrix
- [`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md)
  : Make a grid from a Simple Feature Polygon
- [`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md)
  : Polygon to Hexagonal Grids
- [`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)
  : Transform Winddata
- [`isSpatial()`](https://YsoSirius.github.io/windfarmGA/reference/isSpatial.md)
  : Transform to Simple Feature Polygons
- [`permutations()`](https://YsoSirius.github.io/windfarmGA/reference/permutations.md)
  : Enumerate the Combinations or Permutations of the Elements of a
  Vector
- [`readinteger()`](https://YsoSirius.github.io/windfarmGA/reference/readinteger.md)
  : Check Input Crossover Method
- [`readintegerSel()`](https://YsoSirius.github.io/windfarmGA/reference/readintegerSel.md)
  : Check Input Selection Method
- [`splitAt()`](https://YsoSirius.github.io/windfarmGA/reference/splitAt.md)
  : Split matrices or numeric vectors at specific indices
- [`is_foreach_installed()`](https://YsoSirius.github.io/windfarmGA/reference/package_installed.md)
  [`is_parallel_installed()`](https://YsoSirius.github.io/windfarmGA/reference/package_installed.md)
  [`is_doparallel_installed()`](https://YsoSirius.github.io/windfarmGA/reference/package_installed.md)
  [`is_ggplot2_installed()`](https://YsoSirius.github.io/windfarmGA/reference/package_installed.md)
  [`is_leaflet_installed()`](https://YsoSirius.github.io/windfarmGA/reference/package_installed.md)
  [`is_elevatr_installed()`](https://YsoSirius.github.io/windfarmGA/reference/package_installed.md)
  : Is the package installed or not

## Datasets

Results and Shapefiles included in the package.

- [`resultrect`](https://YsoSirius.github.io/windfarmGA/reference/resultrect.md)
  :

  A resulting matrix of `genetic_algorithm` with 200 iterations and a
  rectangular grid derived from `sp_polygon`

- [`resulthex`](https://YsoSirius.github.io/windfarmGA/reference/resulthex.md)
  :

  A resulting matrix of `genetic_algorithm` with 10 iterations and a
  hexagonal grid derived from `sp_polygon`

- [`sp_polygon`](https://YsoSirius.github.io/windfarmGA/reference/sp_polygon.md)
  :

  The rectangular POLYGON used to create `resultrect` & `resulthex`

- [`big_shape`](https://YsoSirius.github.io/windfarmGA/reference/big_shape.md)
  : A POLYGON with an area of ~70 km2

- [`hole_shape`](https://YsoSirius.github.io/windfarmGA/reference/hole_shape.md)
  : A POLYGON with a hole

- [`multi_shape`](https://YsoSirius.github.io/windfarmGA/reference/multi_shape.md)
  : A MULTIPOLYGON with 3 Polygons
