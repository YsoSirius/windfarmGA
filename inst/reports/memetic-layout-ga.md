# A memetic genetic algorithm for wind farm layout optimization

**windfarmGA 5.0.0**

## Abstract

windfarmGA places a fixed number of turbines on a discrete grid. Version 5.0.0 encodes each candidate as *n* unique cell identifiers rather than a binary string over the whole grid, so crossover and mutation never change the turbine count. The search remains a genetic algorithm—selection, crossover, mutation, fitness—extended with elitism, immigration, and a short neighborhood search on the current best layouts. On a uniform northerly wind, the known optimum occupies the northern and southern grid rows (24 585 kW, 91.28 % efficiency). Random swaps stalled near 98 % of that fitness, typically with a few turbines one row inland. Sliding one elite turbine to an adjacent empty cell recovered the exact layout. Increasing the intensity of that neighborhood search reduced time-to-optimum from about 110 generations to about 30 on the tested seed.

## Encoding

The design variables are combinatorial: exactly *n* turbines, each on a different cell, with wakes between neighbors. Fitness is

\[
F = E \cdot (\eta / 100)^{w},
\]

where *E* is park energy, *η* is park efficiency, and *w* = 1. The wake model, power coefficient, and logarithmic hub-height profile are unchanged from earlier releases.

Until version 4.0 each chromosome was a 0/1 vector over all cells. After crossover or mutation the number of selected bits often differed from *n*; a repair step (`trimton`) then added or deleted turbines at random. That repair destroyed neighborhood structure and produced illegal or duplicate layouts. In 5.0.0 an individual is a set of *n* cell IDs (stored as an *n* × population matrix). The loop is selection, set-crossover, swap-mutation, then fitness. The binary helpers remain available for existing scripts; they are not used by `genetic_algorithm`.

**Set-crossover** keeps the intersection of the parents. Remaining slots are filled from the symmetric difference and, with probability 0.25, from cells used by neither parent. Without that injection the search is confined to the parental union. With probability 0.5 a half-plane splits the site spatially.

**Swap-mutation** replaces a turbine with an unused cell at probability 2/*n* per turbine (at least one swap). Rarely visited cells are preferred.

**Selection** is variable by default: the parent share tracks fitness progress. A fixed 50 % share remains available.

## Memetic search

Elites, immigrants, and local search do not replace the evolutionary loop; they insert or refine individuals after the genetic operators.

The current best layout is archived unchanged (`nelit` = 3). Each elite also produces a few mutated copies and mixes with weaker layouts (injection off, so the elite structure is kept). Three random immigrants enter each generation. Identical ID sets are cached and not re-evaluated; duplicate layouts are dropped.

Local search used to swap an elite turbine to a *random* free cell. On the north-wind site that move usually parks a turbine in the interior and is rejected, so the few useful corrections are chance hits. The operator now slides **one** turbine to a free rook (or hex) neighbor—cells within 1.2 times the typical grid step. The candidate is kept only if fitness rises. Five elites receive six such tries each generation. This is hill-climbing on the elite, not a second global optimizer.

Operator rates follow the same three moves. While the record is stalling, injection and mutation rise (explore). After generation 18, twelve stall generations, and cell coverage of at least 0.35, the rates decay toward injection 0.15, mutation 2/*n*, and about 45 % selection (refine). After 25 generations in refine a short pulse raises the rates again, so a creeping maximum cannot freeze the search. The generation budget is `iteration`. Early stop, if enabled, requires no new layout, no new cell, and no new record—not a flat maximum alone.

## North-wind benchmark

The test site is a projected rectangle (EPSG:3035) with uniform wind from the north (`ws` = 12, `wd` = 0), rotor radius 30 m, and grid factor 5. Cells are 150 m; the lattice is 10 columns by 7 rows (70 cells) and holds 20 turbines. Wakes travel south, so the two edge rows do not shade each other. The known optimum occupies the southern cells 1–10 and the northern cells 61–70.

**Figure 1.** Known optimum under uniform northerly wind: the northern and southern rows of the 10 × 7 grid.

![Figure 1. Northern and southern rows occupied.](fig-north-gold.png)

Evaluated with the same `fitness()` path as the algorithm (roughness 0.3, hub 100 m, logarithmic profile, *C*<sub>p</sub> = 0.45) the layout scores 24 584.64 kW, 91.281 % efficiency, and fitness 22 441.05. A run is counted as a hit if the cell set matches or fitness reaches 99.5 % of that value. Physics are held fixed; only search options vary.

```r
library(sf)
library(windfarmGA)

Polygon1 <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4498482, 4498482, 4499991, 4499991, 4498482),
    c(2668272, 2669343, 2669343, 2668272, 2668272)
  ))),
  crs = 3035
))

genetic_algorithm(
  Polygon1 = Polygon1, n = 20, vdirspe = data.frame(ws = 12, wd = 0),
  Rotor = 30, RotorHeight = 100, iteration = 130
)
```

## Results

With a random-swap local search and an 80-generation budget, none of sixteen runs (eight configurations × seeds 1 and 2) reached the gold layout or 99.5 % of gold fitness. Typical solutions held most of both edge rows and left two to four turbines one row inland. Extra immigrants (mean ratio 0.955) and extra elites (best single ratio 0.960) were the strongest variants. High crossover injection (0.40) was the weakest (0.916): it filled the middle rows. Extending the winner, eight immigrants, to 150 generations on seed 2 reached 98.1 % (24 345 kW, 90.39 %) at generation 125 and still missed the exact set. Combining more elites with more immigrants was worse than either change alone.

The same configuration and seed with *neighborhood* local search hit gold at generation 83 (99.5 % from generation 72): exact IDs, 24 585 kW, 91.28 %. Without the neighbor slide the run remained at 98.1 %.

A 2 × 2 tune on seed 2 (130 generations, neighborhood search on) then asked how soon gold appears.

| Immigrants | Local search 3 × 2 | Local search 5 × 6 |
|---|---|---|
| 3 | generation 112 | generation **30** |
| 8 | generation 83 | generation 41 |

All four runs found the optimum. Additional neighbor tries outperformed additional immigrants; eight immigrants plus the stronger search were slower than the stronger search alone. Package defaults are therefore three immigrants and local search on five elites with six tries.

The north-wind instance has a spatially extreme, known optimum. Sites with several wind directions or irregular polygons are harder, and the gold layout is usually unknown. The tune used a single seed: the drop from generation 112 to 30 is a large effect, not a multi-start statistic. Neighborhood search corrects layouts that already sit one cell off the elite; holes in an edge row still require crossover, mutation, and immigrants.

## Conclusion

A combinatorial genome keeps every operator at exactly *n* turbines. Adaptive rates and immigrants improve the coarse search. The remaining error on the north-wind optimum is geometric—turbines one row too far inland. A random swap rarely fixes that; a neighbor slide often does. The resulting method is a memetic genetic algorithm: evolution for structure, neighborhood search for fine placement. On the documented test case it recovers the known optimum.
