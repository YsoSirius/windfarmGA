# Population size and diversity per generation

Counts evaluated individuals, distinct layouts, duplicates dropped
before the next generation, selected parents, elites, elite offspring
and grid cells (this generation and cumulative).

## Usage

``` r
population_census(result)
```

## Arguments

- result:

  The output of
  [`genetic_algorithm`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)

## Value

A data.frame with one row per generation.

## See also

Other Plotting Functions:
[`generation_layouts()`](https://YsoSirius.github.io/windfarmGA/reference/generation_layouts.md),
[`plot_cell_heatmap()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cell_heatmap.md),
[`plot_cloud()`](https://YsoSirius.github.io/windfarmGA/reference/plot_cloud.md),
[`plot_development()`](https://YsoSirius.github.io/windfarmGA/reference/plot_development.md),
[`plot_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_evolution.md),
[`plot_fitness_evolution()`](https://YsoSirius.github.io/windfarmGA/reference/plot_fitness_evolution.md),
[`plot_generation()`](https://YsoSirius.github.io/windfarmGA/reference/plot_generation.md),
[`plot_parkfitness()`](https://YsoSirius.github.io/windfarmGA/reference/plot_parkfitness.md),
[`plot_population()`](https://YsoSirius.github.io/windfarmGA/reference/plot_population.md),
[`plot_result()`](https://YsoSirius.github.io/windfarmGA/reference/plot_result.md),
[`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md),
[`plot_windrose()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windrose.md),
[`random_search_single()`](https://YsoSirius.github.io/windfarmGA/reference/random_search_single.md)

## Examples

``` r
# \donttest{
population_census(resultrect)
#>    generation evaluated distinct duplicates selected crossover mutated elites
#> 1           1       100      100          0       48        96      96      3
#> 2           2        96       96          0       46        92      92      3
#> 3           3        92       92          0       44        88      88      3
#> 4           4        88       87          1       42        84      84      3
#> 5           5        84       84          0       40        80      80      3
#> 6           6        80       80          0       38        76      76      3
#> 7           7        76       76          0       36        72      72      3
#> 8           8        72       72          0       34        68      68      3
#> 9           9        68       68          0       32        64      64      3
#> 10         10        64       64          0       30        60      60      3
#> 11         11        60       60          0       28        56      56      3
#> 12         12        56       56          0       26        52      52      3
#> 13         13        52       52          0       24        48      48      3
#> 14         14        48       48          0       22        44      44      3
#> 15         15        44       44          0       20        20      20      3
#> 16         16        20       20          0       16        32      32      3
#> 17         17        32       32          0       14        28      28      3
#> 18         18        28       28          0       12        24      24      3
#> 19         19        24       24          0       10        20      20      3
#> 20         20        20       19          1       16        32      32      3
#> 21         21        32       29          3       14        28      28      3
#> 22         22        28       27          1       12        24      24      3
#> 23         23        24       24          0       10        20      20      3
#> 24         24        20       19          1       16        32      32      3
#> 25         25        32       32          0       14        28      28      3
#> 26         26        28       28          0       12        24      24      3
#> 27         27        24       24          0       10        20      20      3
#> 28         28        20       20          0       16        32      32      3
#> 29         29        32       31          1       14        28      28      3
#> 30         30        28       27          1       12        24      24      3
#> 31         31        24       24          0       10        20      20      3
#> 32         32        20       20          0        8        16      16      3
#> 33         33        16       15          1       12        24      24      3
#> 34         34        24       24          0       10        20      20      3
#> 35         35        20       20          0       16        32      32      3
#> 36         36        32       32          0       14        28      28      3
#> 37         37        28       27          1       12        24      24      3
#> 38         38        24       24          0       10        20      20      3
#> 39         39        20       20          0       16        32      32      3
#> 40         40        32       32          0       14        28      28      3
#> 41         41        28       28          0       12        24      24      3
#> 42         42        24       24          0       10        20      20      3
#> 43         43        20       20          0       16        32      32      3
#> 44         44        32       32          0       14        28      28      3
#> 45         45        28       26          2       12        24      24      3
#> 46         46        24       23          1       10        20      20      3
#> 47         47        20       17          3       16        32      32      3
#> 48         48        32       29          3       14        28      28      3
#> 49         49        28       24          4       12        24      24      3
#> 50         50        24       19          5       10        20      20      3
#>    elite_kids cells cells_elite cells_cum dup_share
#> 1          NA    70          28        70  0.000000
#> 2          NA    70          27        70  0.000000
#> 3          NA    70          32        70  0.000000
#> 4          NA    70          33        70  1.123596
#> 5          NA    70          31        70  0.000000
#> 6          NA    70          30        70  0.000000
#> 7          NA    69          26        70  0.000000
#> 8          NA    69          31        70  0.000000
#> 9          NA    69          23        70  0.000000
#> 10         NA    69          22        70  0.000000
#> 11         NA    66          26        70  0.000000
#> 12         NA    67          25        70  0.000000
#> 13         NA    66          28        70  0.000000
#> 14         NA    67          25        70  0.000000
#> 15         NA    65          24        70  0.000000
#> 16         NA    59          29        70  0.000000
#> 17         NA    59          21        70  0.000000
#> 18         NA    53          18        70  0.000000
#> 19         NA    44          20        70  0.000000
#> 20         NA    42          15        70  4.761905
#> 21         NA    45          16        70  8.571429
#> 22         NA    43          18        70  3.448276
#> 23         NA    37          18        70  0.000000
#> 24         NA    33          20        70  4.761905
#> 25         NA    59          22        70  0.000000
#> 26         NA    53          23        70  0.000000
#> 27         NA    50          23        70  0.000000
#> 28         NA    43          21        70  0.000000
#> 29         NA    50          20        70  3.030303
#> 30         NA    48          23        70  3.448276
#> 31         NA    46          21        70  0.000000
#> 32         NA    37          24        70  0.000000
#> 33         NA    36          25        70  5.882353
#> 34         NA    39          25        70  0.000000
#> 35         NA    43          19        70  0.000000
#> 36         NA    45          21        70  0.000000
#> 37         NA    44          22        70  3.448276
#> 38         NA    46          23        70  0.000000
#> 39         NA    45          21        70  0.000000
#> 40         NA    51          23        70  0.000000
#> 41         NA    52          21        70  0.000000
#> 42         NA    52          26        70  0.000000
#> 43         NA    48          23        70  0.000000
#> 44         NA    56          24        70  0.000000
#> 45         NA    49          27        70  6.666667
#> 46         NA    44          25        70  4.000000
#> 47         NA    47          16        70 13.043478
#> 48         NA    56          16        70  8.571429
#> 49         NA    42          16        70 12.500000
#> 50         NA    38          19        70 17.241379
# }
```
