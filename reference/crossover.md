# Crossover Method

The crossover method creates new offspring with the selected individuals
by permutating their genetic codes.

## Usage

``` r
crossover(se6, u, uplimit, crossPart = c("EQU", "RAN"), verbose, seed)
```

## Arguments

- se6:

  The selected individuals. The output of
  [`selection`](https://YsoSirius.github.io/windfarmGA/reference/selection.md)

- u:

  The crossover point rate

- uplimit:

  The upper limit of allowed permutations

- crossPart:

  The crossover method. Either "EQU" or "RAN"

- verbose:

  If `TRUE`, will print out further information

- seed:

  Set a seed for comparability. Default is `NULL`

## Value

Returns a binary coded matrix of all permutations and all grid cells,
where 0 indicates no turbine and 1 indicates a turbine in the grid cell.

## See also

Other Genetic Algorithm Functions:
[`fitness()`](https://YsoSirius.github.io/windfarmGA/reference/fitness.md),
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md),
[`init_population()`](https://YsoSirius.github.io/windfarmGA/reference/init_population.md),
[`mutation()`](https://YsoSirius.github.io/windfarmGA/reference/mutation.md),
[`selection()`](https://YsoSirius.github.io/windfarmGA/reference/selection.md),
[`trimton()`](https://YsoSirius.github.io/windfarmGA/reference/trimton.md)

## Examples

``` r
## Create two random parents with an index and random binary values
Parents <- data.frame(
  ID = 1:20,
  bin = sample(c(0, 1), 20, replace = TRUE, prob = c(70, 30)),
  bin.1 = sample(c(0, 1), 20, replace = TRUE, prob = c(30, 70))
)

## Create random Fitness values for both individuals
FitParents <- data.frame(ID = 1, Fitness = 1000, Fitness.1 = 20)

## Assign both values to a list
CrossSampl <- list(Parents, FitParents)
## Cross their data at equal locations with 2 crossover parts
crossover(CrossSampl, u = 1.1, uplimit = 300, crossPart = "EQU")
#>       [,1] [,2] [,3] [,4]
#>  [1,]    1    1    0    0
#>  [2,]    1    1    0    0
#>  [3,]    1    1    0    0
#>  [4,]    1    1    0    0
#>  [5,]    1    1    0    0
#>  [6,]    0    0    1    1
#>  [7,]    0    0    1    1
#>  [8,]    1    1    0    0
#>  [9,]    1    1    0    0
#> [10,]    0    0    0    0
#> [11,]    0    0    0    0
#> [12,]    0    0    0    0
#> [13,]    1    0    1    0
#> [14,]    1    1    1    1
#> [15,]    1    0    1    0
#> [16,]    0    0    0    0
#> [17,]    0    1    0    1
#> [18,]    0    1    0    1
#> [19,]    1    1    1    1
#> [20,]    1    0    1    0

## with 3 crossover parts and equal locations
crossover(CrossSampl, u = 2.5, uplimit = 300, crossPart = "EQU")
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#>  [1,]    1    1    1    1    0    0    0    0
#>  [2,]    1    1    1    1    0    0    0    0
#>  [3,]    1    1    1    1    0    0    0    0
#>  [4,]    1    1    1    1    0    0    0    0
#>  [5,]    1    1    1    1    0    0    0    0
#>  [6,]    0    0    0    0    1    1    1    1
#>  [7,]    0    0    0    0    1    1    1    1
#>  [8,]    1    1    0    0    1    1    0    0
#>  [9,]    1    1    0    0    1    1    0    0
#> [10,]    0    0    0    0    0    0    0    0
#> [11,]    0    0    0    0    0    0    0    0
#> [12,]    0    0    0    0    0    0    0    0
#> [13,]    1    1    0    0    1    1    0    0
#> [14,]    1    1    1    1    1    1    1    1
#> [15,]    1    0    1    0    1    0    1    0
#> [16,]    0    0    0    0    0    0    0    0
#> [17,]    0    1    0    1    0    1    0    1
#> [18,]    0    1    0    1    0    1    0    1
#> [19,]    1    1    1    1    1    1    1    1
#> [20,]    1    0    1    0    1    0    1    0

## or with random locations and 5 crossover parts
crossover(CrossSampl, u = 4.9, uplimit = 300, crossPart = "RAN")
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#>  [1,]    1    1    1    1    1    1    1    1    1     1     1     1     1
#>  [2,]    1    1    1    1    1    1    1    1    1     1     1     1     1
#>  [3,]    1    1    1    1    1    1    1    1    1     1     1     1     1
#>  [4,]    1    1    1    1    1    1    1    1    1     1     1     1     1
#>  [5,]    1    1    1    1    1    1    1    1    1     1     1     1     1
#>  [6,]    0    0    0    0    0    0    0    0    0     0     0     0     0
#>  [7,]    0    0    0    0    0    0    0    0    0     0     0     0     0
#>  [8,]    1    1    1    1    1    1    1    1    1     1     1     1     1
#>  [9,]    1    1    1    1    1    1    1    1    0     0     0     0     0
#> [10,]    0    0    0    0    0    0    0    0    0     0     0     0     0
#> [11,]    0    0    0    0    0    0    0    0    0     0     0     0     0
#> [12,]    0    0    0    0    0    0    0    0    0     0     0     0     0
#> [13,]    1    1    1    1    1    1    1    1    0     0     0     0     0
#> [14,]    1    1    1    1    1    1    1    1    1     1     1     1     1
#> [15,]    1    1    1    1    0    0    0    0    1     1     1     1     0
#> [16,]    0    0    0    0    0    0    0    0    0     0     0     0     0
#> [17,]    0    0    1    1    0    0    1    1    0     0     1     1     0
#> [18,]    0    1    0    1    0    1    0    1    0     1     0     1     0
#> [19,]    1    1    1    1    1    1    1    1    1     1     1     1     1
#> [20,]    1    0    1    0    1    0    1    0    1     0     1     0     1
#>       [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25]
#>  [1,]     1     1     1     0     0     0     0     0     0     0     0     0
#>  [2,]     1     1     1     0     0     0     0     0     0     0     0     0
#>  [3,]     1     1     1     0     0     0     0     0     0     0     0     0
#>  [4,]     1     1     1     0     0     0     0     0     0     0     0     0
#>  [5,]     1     1     1     0     0     0     0     0     0     0     0     0
#>  [6,]     0     0     0     1     1     1     1     1     1     1     1     1
#>  [7,]     0     0     0     1     1     1     1     1     1     1     1     1
#>  [8,]     1     1     1     0     0     0     0     0     0     0     0     0
#>  [9,]     0     0     0     1     1     1     1     1     1     1     1     0
#> [10,]     0     0     0     0     0     0     0     0     0     0     0     0
#> [11,]     0     0     0     0     0     0     0     0     0     0     0     0
#> [12,]     0     0     0     0     0     0     0     0     0     0     0     0
#> [13,]     0     0     0     1     1     1     1     1     1     1     1     0
#> [14,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [15,]     0     0     0     1     1     1     1     0     0     0     0     1
#> [16,]     0     0     0     0     0     0     0     0     0     0     0     0
#> [17,]     0     1     1     0     0     1     1     0     0     1     1     0
#> [18,]     1     0     1     0     1     0     1     0     1     0     1     0
#> [19,]     1     1     1     1     1     1     1     1     1     1     1     1
#> [20,]     0     1     0     1     0     1     0     1     0     1     0     1
#>       [,26] [,27] [,28] [,29] [,30] [,31] [,32]
#>  [1,]     0     0     0     0     0     0     0
#>  [2,]     0     0     0     0     0     0     0
#>  [3,]     0     0     0     0     0     0     0
#>  [4,]     0     0     0     0     0     0     0
#>  [5,]     0     0     0     0     0     0     0
#>  [6,]     1     1     1     1     1     1     1
#>  [7,]     1     1     1     1     1     1     1
#>  [8,]     0     0     0     0     0     0     0
#>  [9,]     0     0     0     0     0     0     0
#> [10,]     0     0     0     0     0     0     0
#> [11,]     0     0     0     0     0     0     0
#> [12,]     0     0     0     0     0     0     0
#> [13,]     0     0     0     0     0     0     0
#> [14,]     1     1     1     1     1     1     1
#> [15,]     1     1     1     0     0     0     0
#> [16,]     0     0     0     0     0     0     0
#> [17,]     0     1     1     0     0     1     1
#> [18,]     1     0     1     0     1     0     1
#> [19,]     1     1     1     1     1     1     1
#> [20,]     0     1     0     1     0     1     0
```
