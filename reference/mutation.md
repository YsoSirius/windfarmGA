# Mutation Method

Legacy bit-flip mutation on 0/1 chromosomes. The GA loop uses
[`swap_mutation`](https://YsoSirius.github.io/windfarmGA/reference/swap_mutation.md)
so that every individual keeps exactly `n` turbines.

## Usage

``` r
mutation(a, p, seed = NULL)
```

## Arguments

- a:

  The binary matrix of all individuals

- p:

  The mutation rate

- seed:

  Set a seed for comparability. Default is `NULL`

## Value

Returns a binary matrix with mutated genes.

## See also

Other Genetic Algorithm Functions:
[`crossover()`](https://YsoSirius.github.io/windfarmGA/reference/crossover.md),
[`fitness()`](https://YsoSirius.github.io/windfarmGA/reference/fitness.md),
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md),
[`init_population()`](https://YsoSirius.github.io/windfarmGA/reference/init_population.md),
[`selection()`](https://YsoSirius.github.io/windfarmGA/reference/selection.md),
[`set_crossover()`](https://YsoSirius.github.io/windfarmGA/reference/set_crossover.md),
[`swap_mutation()`](https://YsoSirius.github.io/windfarmGA/reference/swap_mutation.md),
[`trimton()`](https://YsoSirius.github.io/windfarmGA/reference/trimton.md)

## Examples

``` r
## Create 4 random individuals with binary values
a <- cbind(
  bin0 = sample(c(0, 1), 20, replace = TRUE, prob = c(70, 30)),
  bin1 = sample(c(0, 1), 20, replace = TRUE, prob = c(30, 70)),
  bin2 = sample(c(0, 1), 20, replace = TRUE, prob = c(30, 70)),
  bin3 = sample(c(0, 1), 20, replace = TRUE, prob = c(30, 70))
)
a
#>       bin0 bin1 bin2 bin3
#>  [1,]    0    1    1    1
#>  [2,]    0    0    1    1
#>  [3,]    1    1    1    1
#>  [4,]    1    1    0    0
#>  [5,]    0    1    1    1
#>  [6,]    0    0    0    0
#>  [7,]    0    0    1    0
#>  [8,]    0    0    1    0
#>  [9,]    1    0    1    1
#> [10,]    0    1    0    0
#> [11,]    0    1    1    1
#> [12,]    1    0    1    0
#> [13,]    0    1    1    1
#> [14,]    0    1    0    1
#> [15,]    0    1    1    1
#> [16,]    0    0    1    1
#> [17,]    0    1    0    0
#> [18,]    0    1    1    1
#> [19,]    0    0    1    1
#> [20,]    0    1    1    1

## Mutate the individuals with a low percentage
aMut <- mutation(a, 0.1, NULL)
## Check which values are not like the originals
a == aMut
#>        bin0  bin1  bin2  bin3
#>  [1,]  TRUE  TRUE  TRUE FALSE
#>  [2,]  TRUE  TRUE  TRUE  TRUE
#>  [3,] FALSE FALSE  TRUE  TRUE
#>  [4,]  TRUE  TRUE  TRUE  TRUE
#>  [5,]  TRUE  TRUE  TRUE FALSE
#>  [6,]  TRUE  TRUE  TRUE  TRUE
#>  [7,]  TRUE  TRUE  TRUE  TRUE
#>  [8,]  TRUE  TRUE  TRUE  TRUE
#>  [9,]  TRUE  TRUE  TRUE  TRUE
#> [10,]  TRUE  TRUE  TRUE  TRUE
#> [11,]  TRUE FALSE  TRUE  TRUE
#> [12,] FALSE  TRUE  TRUE  TRUE
#> [13,]  TRUE FALSE  TRUE  TRUE
#> [14,]  TRUE  TRUE  TRUE  TRUE
#> [15,]  TRUE  TRUE FALSE  TRUE
#> [16,]  TRUE  TRUE  TRUE  TRUE
#> [17,] FALSE FALSE FALSE  TRUE
#> [18,]  TRUE  TRUE  TRUE  TRUE
#> [19,]  TRUE  TRUE  TRUE  TRUE
#> [20,] FALSE  TRUE  TRUE  TRUE

## Mutate the individuals with a high percentage
aMut <- mutation(a, 0.4, NULL)
## Check which values are not like the originals
a == aMut
#>        bin0  bin1  bin2  bin3
#>  [1,]  TRUE  TRUE FALSE FALSE
#>  [2,]  TRUE  TRUE  TRUE  TRUE
#>  [3,] FALSE  TRUE  TRUE  TRUE
#>  [4,]  TRUE  TRUE  TRUE  TRUE
#>  [5,] FALSE  TRUE FALSE  TRUE
#>  [6,]  TRUE  TRUE FALSE FALSE
#>  [7,]  TRUE  TRUE  TRUE FALSE
#>  [8,] FALSE  TRUE FALSE FALSE
#>  [9,]  TRUE  TRUE  TRUE FALSE
#> [10,]  TRUE  TRUE  TRUE  TRUE
#> [11,]  TRUE  TRUE  TRUE FALSE
#> [12,] FALSE FALSE FALSE  TRUE
#> [13,]  TRUE  TRUE FALSE  TRUE
#> [14,] FALSE  TRUE  TRUE  TRUE
#> [15,] FALSE FALSE FALSE FALSE
#> [16,]  TRUE FALSE  TRUE  TRUE
#> [17,]  TRUE FALSE  TRUE FALSE
#> [18,]  TRUE  TRUE  TRUE  TRUE
#> [19,]  TRUE FALSE  TRUE  TRUE
#> [20,] FALSE  TRUE  TRUE  TRUE
```
