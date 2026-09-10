# Mutation Method

The function randomly mutates an individual's genetic code

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
#>  [1,]    0    1    1    0
#>  [2,]    0    1    0    1
#>  [3,]    1    1    0    1
#>  [4,]    1    1    1    1
#>  [5,]    1    1    1    1
#>  [6,]    0    1    0    1
#>  [7,]    1    0    1    1
#>  [8,]    0    0    1    1
#>  [9,]    0    0    0    0
#> [10,]    0    1    1    1
#> [11,]    0    1    1    1
#> [12,]    0    1    0    0
#> [13,]    0    1    0    1
#> [14,]    0    1    1    0
#> [15,]    0    1    1    1
#> [16,]    0    0    1    1
#> [17,]    0    1    1    1
#> [18,]    0    1    0    1
#> [19,]    0    1    1    1
#> [20,]    0    1    0    1

## Mutate the individuals with a low percentage
aMut <- mutation(a, 0.1, NULL)
## Check which values are not like the originals
a == aMut
#>        bin0  bin1 bin2 bin3
#>  [1,]  TRUE  TRUE TRUE TRUE
#>  [2,]  TRUE  TRUE TRUE TRUE
#>  [3,]  TRUE  TRUE TRUE TRUE
#>  [4,]  TRUE  TRUE TRUE TRUE
#>  [5,] FALSE  TRUE TRUE TRUE
#>  [6,]  TRUE  TRUE TRUE TRUE
#>  [7,]  TRUE  TRUE TRUE TRUE
#>  [8,]  TRUE  TRUE TRUE TRUE
#>  [9,]  TRUE FALSE TRUE TRUE
#> [10,] FALSE  TRUE TRUE TRUE
#> [11,]  TRUE  TRUE TRUE TRUE
#> [12,]  TRUE  TRUE TRUE TRUE
#> [13,]  TRUE  TRUE TRUE TRUE
#> [14,]  TRUE  TRUE TRUE TRUE
#> [15,]  TRUE  TRUE TRUE TRUE
#> [16,]  TRUE FALSE TRUE TRUE
#> [17,]  TRUE  TRUE TRUE TRUE
#> [18,]  TRUE  TRUE TRUE TRUE
#> [19,]  TRUE FALSE TRUE TRUE
#> [20,]  TRUE  TRUE TRUE TRUE

## Mutate the individuals with a high percentage
aMut <- mutation(a, 0.4, NULL)
## Check which values are not like the originals
a == aMut
#>        bin0  bin1  bin2  bin3
#>  [1,]  TRUE  TRUE  TRUE FALSE
#>  [2,] FALSE FALSE FALSE  TRUE
#>  [3,]  TRUE FALSE FALSE  TRUE
#>  [4,] FALSE FALSE  TRUE  TRUE
#>  [5,]  TRUE FALSE FALSE FALSE
#>  [6,]  TRUE FALSE  TRUE  TRUE
#>  [7,]  TRUE FALSE FALSE FALSE
#>  [8,] FALSE  TRUE FALSE FALSE
#>  [9,] FALSE  TRUE FALSE  TRUE
#> [10,] FALSE  TRUE  TRUE  TRUE
#> [11,]  TRUE  TRUE FALSE FALSE
#> [12,] FALSE  TRUE FALSE FALSE
#> [13,]  TRUE FALSE FALSE FALSE
#> [14,] FALSE  TRUE  TRUE  TRUE
#> [15,]  TRUE FALSE  TRUE FALSE
#> [16,] FALSE FALSE FALSE  TRUE
#> [17,] FALSE  TRUE  TRUE  TRUE
#> [18,]  TRUE FALSE  TRUE FALSE
#> [19,]  TRUE FALSE  TRUE FALSE
#> [20,]  TRUE  TRUE  TRUE  TRUE
```
