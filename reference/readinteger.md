# Check Input Crossover Method

Checks whether the input for
[`crossover`](https://YsoSirius.github.io/windfarmGA/reference/crossover.md)
is given correctly. If not, a message is prompted which asks to input
one of the 2 available crossover methods. The available inputs are "E"
and "R". "E" refers to partitioning at equal intervals and "R" refers to
random partitioning.

## Usage

``` r
readinteger()
```

## Value

Returns the selected crossover method (character)

## See also

Other Helper Functions:
[`get_grids()`](https://YsoSirius.github.io/windfarmGA/reference/get_grids.md),
[`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md),
[`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md),
[`isSpatial()`](https://YsoSirius.github.io/windfarmGA/reference/isSpatial.md),
[`permutations()`](https://YsoSirius.github.io/windfarmGA/reference/permutations.md),
[`readintegerSel()`](https://YsoSirius.github.io/windfarmGA/reference/readintegerSel.md),
[`splitAt()`](https://YsoSirius.github.io/windfarmGA/reference/splitAt.md),
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)
