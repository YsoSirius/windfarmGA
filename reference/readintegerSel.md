# Check Input Selection Method

Checks whether the input for
[`selection`](https://YsoSirius.github.io/windfarmGA/reference/selection.md)
is given correctly. If not, a message is prompted which asks to input
one of the 2 available selection methods. The available inputs are "F"
and "V". "F" refers to a fixed percentage of 50% and "V" refers to a
variable percentage, based on the development of the population fitness
values.

## Usage

``` r
readintegerSel()
```

## Value

Returns the selected selection method (character)

## See also

Other Helper Functions:
[`get_grids()`](https://YsoSirius.github.io/windfarmGA/reference/get_grids.md),
[`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md),
[`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md),
[`isSpatial()`](https://YsoSirius.github.io/windfarmGA/reference/isSpatial.md),
[`permutations()`](https://YsoSirius.github.io/windfarmGA/reference/permutations.md),
[`readinteger()`](https://YsoSirius.github.io/windfarmGA/reference/readinteger.md),
[`splitAt()`](https://YsoSirius.github.io/windfarmGA/reference/splitAt.md),
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)
