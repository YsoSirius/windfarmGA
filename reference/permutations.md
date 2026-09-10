# Enumerate the Combinations or Permutations of the Elements of a Vector

permutations enumerates the possible permutations. The function is
forked and minified from gtools::permutations

## Usage

``` r
permutations(n, r, v = 1:n)
```

## Arguments

- n:

  Size of the source vector

- r:

  Size of the target vectors

- v:

  Source vector. Defaults to 1:n

## Value

Returns a matrix where each row contains a vector of length r.

## References

Venables, Bill. "Programmers Note", R-News, Vol 1/1, Jan. 2001.
<https://cran.r-project.org/doc/Rnews/>

## See also

Other Helper Functions:
[`get_grids()`](https://YsoSirius.github.io/windfarmGA/reference/get_grids.md),
[`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md),
[`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md),
[`isSpatial()`](https://YsoSirius.github.io/windfarmGA/reference/isSpatial.md),
[`readinteger()`](https://YsoSirius.github.io/windfarmGA/reference/readinteger.md),
[`readintegerSel()`](https://YsoSirius.github.io/windfarmGA/reference/readintegerSel.md),
[`splitAt()`](https://YsoSirius.github.io/windfarmGA/reference/splitAt.md),
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)

## Author

Original versions by Bill Venables. Extended to handle repeats.allowed
by Gregory R. Warnes
