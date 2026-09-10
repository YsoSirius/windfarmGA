# Split matrices or numeric vectors at specific indices

The function is used by the crossover method to split a genetic code at
certain intervals. See also
[`crossover`](https://YsoSirius.github.io/windfarmGA/reference/crossover.md).

## Usage

``` r
splitAt(x, pos)
```

## Arguments

- x:

  A numeric variable that represents an individual's binary genetic code

- pos:

  A numeric value that indicates where to split the genetic code

## Value

Returns a list of the split genetic code.

## See also

Other Helper Functions:
[`get_grids()`](https://YsoSirius.github.io/windfarmGA/reference/get_grids.md),
[`grid_area()`](https://YsoSirius.github.io/windfarmGA/reference/grid_area.md),
[`hexa_area()`](https://YsoSirius.github.io/windfarmGA/reference/hexa_area.md),
[`isSpatial()`](https://YsoSirius.github.io/windfarmGA/reference/isSpatial.md),
[`permutations()`](https://YsoSirius.github.io/windfarmGA/reference/permutations.md),
[`readinteger()`](https://YsoSirius.github.io/windfarmGA/reference/readinteger.md),
[`readintegerSel()`](https://YsoSirius.github.io/windfarmGA/reference/readintegerSel.md),
[`windata_format()`](https://YsoSirius.github.io/windfarmGA/reference/windata_format.md)

## Examples

``` r
splitAt(1:100, 20)
#> [[1]]
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19
#> 
#> [[2]]
#>  [1]  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38
#> [20]  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57
#> [39]  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76
#> [58]  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
#> [77]  96  97  98  99 100
#> 
splitAt(as.matrix(1:100), 20)
#> [[1]]
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19
#> 
#> [[2]]
#>  [1]  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38
#> [20]  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57
#> [39]  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76
#> [58]  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
#> [77]  96  97  98  99 100
#> 
```
