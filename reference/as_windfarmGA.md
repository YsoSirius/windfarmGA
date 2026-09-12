# Mark a genetic_algorithm result

Attach the `windfarmGA` class so
[`print()`](https://rdrr.io/r/base/print.html) and
[`plot()`](https://r-spatial.github.io/sf/reference/plot.html) dispatch.
New runs from
[`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)
already have this class. [`print()`](https://rdrr.io/r/base/print.html)
shows run inputs, the wind table, and each generation that set a new
fitness maximum.

## Usage

``` r
as_windfarmGA(x)

# S3 method for class 'windfarmGA'
print(x, ...)

# S3 method for class 'windfarmGA'
plot(x, y, ...)
```

## Arguments

- x:

  A result matrix from
  [`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md).

- ...:

  Passed to
  [`plot_windfarmGA()`](https://YsoSirius.github.io/windfarmGA/reference/plot_windfarmGA.md).

- y:

  The site polygon (same as `area` in
  [`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)).

## Value

`x` with class `windfarmGA`.
