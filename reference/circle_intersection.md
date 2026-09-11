# Get area of intersecting circles

Calculate the intersection area of two circles with different radii and
different heights

## Usage

``` r
circle_intersection(r1, r2, h1, h2, dx)
```

## Arguments

- r1:

  The radius of circle 1

- r2:

  The radius of circle 2

- h1:

  The height of the circle center 1

- h2:

  The height of the circle center 2

- dx:

  The distance on the x-axis between both centers

## Value

A numeric vector; one intersection area per pair. Scalars stay length 1.

## See also

Other Wind Energy Calculation Functions:
[`barometric_height()`](https://YsoSirius.github.io/windfarmGA/reference/barometric_height.md),
[`calculate_energy()`](https://YsoSirius.github.io/windfarmGA/reference/calculate_energy.md),
[`get_dist_angles()`](https://YsoSirius.github.io/windfarmGA/reference/get_dist_angles.md),
[`turbine_influences()`](https://YsoSirius.github.io/windfarmGA/reference/turbine_influences.md)
