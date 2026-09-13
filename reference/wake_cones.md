# Downwind wake search cones

Build sf polygons for the Jensen search cone of each turbine and wind
direction. Colour can encode wake loss (`AbschGesamt`).

## Usage

``` r
wake_cones(xy, wind, rotor, area, half_deg = NULL, colors = NULL)
```

## Arguments

- xy:

  Matrix or data.frame of turbine X/Y in the site CRS.

- wind:

  Wind table with `wd` (and optional `probab`), as stored in a
  [`genetic_algorithm()`](https://YsoSirius.github.io/windfarmGA/reference/genetic_algorithm.md)
  result.

- rotor:

  Rotor radius in metres (sets cone length together with `area`).

- area:

  Site polygon (`sf`), projected in metres.

- half_deg:

  Half search angle. Default `windfarmGA.max_angle`.

- colors:

  Optional colour per turbine (recycled).

## Value

An `sf` polygon layer with `turb`, `wd`, `prob`, `farbe`.
