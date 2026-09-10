# Calculates Air Density, Air Pressure and Temperature according to the Barometric Height Formula

Calculates air density, temperature and air pressure respective to
certain heights according to the International standard atmosphere and
the barometric height formula.

## Usage

``` r
barometric_height(data, height, po = 101325, ro = 1.225)
```

## Arguments

- data:

  A data.frame containing the height values

- height:

  Column name of the height values

- po:

  Standardized air pressure at sea level (101325 Pa)

- ro:

  Standardized air density at sea level (1,225 kg per m3)

## Value

Returns a data.frame with height values and corresponding air pressures,
air densities and temperatures in Kelvin and Celsius.

## See also

Other Wind Energy Calculation Functions:
[`calculate_energy()`](https://YsoSirius.github.io/windfarmGA/reference/calculate_energy.md),
[`circle_intersection()`](https://YsoSirius.github.io/windfarmGA/reference/circle_intersection.md),
[`get_dist_angles()`](https://YsoSirius.github.io/windfarmGA/reference/get_dist_angles.md),
[`turbine_influences()`](https://YsoSirius.github.io/windfarmGA/reference/turbine_influences.md)

## Examples

``` r
data <- matrix(seq(0, 5000, 500))
barometric_height(data)
#>    Height        ph        rh  tempK  tempC
#> 1       0 101325.00 1.2250000 288.15  15.00
#> 2     500  95176.51 1.1506659 284.90  11.75
#> 3    1000  89401.12 1.0808425 281.65   8.50
#> 4    1500  83976.18 1.0152560 278.40   5.25
#> 5    2000  78880.43 0.9536494 275.15   2.00
#> 6    2500  74093.90 0.8957811 271.90  -1.25
#> 7    3000  69597.81 0.8414244 268.65  -4.50
#> 8    3500  65374.56 0.7903660 265.40  -7.75
#> 9    4000  61407.57 0.7424059 262.15 -11.00
#> 10   4500  57681.31 0.6973561 258.90 -14.25
#> 11   5000  54181.16 0.6550399 255.65 -17.50
plot.ts(barometric_height(data))
```
