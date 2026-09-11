# Get or set windfarmGA options

Print every `windfarmGA.*` session option, or set some of them. Names
may be given with or without the `windfarmGA.` prefix.

## Usage

``` r
ga_options(...)
```

## Arguments

- ...:

  Named options to set, or a single named list. With no arguments, print
  the current values and return them invisibly. Setting is silent.

## Value

A named list of `windfarmGA.*` options, invisibly.

## See also

[`options()`](https://rdrr.io/r/base/options.html)

## Examples

``` r
ga_options()
#>  option                    value
#>  Cp                        0.45 
#>  air_rh                    1.225
#>  cT                        0.88 
#>  connection                0    
#>  crossover_inject          0.25 
#>  cut_in                    0    
#>  cut_out                   Inf  
#>  elite_children            3    
#>  elite_mix                 2    
#>  explore_pulse             10   
#>  fitness_efficiency_weight 1    
#>  immigrants                3    
#>  k                         0.075
#>  local_search_elites       5    
#>  local_search_tries        6    
#>  max_angle                 20   
#>  max_distance              1e+05
#>  max_population            300  
#>  max_selection             300  
#>  min_swaps                 1    
#>  rated_ws                  Inf  
#>  refine_after              12   
#>  refine_hold               25   
#>  refine_min_gen            18   
#>  spatial_crossover         0.5  
#>  stall_generations         40   
#>  wind_profile              log  
ga_options(immigrants = 3, local_search_tries = 6)
```
