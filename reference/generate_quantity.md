# Add new quantity to shinystan object

Add to shinystan object a new parameter as a function of one or two
existing parameters.

## Usage

``` r
generate_quantity(sso, param1, param2, fun, new_name)
```

## Arguments

- sso:

  A
  [`shinystan object`](https://mc-stan.org/shinystan/reference/as.shinystan.md).

- param1:

  Name of first parameter as character string.

- param2:

  Optional. Name of second parameter as character string.

- fun:

  Function to call, i.e. `function(param1)` or
  `function(param1,param2)`. See Examples, below.

- new_name:

  Name for the new parameter as character string.

## Value

sso, updated. See Examples.

## See also

[`drop_parameters`](https://mc-stan.org/shinystan/reference/drop_parameters.md)
to remove parameters from a `shinystan` object.

## Examples

``` r
# Using example shinystan object 'eight_schools'
sso <- eight_schools
sso <- generate_quantity(sso, fun = function(x) x^2, 
                         param1 = "tau", new_name = "tau_sq")
sso <- generate_quantity(sso, fun = "-", 
                         param1 = "theta[1]", param2 = "theta[2]", 
                         new_name = "theta1minus2")
                         
```
