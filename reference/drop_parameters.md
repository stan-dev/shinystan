# Drop parameters from a `shinystan` object

Remove selected parameters from a `shinystan` object. This is useful if
you have a very large `shinystan` object when you only want to look at a
subset of parameters. With a smaller `shinystan` object,
[`launch_shinystan`](https://mc-stan.org/shinystan/reference/launch_shinystan.md)
will be faster and you should experience better performance
(responsiveness) after launching when using the 'ShinyStan' app.

## Usage

``` r
drop_parameters(sso, pars)
```

## Arguments

- sso:

  A
  [`shinystan object`](https://mc-stan.org/shinystan/reference/as.shinystan.md).

- pars:

  A character vector of parameter names. If the name of a non-scalar
  (e.g. vector, matrix) parameter is included in `pars` all of its
  elements will be removed. Currently it is not possible to remove only
  a subset of the elements of a non-scalar parameter.

## Value

`sso`, with `pars` dropped.

## See also

[`generate_quantity`](https://mc-stan.org/shinystan/reference/generate_quantity.md)
to add a new quantity to a `shinystan` object.

## Examples

``` r
# Using example shinystan object 'eight_schools'
print(eight_schools@param_names)

# Remove the scalar parameters mu and tau
sso <- drop_parameters(eight_schools, pars = c("mu", "tau"))
print(sso@param_names)

# Remove all elements of the parameter vector theta
sso <- drop_parameters(sso, pars = "theta")
print(sso@param_names)
```
