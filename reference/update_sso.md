# Update an object created by the previous version of shinystan

If you encounter any errors when using a shinystan object (`sso`)
created by a previous version of shinystan, you might need to run
`update_sso`. If `update_sso` does not resolve the problem and you still
have the object (e.g. stanfit, stanreg, mcmc.list) from which `sso` was
originally created, you can create a new shinystan object using
[`as.shinystan`](https://mc-stan.org/shinystan/reference/as.shinystan.md).

## Usage

``` r
update_sso(sso)
```

## Arguments

- sso:

  A
  [`shinystan object`](https://mc-stan.org/shinystan/reference/as.shinystan.md).

## Value

If `sso` is already compatible with your version of shinystan then `sso`
itself is returned and a message is printed indicating that `sso` is
already up-to-date. Otherwise an updated version of `sso` is returned
unless an error is encountered.

## See also

[`as.shinystan`](https://mc-stan.org/shinystan/reference/as.shinystan.md)
for creating `shinystan` objects.

## Examples

``` r
if (FALSE) { # \dontrun{
sso_new <- update_sso(sso)
} # }
```
