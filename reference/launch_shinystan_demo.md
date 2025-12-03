# 'ShinyStan' demo

'ShinyStan' demo

## Usage

``` r
launch_shinystan_demo(
  demo_name = "eight_schools",
  rstudio = getOption("shinystan.rstudio"),
  ...
)
```

## Arguments

- demo_name:

  The name of the demo. Currently `"eight_schools"` is the only option,
  but additional demos may be available in future releases.

  `eight_schools`

  :   Hierarchical meta-analysis model. See *Meta Analysis* chapter of
      the 'Stan' manual <https://mc-stan.org/users/documentation/>.

- rstudio:

  Only relevant for 'RStudio' users. The default (`FALSE`) is to launch
  the app in the user's default web browser rather than the pop-up
  Viewer provided by 'RStudio'. Users can change the default to `TRUE`
  by setting the global option `options(shinystan.rstudio = TRUE)`.

- ...:

  Optional arguments passed to
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

An S4 shinystan object.

## See also

[`launch_shinystan`](https://mc-stan.org/shinystan/reference/launch_shinystan.md)
to launch the 'ShinyStan' interface using a particular `shinystan`
object.

[`as.shinystan`](https://mc-stan.org/shinystan/reference/as.shinystan.md)
for creating `shinystan` objects.

## Examples

``` r
if (FALSE) { # \dontrun{
# launch demo but don't save a shinystan object
launch_shinystan_demo() 

# launch demo and save the shinystan object for the demo 
sso_demo <- launch_shinystan_demo()
} # }
```
