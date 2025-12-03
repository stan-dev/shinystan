# Launch the 'ShinyStan' app

Launch the 'ShinyStan' app in the default web browser. 'RStudio' users
also have the option of launching the app in the pop-up Viewer.

## Usage

``` r
launch_shinystan(object, ...)

# Default S3 method
launch_shinystan(object, ..., rstudio = getOption("shinystan.rstudio"))

# S3 method for class 'shinystan'
launch_shinystan(object, ..., rstudio = getOption("shinystan.rstudio"))
```

## Arguments

- object:

  The object to use. For the default method this can be an object of
  class `"shinystan"`, `"stanfit"`, or `"stanreg"`. To use other types
  of objects first create a shinystan object using
  [`as.shinystan`](https://mc-stan.org/shinystan/dev/reference/as.shinystan.md).

- ...:

  Optional arguments passed to
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html).

- rstudio:

  Only relevant for 'RStudio' users. The default (`FALSE`) is to launch
  the app in the user's default web browser rather than the pop-up
  Viewer provided by 'RStudio'. Users can change the default to `TRUE`
  by setting the global option `options(shinystan.rstudio = TRUE)`.

## Value

The `launch_shinystan` function is used for the side effect of starting
the 'ShinyStan' app, but it also returns a `shinystan` object, an
instance of S4 class `"shinystan"`.

## References

Muth, C., Oravecz, Z., and Gabry, J. (2018) User-friendly Bayesian
regression modeling: A tutorial with rstanarm and shinystan. *The
Quantitative Methods for Psychology*. 14(2), 99–119.
<https://www.tqmp.org/RegularArticles/vol14-2/p099/p099.pdf>

Gabry, J. , Simpson, D. , Vehtari, A. , Betancourt, M. and Gelman, A.
(2019), Visualization in Bayesian workflow. \*J. R. Stat. Soc. A\*, 182:
389-402. doi:10.1111/rssa.12378 (\[journal
version\](https://rss.onlinelibrary.wiley.com/doi/full/10.1111/rssa.12378),
\[preprint arXiv:1709.01449\](https://arxiv.org/abs/1709.01449), \[code
on GitHub\](https://github.com/jgabry/bayes-vis-paper))

## See also

[`as.shinystan`](https://mc-stan.org/shinystan/dev/reference/as.shinystan.md)
for creating `shinystan` objects.

[`update_sso`](https://mc-stan.org/shinystan/dev/reference/update_sso.md)
to update a `shinystan` object created by a previous version of the
package.

[`launch_shinystan_demo`](https://mc-stan.org/shinystan/dev/reference/launch_shinystan_demo.md)
to try a demo.

## Examples

``` r
if (FALSE) { # \dontrun{
#######################################
# Example 1: 'sso' is a shinystan object
#######################################

# Just launch shinystan
launch_shinystan(sso)

# Launch shinystan and replace sso with an updated version of itself
# if any changes are made to sso while using the app
sso <- launch_shinystan(sso)

# Launch shinystan but save any changes made to sso while running the app
# in a new shinystan object sso2. sso will remained unchanged. 
sso2 <- launch_shinystan(sso) 

#######################################
# Example 2: 'sf' is a stanfit object
#######################################

# Just launch shinystan
launch_shinystan(sf)

# Launch shinystan and save the resulting shinystan object
sf_sso <- launch_shinystan(sf)

# Now sf_sso is a shinystan object and so Example 1 (above) applies when
# using sf_sso. 

#######################################
# Example 3: 'fit' is an mcmc.list, array or list of matrices
#######################################

# First create shinystan object (see ?as.shinystan) for full details)
} # }
```
