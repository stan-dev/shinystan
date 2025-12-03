# Deploy a 'ShinyStan' app on the web using 'shinyapps.io' by 'RStudio'

Requires a (free or paid) 'ShinyApps' account. Visit
<https://www.shinyapps.io/> to sign up.

## Usage

``` r
deploy_shinystan(sso, appName, account = NULL, ..., deploy = TRUE)
```

## Arguments

- sso:

  A
  [`shinystan object`](https://mc-stan.org/shinystan/dev/reference/as.shinystan.md).

- appName:

  The name to use for the application. Application names must be at
  least four characters long and may only contain letters, numbers,
  dashes and underscores.

- account:

  shinyapps.io account username. Only required if more than one account
  is configured on the system.

- ...:

  Optional arguments. See Details.

- deploy:

  Should the app be deployed? The only reason for this to be `FALSE` is
  if you just want to check that the preprocessing before deployment is
  successful.

## Value

[Invisibly](https://rdrr.io/r/base/invisible.html), `TRUE` if deployment
succeeded (did not encounter an error) or, if `deploy` argument is set
to `FALSE`, the path to the temporary directory containing the app ready
for deployment (also invisibly).

## Details

In `...`, the arguments `ppcheck_data` and `ppcheck_yrep` can be
specified. `ppcheck_data` should be a vector of observations to use for
graphical posterior predictive checking and `ppcheck_yrep` should be a
character string naming the parameter in `sso` containing the posterior
predictive simulations/replications. The value of `ppcheck_yrep` is only
used to preselect the appropriate parameter/generated quantity to use
for the posterior predictive checking. `ppcheck_yrep` (but not
`ppcheck_data`) can also be set interactively on shinyapps.io when using
the app.

## See also

The example in the *Deploying to shinyapps.io* vignette that comes with
this package.

<https://www.shinyapps.io/> to sign up for a free or paid 'ShinyApps'
account and for details on how to configure your account on your local
system using the rsconnect package from 'RStudio'.

## Examples

``` r
if (FALSE) { # \dontrun{
# For this example assume sso is the name of the \code{shinystan} object for
# the model you want to use. Assume also that you want to name your app
# 'my-model' and that your shinyapps.io username is 'username'.

deploy_shinystan(sso, appName = "my-model", account = "username")

# If you only have one ShinyApps account configured then you can also omit
# the 'account' argument.

deploy_shinystan(sso, appName = "my-model")
} # }
```
