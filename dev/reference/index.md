# Package index

## Package overview

Details about the shinystan R package.

- [`shinystan-package`](https://mc-stan.org/shinystan/dev/reference/shinystan-package.md)
  :

  shinystan R package ('ShinyStan' graphical user interface)

## Launching the app

Functions for launching the app using a particular model or launching a
demo of the app.

- [`launch_shinystan()`](https://mc-stan.org/shinystan/dev/reference/launch_shinystan.md)
  : Launch the 'ShinyStan' app
- [`launch_shinystan_demo()`](https://mc-stan.org/shinystan/dev/reference/launch_shinystan_demo.md)
  : 'ShinyStan' demo

## shinystan objects

Functions for creating, examining, and modifying shinystan objects.

- [`as.shinystan()`](https://mc-stan.org/shinystan/dev/reference/as.shinystan.md)
  [`is.shinystan()`](https://mc-stan.org/shinystan/dev/reference/as.shinystan.md)
  :

  Create and test `shinystan` objects

- [`shinystan-class`](https://mc-stan.org/shinystan/dev/reference/shinystan-class.md)
  [`shinystan`](https://mc-stan.org/shinystan/dev/reference/shinystan-class.md)
  :

  S4 `shinystan` objects

- [`sso_info()`](https://mc-stan.org/shinystan/dev/reference/shinystan-metadata.md)
  [`model_code()`](https://mc-stan.org/shinystan/dev/reference/shinystan-metadata.md)
  [`notes()`](https://mc-stan.org/shinystan/dev/reference/shinystan-metadata.md)
  [`model_name()`](https://mc-stan.org/shinystan/dev/reference/shinystan-metadata.md)
  :

  View or change metadata associated with a `shinystan` object

- [`retrieve()`](https://mc-stan.org/shinystan/dev/reference/retrieve.md)
  : Get summary statistics from shinystan object

- [`drop_parameters()`](https://mc-stan.org/shinystan/dev/reference/drop_parameters.md)
  :

  Drop parameters from a `shinystan` object

- [`update_sso()`](https://mc-stan.org/shinystan/dev/reference/update_sso.md)
  : Update an object created by the previous version of shinystan

- [`generate_quantity()`](https://mc-stan.org/shinystan/dev/reference/generate_quantity.md)
  : Add new quantity to shinystan object

## Deploying apps

Functions for deploying shinystan apps on shinyapps.io.

- [`deploy_shinystan()`](https://mc-stan.org/shinystan/dev/reference/deploy_shinystan.md)
  : Deploy a 'ShinyStan' app on the web using 'shinyapps.io' by
  'RStudio'
