# Create and test `shinystan` objects

The `as.shinystan` function creates `shinystan` objects that can be used
with
[`launch_shinystan`](https://mc-stan.org/shinystan/dev/reference/launch_shinystan.md)
and various other functions in the shinystan package. `as.shinystan` is
a generic for which the shinystan package provides several methods.
Currently methods are provided for creating `shinystan` objects from
arrays, lists of matrices, `stanfit` objects (rstan), `stanreg` objects
(rstanarm), and `mcmc.list` objects (coda).

`is.shinystan` tests if an object is a `shinystan` object.

## Usage

``` r
as.shinystan(X, ...)

is.shinystan(X)

# S4 method for class 'array'
as.shinystan(
  X,
  model_name = "unnamed model",
  warmup = 0,
  burnin = 0,
  param_dims = list(),
  model_code = NULL,
  note = NULL,
  sampler_params = NULL,
  algorithm = NULL,
  max_treedepth = NULL,
  ...
)

# S4 method for class 'list'
as.shinystan(
  X,
  model_name = "unnamed model",
  warmup = 0,
  burnin = 0,
  param_dims = list(),
  model_code = NULL,
  note = NULL,
  sampler_params = NULL,
  algorithm = NULL,
  max_treedepth = NULL,
  ...
)

# S4 method for class 'mcmc.list'
as.shinystan(
  X,
  model_name = "unnamed model",
  warmup = 0,
  burnin = 0,
  param_dims = list(),
  model_code = NULL,
  note = NULL,
  ...
)

# S4 method for class 'stanfit'
as.shinystan(X, pars, model_name = X@model_name, note = NULL, ...)

# S4 method for class 'stanreg'
as.shinystan(X, ppd = TRUE, seed = 1234, model_name = NULL, note = NULL, ...)

# S4 method for class 'CmdStanMCMC'
as.shinystan(X, pars = NULL, model_name = NULL, note = NULL, ...)

# S4 method for class 'CmdStanMCMC_CSV'
as.shinystan(X, pars = NULL, model_name = NULL, note = NULL, ...)
```

## Arguments

- X:

  For `as.shinystan`, an object to be converted to a `shinystan` object.
  See the Methods section below. For `is.shinystan`, an object to check.

- ...:

  Arguments passed to the individual methods.

- model_name:

  A string giving a name for the model.

- warmup:

  The number of iterations to treat as warmup. Should be `0` if warmup
  iterations are not included in `X`.

- burnin:

  Deprecated. Use `warmup` instead. The `burnin` argument will be
  removed in a future release.

- param_dims:

  Rarely used and never necessary. A named list giving the dimensions
  for all parameters. For scalar parameters use `0` as the dimension.
  See Examples.

- model_code:

  Optionally, a character string with the code used to run the model.
  This can also be added to your `shinystan` object later using the
  [`model_code`](https://mc-stan.org/shinystan/dev/reference/shinystan-metadata.md)
  function. See
  [`model_code`](https://mc-stan.org/shinystan/dev/reference/shinystan-metadata.md)
  for additional formatting instructions. After launching the app the
  code will be viewable in the **Model Code** tab. For `stanfit` (rstan)
  and `stanreg` (rstanarm) objects the model code is automatically
  retrieved from the object.

- note:

  Optionally, text to display on the **Notepad** page in the 'ShinyStan'
  GUI (stored in `user_model_info` slot of the `shinystan` object).

- sampler_params, algorithm, max_treedepth:

  Rarely used and never necessary. If using the `as.shinystan` method
  for arrays or lists, these arguments can be used to manually provide
  information that is automatically retrieved from a stanfit object when
  using the `as.shinystan` method for stanfit objects. If specified,
  `sampler_params` must have the same structure as an object returned by
  [`get_sampler_params`](https://mc-stan.org/rstan/reference/stanfit-class.html)
  (rstan), which is a list of matrices, with one matrix per chain.
  `algorithm`, if specified, must be either `"NUTS"` or `"HMC"` (static
  HMC). If `algorithm` is `"NUTS"` then `max_treedepth` (an integer
  indicating the maximum allowed treedepth when the model was fit) must
  also be provided.

- pars:

  For stanfit objects (rstan), an optional character vector specifying
  which parameters should be included in the `shinystan` object.

- ppd:

  For `stanreg` objects (rstanarm), `ppd` (logical) indicates whether to
  draw from the posterior predictive distribution before launching the
  app. The default is `TRUE`, although for very large objects it can be
  convenient to set it to `FALSE` as drawing from the posterior
  predictive distribution can be time consuming. If `ppd` is `TRUE` then
  graphical posterior predictive checks are available when 'ShinyStan'
  is launched.

- seed:

  Passed to
  [`pp_check`](https://mc-stan.org/bayesplot/reference/pp_check.html)
  (rstanarm) if `ppd` is `TRUE`.

## Value

`as.shinystan` returns a `shinystan` object, which is an instance of S4
class `"shinystan"`.

`is.shinystan` returns `TRUE` if the tested object is a `shinystan`
object and `FALSE` otherwise.

## Functions

- `as.shinystan(array)`: Create a `shinystan` object from a 3-D
  [`array`](https://rdrr.io/r/base/array.html) of simulations. The array
  should have dimensions corresponding to iterations, chains, and
  parameters, in that order.

- `as.shinystan(list)`: Create a `shinystan` object from a
  [`list`](https://rdrr.io/r/base/list.html) of matrices. Each
  [`matrix`](https://rdrr.io/r/base/matrix.html) (or 2-D array) should
  contain the simulations for an individual chain and all of the
  matrices should have the same number of iterations (rows) and
  parameters (columns). Parameters should have the same names and be in
  the same order.

- `as.shinystan(mcmc.list)`: Create a `shinystan` object from an
  `mcmc.list` object (coda).

- `as.shinystan(stanfit)`: Create a `shinystan` object from a `stanfit`
  object ([rstan](https://mc-stan.org/rstan/reference/rstan.html)).
  Fewer optional arguments are available for this method because all
  important information can be taken automatically from the `stanfit`
  object.

- `as.shinystan(stanreg)`: Create a `shinystan` object from a `stanreg`
  object
  ([rstanarm](https://mc-stan.org/rstanarm/reference/rstanarm-package.html)).

- `as.shinystan(CmdStanMCMC)`: Create a `shinystan` object from a
  `CmdStanMCMC` object (cmdstanr).

- `as.shinystan(CmdStanMCMC_CSV)`: Create a `shinystan` object from a
  `CmdStanMCMC_CSV` object created using
  [`cmdstanr::as_cmdstan_fit()`](https://mc-stan.org/cmdstanr/reference/read_cmdstan_csv.html)
  (cmdstanr).

## See also

[`launch_shinystan`](https://mc-stan.org/shinystan/dev/reference/launch_shinystan.md)
to launch the 'ShinyStan' interface using a particular `shinystan`
object.

[`drop_parameters`](https://mc-stan.org/shinystan/dev/reference/drop_parameters.md)
to remove parameters from a `shinystan` object.

[`generate_quantity`](https://mc-stan.org/shinystan/dev/reference/generate_quantity.md)
to add a new quantity to a `shinystan` object.

## Examples

``` r
 
if (FALSE) { # \dontrun{
sso <- as.shinystan(X, ...) # replace ... with optional arguments or omit it
launch_shinystan(sso)
} # }

if (FALSE) { # \dontrun{   
########################
### list of matrices ###
########################

# Generate some fake data
chain1 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
chain2 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
sso <- as.shinystan(list(chain1, chain2))
launch_shinystan(sso)

# We can also specify some or all of the optional arguments
# note: in order to use param_dims we need to rename 'beta1' and 'beta2'
# to 'beta[1]' and 'beta[2]'
colnames(chain1) <- colnames(chain2) <- c(paste0("beta[",1:2,"]"), "sigma")
sso2 <- as.shinystan(list(chain1, chain2), 
                     model_name = "Example", warmup = 0, 
                     param_dims = list(beta = 2, sigma = 0))
launch_shinystan(sso2)
} # }

if (FALSE) { # \dontrun{
######################
### stanfit object ###
######################
library("rstan")
fit <- stan_demo("eight_schools")
sso <- as.shinystan(fit, model_name = "example")
} # }

if (FALSE) { # \dontrun{
######################
### stanreg object ###
######################
library("rstanarm")
example("example_model")
sso <- as.shinystan(example_model)
launch_shinystan(sso)
} # }
```
