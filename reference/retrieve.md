# Get summary statistics from shinystan object

From a shinystan object get rhat, effective sample size, posterior
quantiles, means, standard deviations, sampler diagnostics, etc.

## Usage

``` r
retrieve(sso, what, ...)
```

## Arguments

- sso:

  A
  [`shinystan object`](https://mc-stan.org/shinystan/reference/as.shinystan.md).

- what:

  What do you want to get? See Details, below.

- ...:

  Optional arguments, in particular `pars` to specify parameter names
  (by default all parameters will be used). For NUTS sampler parameters
  only (e.g. stepsize, treedepth) `inc_warmup` can also be specified to
  include/exclude warmup iterations (the default is `FALSE`). See
  Details, below.

## Details

The argument `what` can take on the values below. 'Args: `arg`' means
that `arg` can be specified in `...` for this value of `what`.

- `"rhat"`, `"Rhat"`, `"r_hat"`, or `"R_hat"`:

  returns: Rhat statistics. Args: `pars`

- `"N_eff"`, `"n_eff"`, `"neff"`, `"Neff"`, `"ess"`, or `"ESS"`:

  returns: Effective sample sizes. Args: `pars`

- `"mean"`:

  returns: Posterior means. Args: `pars`

- `"sd"`:

  returns: Posterior standard deviations. Args: `pars`

- `"se_mean"` or `"mcse"`:

  returns: Monte Carlo standard error. Args: `pars`

- `"median"`:

  returns: Posterior medians. Args: `pars`.

- `"quantiles"` or any string with `"quant"` in it (not case sensitive):

  returns: 2.5%, 25%, 50%, 75%, 97.5% posterior quantiles. Args: `pars`.

- `"avg_accept_stat"` or any string with `"accept"` in it (not case
  sensitive):

  returns: Average value of "accept_stat" (which itself is the average
  acceptance probability over the NUTS subtree). Args: `inc_warmup`

- `"prop_divergent"` or any string with `"diverg"` in it (not case
  sensitive):

  returns: Proportion of divergent iterations for each chain. Args:
  `inc_warmup`

- `"max_treedepth"` or any string with `"tree"` or `"depth"` in it (not
  case sensitive):

  returns: Maximum treedepth for each chain. Args: `inc_warmup`

- `"avg_stepsize"` or any string with `"step"` in it (not case
  sensitive):

  returns: Average stepsize for each chain. Args: `inc_warmup`

## Note

Sampler diagnostics (e.g. `"avg_accept_stat"`) only available for models
originally fit using Stan.

## Examples

``` r
# Using example shinystan object 'eight_schools'
sso <- eight_schools
retrieve(sso, "rhat")
retrieve(sso, "mean", pars = c('theta[1]', 'mu'))
retrieve(sso, "quantiles")
retrieve(sso, "max_treedepth")  # equivalent to retrieve(sso, "depth"), retrieve(sso, "tree"), etc.
retrieve(sso, "prop_divergent")
retrieve(sso, "prop_divergent", inc_warmup = TRUE)
```
