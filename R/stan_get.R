#' Convenience function for \code{stanfit} objects
#'
#' From a \code{stanfit} object get rhat, effective sample size,
#' total sample size, posterior quantiles, means, standard deviations,
#' sampler diagnostics, etc.
#'
#' @param stanfit A \code{stanfit} object
#' @param what What do you want to get? See \strong{Details}, below.
#' @param ... Optional arguments, in particular \code{pars} to specify parameter
#' names (by default all parameters will be used) and \code{probs} to
#' specify which quantiles should be returned. For NUTS sampler parameters
#' only (e.g. stepsize, treedepth) \code{inc_warmup} can also be specified to
#' include/exclude warmup iterations (the default is \code{TRUE}, i.e. to
#' include warmup iterations). See \strong{Details}, below.
#'
#' @details The argument \code{what} can take on the values below. Args: \code{arg}
#' means that \code{arg} can be specified in \code{...} for this value of \code{what}.
#' \describe{
#'   \item{\code{"rhat", "Rhat", "r_hat", or "R_hat"}}{returns: Rhat statistics. Args: \code{pars}}
#'   \item{\code{"N_eff","n_eff", "neff", "Neff", "ess", or "ESS"}}{returns: Effective sample sizes. Args: \code{pars}}
#'   \item{\code{"N", "nsamples", "nSamples", "samplesize", "nsamps", "Nsamps", or "nSamps"}}{returns: Total sample size. Args: \code{pars}}
#'   \item{\code{"mean"}}{returns: Posterior means. Args: \code{pars}}
#'   \item{\code{"sd"}}{returns: Posterior standard deviations. Args: \code{pars}}
#'   \item{\code{"se_mean" or "mcse"}}{returns: Monte carlo standard error. Args: \code{pars}}
#'   \item{\code{"median"}}{returns: Posterior medians. Args: \code{pars}.}
#'   \item{\code{"quantile" or any string with "quant" in it (not case sensitive)}}{returns: Posterior quantiles. Args: \code{pars}, \code{probs}.}
#'   \item{\code{"avg_accept_stat" or any string with "accept" in it (not case sensitive)}}{returns: Average value of "accept_stat" (which itself is the average acceptance probability over the NUTS subtree). Args: \code{inc_warmup}}
#'   \item{\code{"prop_divergent" or any string with "diverg" in it (not case sensitive)}}{returns: Proportion of divergent iterations for each chain. Args: \code{inc_warmup}}
#'   \item{\code{"max_treedepth" or any string with "tree" or "depth" in it (not case sensitive)}}{returns: Maximum treedepth for each chain. Args: \code{inc_warmup}}
#'   \item{\code{"avg_stepsize" or any string with "step" in it (not case sensitive)}}{returns: Average stepsize for each chain. Args: \code{inc_warmup}}
#' }
#' @export
#' @examples
#' \dontrun{
#' # assume 'X' is a stanfit object with parameters
#' # 'beta[1]', 'beta[2]', 'sigma[1]', 'sigma[2]'"
#'
#' stan_get(X, "rhat")
#' stan_get(X, "mean", pars = c('beta[1]', 'sigma[1]'))
#' stan_get(X, "quantile", probs = c(0.1, 0.9))
#'
#' stan_get(X, "max_treedepth")  # equivalent to stan_get(X, "depth"), stan_get(X, "tree"), etc.
#' stan_get(X, "prop_divergent", inc_warmup = FALSE)  # don't include warmup iterations
#' }
#'

stan_get <- function(stanfit, what, ...) {
  my_grepl <- function(pattern, x, ignore.case = TRUE) {
    grepl(pattern = pattern, x = x, ignore.case = ignore.case)
  }

  if (what %in% c("rhat", "rhats", "Rhat", "Rhats", "r_hat", "R_hat")) {
    return(stan_rhat(stanfit, ...))
  }
  if (what %in% c("N_eff","n_eff", "neff", "Neff", "ess","ESS")) {
    return(stan_neff(stanfit, ...))
  }
  if (what %in% c("N", "nsamples", "nSamples", "samplesize",
                  "nsamps", "Nsamps", "nSamps")) {
    return(stan_nsamples(stanfit, ...))
  }
  if (my_grepl("mean", what)) {
    return(stan_mean(stanfit, ...))
  }
  if (my_grepl("sd", what)) {
    return(stan_sd(stanfit, ...))
  }
  if (what %in% c("se_mean", "mcse")) {
    return(stan_mcse(stanfit, ...))
  }
  if (my_grepl("quant", what)) {
    return(stan_quant(stanfit, ...))
  }
  if (my_grepl("median", what)) {
    return(stan_median(stanfit, ...))
  }
  if (my_grepl("tree", what) | my_grepl("depth", what)) {
    return(stan_max_treedepth(stanfit, ...))
  }
  if (my_grepl("step", what)) {
    return(stan_avg_stepsize(stanfit, ...))
  }
  if (my_grepl("diverg", what)) {
    return(stan_prop_divergent(stanfit, ...))
  }
  if (my_grepl("accept", what)) {
    return(stan_avg_accept(stanfit, ...))
  }
}
