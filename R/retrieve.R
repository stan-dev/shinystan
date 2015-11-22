# This file is part of shinystan
# Copyright (C) 2015 Jonah Gabry
#
# shinystan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinystan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.

#' Get summary statistics from shinystan object
#'
#' From a shinystan object get rhat, effective sample size, posterior
#' quantiles, means, standard deviations, sampler diagnostics, etc.
#' 
#' @param sso A shinystan object
#' @param what What do you want to get? See \strong{Details}, below.
#' @param ... Optional arguments, in particular \code{pars} to specify parameter
#'   names (by default all parameters will be used). For NUTS sampler parameters
#'   only (e.g. stepsize, treedepth) \code{inc_warmup} can also be specified to 
#'   include/exclude warmup iterations (the default is \code{FALSE}). See
#'   \strong{Details}, below.
#'   
#' @details The argument \code{what} can take on the values below. Args: \code{arg}
#' means that \code{arg} can be specified in \code{...} for this value of \code{what}.
#' \describe{
#'   \item{\code{"rhat", "Rhat", "r_hat", or "R_hat"}}{returns: Rhat statistics. Args: \code{pars}}
#'   \item{\code{"N_eff","n_eff", "neff", "Neff", "ess", or "ESS"}}{returns: Effective sample sizes. Args: \code{pars}}
#'   \item{\code{"mean"}}{returns: Posterior means. Args: \code{pars}}
#'   \item{\code{"sd"}}{returns: Posterior standard deviations. Args: \code{pars}}
#'   \item{\code{"se_mean" or "mcse"}}{returns: Monte carlo standard error. Args: \code{pars}}
#'   \item{\code{"median"}}{returns: Posterior medians. Args: \code{pars}.}
#'   \item{\code{"quantiles" or any string with "quant" in it (not case sensitive)}}{returns: 2.5\%, 25\%, 50\%, 75\%, 97.5\% posterior quantiles. Args: \code{pars}.}
#'   \item{\code{"avg_accept_stat" or any string with "accept" in it (not case sensitive)}}{returns: Average value of "accept_stat" (which itself is the average acceptance probability over the NUTS subtree). Args: \code{inc_warmup}}
#'   \item{\code{"prop_divergent" or any string with "diverg" in it (not case sensitive)}}{returns: Proportion of divergent iterations for each chain. Args: \code{inc_warmup}}
#'   \item{\code{"max_treedepth" or any string with "tree" or "depth" in it (not case sensitive)}}{returns: Maximum treedepth for each chain. Args: \code{inc_warmup}}
#'   \item{\code{"avg_stepsize" or any string with "step" in it (not case sensitive)}}{returns: Average stepsize for each chain. Args: \code{inc_warmup}}
#' }
#' 
#' @note Sampler diagnostics (e.g. \code{"avg_accept_stat"}) only available for
#'   models originally fit using Stan.
#' 
#' @export
#' @examples
#' \dontrun{
#' # assume 'X' is a shinystan object with parameters
#' # 'beta[1]', 'beta[2]', 'sigma[1]', 'sigma[2]'"
#'
#' retrieve(X, "rhat")
#' retrieve(X, "mean", pars = c('beta[1]', 'sigma[1]'))
#' retrieve(X, "quantiles")
#'
#' retrieve(X, "max_treedepth")  # equivalent to retrieve(X, "depth"), retrieve(X, "tree"), etc.
#' retrieve(X, "prop_divergent", inc_warmup = FALSE)  # don't include warmup iterations
#' }
#'

retrieve <- function(sso, what, ...) {
  sso_check(sso)
  .retrieve(sso, what, ...)
}
