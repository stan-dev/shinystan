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
#' @export
#' @template args-sso
#' @param what What do you want to get? See Details, below.
#' @param ... Optional arguments, in particular \code{pars} to specify parameter
#'   names (by default all parameters will be used). For NUTS sampler parameters
#'   only (e.g. stepsize, treedepth) \code{inc_warmup} can also be specified to 
#'   include/exclude warmup iterations (the default is \code{FALSE}). See
#'   Details, below.
#'   
#' @details The argument \code{what} can take on the values below. Args:
#'   \code{arg} means that \code{arg} can be specified in \code{...} for this
#'   value of \code{what}.
#' \describe{
#'   \item{\code{"rhat"}, \code{"Rhat"}, \code{"r_hat"}, or \code{"R_hat"}}{returns: Rhat statistics. Args: \code{pars}}
#'   \item{\code{"N_eff"}, \code{"n_eff"}, \code{"neff"}, \code{"Neff"}, \code{"ess"}, or \code{"ESS"}}{returns: Effective sample sizes. Args: \code{pars}}
#'   \item{\code{"mean"}}{returns: Posterior means. Args: \code{pars}}
#'   \item{\code{"sd"}}{returns: Posterior standard deviations. Args: \code{pars}}
#'   \item{\code{"se_mean"} or \code{"mcse"}}{returns: Monte carlo standard error. Args: \code{pars}}
#'   \item{\code{"median"}}{returns: Posterior medians. Args: \code{pars}.}
#'   \item{\code{"quantiles"} or any string with \code{"quant"} in it (not case sensitive)}{returns: 2.5\%, 25\%, 50\%, 75\%, 97.5\% posterior quantiles. Args: \code{pars}.}
#'   \item{\code{"avg_accept_stat"} or any string with \code{"accept"} in it (not case sensitive)}{returns: Average value of "accept_stat" (which itself is the average acceptance probability over the NUTS subtree). Args: \code{inc_warmup}}
#'   \item{\code{"prop_divergent"} or any string with \code{"diverg"} in it (not case sensitive)}{returns: Proportion of divergent iterations for each chain. Args: \code{inc_warmup}}
#'   \item{\code{"max_treedepth"} or any string with \code{"tree"} or \code{"depth"} in it (not case sensitive)}{returns: Maximum treedepth for each chain. Args: \code{inc_warmup}}
#'   \item{\code{"avg_stepsize"} or any string with \code{"step"} in it (not case sensitive)}{returns: Average stepsize for each chain. Args: \code{inc_warmup}}
#' }
#' 
#' @note Sampler diagnostics (e.g. \code{"avg_accept_stat"}) only available for
#'   models originally fit using Stan.
#'   
#' @examples
#' # Using example shinystan object 'eight_schools'
#' sso <- eight_schools
#' retrieve(sso, "rhat")
#' retrieve(sso, "mean", pars = c('theta[1]', 'mu'))
#' retrieve(sso, "quantiles")
#' retrieve(sso, "max_treedepth")  # equivalent to retrieve(sso, "depth"), retrieve(sso, "tree"), etc.
#' retrieve(sso, "prop_divergent")
#' retrieve(sso, "prop_divergent", inc_warmup = TRUE)
#'
retrieve <- function(sso, what, ...) {
  sso_check(sso)
  .retrieve(sso, what, ...)
}


# retrieve helpers
.retrieve <- function(sso, what, ...) {
  if (what %in% c("rhat", "rhats", "Rhat", "Rhats", "r_hat", "R_hat"))
    return(retrieve_rhat(sso, ...))
  if (what %in% c("N_eff", "n_eff", "neff", "Neff", "ess", "ESS"))
    return(retrieve_neff(sso, ...))
  if (grepl_ic("mean", what))
    return(retrieve_mean(sso, ...))
  if (grepl_ic("sd", what))
    return(retrieve_sd(sso, ...))
  if (what %in% c("se_mean", "mcse"))
    return(retrieve_mcse(sso, ...))
  if (grepl_ic("quant", what))
    return(retrieve_quant(sso, ...))
  if (grepl_ic("median", what))
    return(retrieve_median(sso, ...))
  if (grepl_ic("tree", what) | grepl_ic("depth", what))
    return(retrieve_max_treedepth(sso, ...))
  if (grepl_ic("step", what))
    return(retrieve_avg_stepsize(sso, ...))
  if (grepl_ic("diverg", what))
    return(retrieve_prop_divergent(sso, ...))
  if (grepl_ic("accept", what))
    return(retrieve_avg_accept(sso, ...))
}


retrieve_rhat <- function(sso, pars) {
  if (missing(pars))
    return(slot(sso, "summary")[, "Rhat"])
  slot(sso, "summary")[pars, "Rhat"]
}

retrieve_neff <- function(sso, pars) {
  if (missing(pars))
    return(slot(sso, "summary")[, "n_eff"])
  slot(sso, "summary")[pars, "n_eff"]
}

retrieve_mcse <- function(sso, pars) {
  if (missing(pars))
    return(slot(sso, "summary")[, "se_mean"])
  slot(sso, "summary")[pars, "se_mean"]
}

retrieve_quant <- function(sso, pars) {
  cols <- paste0(100 * c(0.025, 0.25, 0.5, 0.75, 0.975), "%")
  if (missing(pars))
    return(slot(sso, "summary")[, cols])
  slot(sso, "summary")[pars, cols]
}

retrieve_median <- function(sso, pars) {
  if (missing(pars))
    return(retrieve_quant(sso)[, "50%"])
  retrieve_quant(sso, pars)[, "50%"]
}

retrieve_mean <- function(sso, pars) {
  if (missing(pars))
    return(slot(sso, "summary")[, "mean"])
  slot(sso, "summary")[pars, "mean"]
}

retrieve_sd <- function(sso, pars) {
  if (missing(pars))
    return(slot(sso, "summary")[, "sd"])
  slot(sso, "summary")[pars, "sd"]
}


.sp_check <- function(sso) {
  if (identical(slot(sso, "sampler_params"), list(NA)))
    stop("No sampler parameters found", call. = FALSE)
}

.which_rows <- function(sso, inc_warmup) {
  if (inc_warmup) {
    seq_len(slot(sso, "nIter"))
  } else {
    seq(from = 1 + slot(sso, "nWarmup"), 
        to = slot(sso, "nIter"))
  }
}

retrieve_max_treedepth <- function(sso, inc_warmup = FALSE) {
  .sp_check(sso)
  rows <- .which_rows(sso, inc_warmup)
  max_td <- sapply(slot(sso, "sampler_params"), function(x) 
    max(x[rows, "treedepth__"]))
  names(max_td) <- paste0("chain", 1:length(max_td))
  max_td
}

retrieve_prop_divergent <- function(sso, inc_warmup = FALSE) {
  .sp_check(sso)
  rows <- .which_rows(sso, inc_warmup)
  prop_div <- sapply(slot(sso, "sampler_params"), function(x) 
    mean(x[rows, "divergent__"]))
  names(prop_div) <- paste0("chain", 1:length(prop_div))
  prop_div
}

retrieve_avg_stepsize <- function(sso, inc_warmup = FALSE) {
  .sp_check(sso)
  rows <- .which_rows(sso, inc_warmup)
  avg_ss <- sapply(slot(sso, "sampler_params"), function(x) 
    mean(x[rows, "stepsize__"]))
  names(avg_ss) <- paste0("chain", 1:length(avg_ss))
  avg_ss
}

retrieve_avg_accept <- function(sso, inc_warmup = FALSE) {
  .sp_check(sso)
  rows <- .which_rows(sso, inc_warmup)
  avg_accept <- sapply(slot(sso, "sampler_params"), function(x) 
    mean(x[rows, "accept_stat__"]))
  names(avg_accept) <- paste0("chain", 1:length(avg_accept))
  avg_accept
}
