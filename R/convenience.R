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
  
retrieve_rhat <- function(sso, pars) {
  if (missing(pars)) {
    return(sso@summary[,"Rhat"])
  }
  sso@summary[pars,"Rhat"]
}

retrieve_neff <- function(sso, pars) {
  if (missing(pars)) {
    return(sso@summary[,"n_eff"])
  }
  sso@summary[pars,"n_eff"]
}

retrieve_mcse <- function(sso, pars) {
  if (missing(pars)) {
    return(sso@summary[,"se_mean"])
  }
  sso@summary[pars,"se_mean"]
}

retrieve_quant <- function(sso, pars) {
  cols <- paste0(100*c(0.025, 0.25, 0.5, 0.75, 0.975), "%")
  if (missing(pars)) {
    return(sso@summary[, cols])
  }
  sso@summary[pars, cols]
}

retrieve_median <- function(sso, pars) {
  if (missing(pars)) {
    return(retrieve_quant(sso)[,"50%"])
  }
  retrieve_quant(sso, pars)[,"50%"]
}

retrieve_mean <- function(sso, pars) {
  if (missing(pars)) {
    return(sso@summary[,"mean"])
  }
  sso@summary[pars,"mean"]
}

retrieve_sd <- function(sso, pars) {
  if (missing(pars)) {
    return(sso@summary[, "sd"])
  }
  sso@summary[pars, "sd"]
}


sp_check <- function(sso) {
  if (identical(sso@sampler_params, list(NA)))
    stop("No sampler parameters found", call. = FALSE)
}

retrieve_max_treedepth <- function(sso, inc_warmup = FALSE) {
  sp_check(sso)
  rows <- if (inc_warmup) 1:sso@nIter else (sso@nWarmup+1):sso@nIter
  max_td <- sapply(sso@sampler_params,
                   function(x) max(x[rows,"treedepth__"]))
  names(max_td) <- paste0("chain", 1:length(max_td))
  max_td
}

retrieve_prop_divergent <- function(sso, inc_warmup = FALSE) {
  sp_check(sso)
  rows <- if (inc_warmup) 1:sso@nIter else (sso@nWarmup+1):sso@nIter
  prop_div <- sapply(sso@sampler_params,
                     function(x) mean(x[rows,"n_divergent__"]))
  names(prop_div) <- paste0("chain", 1:length(prop_div))
  prop_div
}

retrieve_avg_stepsize <- function(sso, inc_warmup = FALSE) {
  sp_check(sso)
  rows <- if (inc_warmup) 1:sso@nIter else (sso@nWarmup+1):sso@nIter
  avg_ss <- sapply(sso@sampler_params,
                   function(x) mean(x[rows,"stepsize__"]))
  names(avg_ss) <- paste0("chain", 1:length(avg_ss))
  avg_ss
}

retrieve_avg_accept <- function(sso, inc_warmup = FALSE) {
  sp_check(sso)
  rows <- if (inc_warmup) 1:sso@nIter else (sso@nWarmup+1):sso@nIter
  avg_accept <- sapply(sso@sampler_params,
                       function(x) mean(x[rows,"accept_stat__"]))
  names(avg_accept) <- paste0("chain", 1:length(avg_accept))
  avg_accept
}
