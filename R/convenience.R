# This file is part of shinyStan
# Copyright (C) 2015 Jonah Sol Gabry & Stan Development Team
#
# shinyStan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinyStan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.


stan_rhat <- function(stanfit, pars) {
  if (missing(pars)) {
    return(rstan::summary(stanfit)$summary[,"Rhat"])
  }
  rstan::summary(stanfit, pars = pars)$summary[,"Rhat"]
}



stan_neff <- function(stanfit, pars) {
  if (missing(pars)) {
    return(rstan::summary(stanfit)$summary[,"n_eff"])
  }
  rstan::summary(stanfit, pars = pars)$summary[,"n_eff"]
}


stan_mcse <- function(stanfit, pars) {
  if (missing(pars)) {
    return(rstan::summary(stanfit)$summary[,"se_mean"])
  }
  rstan::summary(stanfit, pars = pars)$summary[,"se_mean"]
}


stan_nsamples <- function(stanfit) {
  length(rstan::extract(stanfit, pars = "lp__")[[1]])
}


stan_quant <- function(stanfit, probs, pars) {
  cols <- paste0(100*probs, "%")
  if (missing(pars)) {
    return(rstan::summary(stanfit, probs = probs)$summary[, cols])
  }
  rstan::summary(stanfit, pars = pars, probs = probs)$summary[, cols]
}


stan_mean <- function(stanfit, pars) {
  if (missing(pars)) {
    return(rstan::summary(stanfit)$summary[, "mean"])
  }
  rstan::summary(stanfit, pars = pars)$summary[, "mean"]
}


stan_median <- function(stanfit, pars) {
  if (missing(pars)) {
    return(stan_quant(stanfit, probs = 0.5))
  }
  stan_quant(stanfit, pars = pars, probs = 0.5)
}


stan_sd <- function(stanfit, pars) {
  if (missing(pars)) {
    return(rstan::summary(stanfit)$summary[, "sd"])
  }
  rstan::summary(stanfit, pars = pars)$summary[, "sd"]
}

stan_max_treedepth <- function(stanfit, inc_warmup = TRUE) {
  max_td <- sapply(rstan::get_sampler_params(stanfit, inc_warmup = inc_warmup),
                   function(x) max(x[,"treedepth__"]))
  names(max_td) <- paste0("chain", 1:length(max_td))
  max_td
}

stan_prop_divergent <- function(stanfit, inc_warmup = TRUE) {
  prop_div <- sapply(rstan::get_sampler_params(stanfit, inc_warmup = inc_warmup),
                     function(x) mean(x[,"n_divergent__"]))
  names(prop_div) <- paste0("chain", 1:length(prop_div))
  prop_div
}

stan_avg_stepsize <- function(stanfit, inc_warmup = TRUE) {
  avg_ss <- sapply(rstan::get_sampler_params(stanfit, inc_warmup = inc_warmup),
                   function(x) mean(x[,"stepsize__"]))
  names(avg_ss) <- paste0("chain", 1:length(avg_ss))
  avg_ss
}

stan_avg_accept <- function(stanfit, inc_warmup = TRUE) {
  avg_accept <- sapply(rstan::get_sampler_params(stanfit, inc_warmup = inc_warmup),
                       function(x) mean(x[,"accept_stat__"]))
  names(avg_accept) <- paste0("chain", 1:length(avg_accept))
  avg_accept
}
