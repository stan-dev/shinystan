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


.rename_scalar <- function(sso, oldname = "lp__", newname = "log-posterior") {
  p <- which(sso@param_names == oldname)
  if (identical(integer(0), p)) 
    return(sso)
  sso@param_names[p] <- 
    dimnames(sso@samps_all)$parameters[p] <-
      names(sso@param_dims)[which(names(sso@param_dims) == oldname)] <- newname
  sso
}

# convert stanfit object to shinystan object
stan2shinystan <- function(stanfit, model_name, notes) {
  # notes: text to add to user_model_info slot
  rstan_check()
  if (!is.stanfit(stanfit)) {
    name <- deparse(substitute(stanfit))
    stop(paste(name, "is not a stanfit object."))
  }
  
  stan_args <- stanfit@stan_args[[1L]]
  stan_method <- stan_args$method
  vb <- stan_method == "variational"
  from_cmdstan_csv <- ("engine" %in% names(stan_args))
  stan_algorithm <- if (from_cmdstan_csv) 
    toupper(stan_args$engine) else stan_args$algorithm
  warmup <- if (from_cmdstan_csv) stanfit@sim$warmup2[1L] else stanfit@sim$warmup
  if (!is.null(stan_args[["save_warmup"]])) {
    if (!stan_args[["save_warmup"]]) warmup <- 0
  }
  nWarmup <- if (from_cmdstan_csv) warmup else floor(warmup / stanfit@sim$thin)
  
  cntrl <- stanfit@stan_args[[1L]]$control
  if (is.null(cntrl)) 
    max_td <- 11
  else {
    max_td <- cntrl$max_treedepth
    if (is.null(max_td)) 
      max_td <- 11
  }
  
  samps_all <- rstan::extract(stanfit, permuted = FALSE, inc_warmup = TRUE)
  param_names <- dimnames(samps_all)[[3L]] # stanfit@sim$fnames_oi
  param_dims <- stanfit@sim$dims_oi
  
  if (!vb && !(stan_algorithm %in% c("NUTS", "HMC"))) {
    warning("Most features are only available for models using
            algorithm NUTS or algorithm HMC.")
  }
  mname <- if (!missing(model_name)) model_name else stanfit@model_name
  mcode <- rstan::get_stancode(stanfit)
  
  sampler_params <- if (vb) list(NA) else suppressWarnings(rstan::get_sampler_params(stanfit))
  stan_summary <- rstan::summary(stanfit)$summary
  if (vb) stan_summary <- cbind(stan_summary, Rhat = NA, n_eff = NA, se_mean = NA)
  
  slots <- list()
  slots$Class <- "shinystan"
  slots$model_name <- mname
  slots$param_names <- param_names
  slots$param_dims <- param_dims
  slots$samps_all <- samps_all
  slots$summary <- stan_summary
  slots$sampler_params <- sampler_params
  slots$nChains <- ncol(stanfit)
  slots$nIter <- nrow(samps_all) 
  slots$nWarmup <- nWarmup
  if (!missing(notes)) slots$user_model_info <- notes
  if (length(mcode) > 0) slots$model_code <- mcode
  slots$misc <- list(max_td = max_td, stan_method = stan_method, 
                     stan_algorithm = stan_algorithm)
  sso <- do.call("new", slots)
  .rename_scalar(sso)
}

