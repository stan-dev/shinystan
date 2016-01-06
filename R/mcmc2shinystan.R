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

# Convert mcmc.list object to shinystan object
#
# @param X An mcmc.list object (\pkg{coda})
# @param model_name A character string giving a name for the model
# @param burnin The number of burnin (warmup) iterations. Should only be specified if the 
# burnin samples are included in \code{X}.
# @param param_dims Rarely used and never necessary. A named list giving the dimensions for 
# all parameters. For scalar parameters use \code{0} as the dimension. 
# See Examples in \code{\link[shinyStan]{as.shinystan}}.
# @param model_code A character string with the code you used to run your model. This can 
# also be added to your \code{shinystan} object later using the 
# \code{\link[shinyStan]{include_model_code}} function. 
# See \code{\link[shinyStan]{include_model_code}} for additional formatting instructions. 
# After launching the app \code{model_code} will be viewable in the \strong{Model Code} tab.
# 
# @return An object of class \code{shinystan} that can be used with 
# \code{\link[shinyStan]{launch_shinystan}}. 
# 

mcmc2shinystan <- function(X, model_name = "unnamed model", burnin = 0, param_dims = list(),
                           model_code) {

  coda_check()
  Xname <- deparse(substitute(X))
  if (!inherits(X, "mcmc.list")) {
    stop (paste(Xname, "is not an mcmc.list."))
  }

  if (length(X) == 1) {
    return(chains2shinystan(list(mcmclist2matrix(X))))
  }

  samps_array <- array(NA, dim = c(coda::niter(X), coda::nvar(X), coda::nchain(X)),
                       dimnames = list(iter = time(X), var = coda::varnames(X), chain = coda::chanames(X)))
  for (c in 1:coda::nchain(X)) samps_array[,,c] <- X[[c]]
  samps_array <- aperm(drop(samps_array), c(1,3,2))
  dimnames(samps_array) <- list(iterations = 1:nrow(samps_array),
                                chains = paste0("chain:",1:ncol(samps_array)),
                                parameters = dimnames(samps_array)[[3]])
  param_names <- dimnames(X[[1]])[[2]]
  param_dims <- param_dims

  if (length(param_dims) != 0) {
    zeros <- sapply(1:length(param_dims), function(i) {
      0 %in% param_dims[[i]]
    })
    for (i in which(zeros)) {
      param_dims[[i]] <- numeric(0)
    }
  }

  if (length(param_dims) == 0) {
    param_dims <- list()
    param_dims[1:length(param_names)] <- NA
    names(param_dims) <- param_groups <- param_names
    for(i in 1:length(param_names)) {
      param_dims[[i]] <- numeric(0)
    }
  } else {
    param_groups <- names(param_dims)
  }
  slots <- list()
  slots$Class <- "shinystan"
  slots$model_name <- model_name
  slots$param_names <- param_names
  slots$param_dims <- param_dims
  slots$samps_all <- samps_array
  slots$summary <- shinystan_monitor(samps_array, warmup = burnin)
  slots$sampler_params <- list(NA)
  slots$nChains <- ncol(samps_array)
  slots$nIter <- nrow(samps_array)
  slots$nWarmup <- burnin
  if (!missing(model_code)) slots$model_code <- model_code

  do.call("new", slots)
}
