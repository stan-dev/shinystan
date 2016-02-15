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

#' @importFrom stats model.frame model.response
#' 
stanreg2shinystan <- function(X, ppd = TRUE, ...) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) 
    stop("Please install the 'rstanarm' package.", call. = FALSE)
  stopifnot(is.stanreg(X))
  sso <- stan2shinystan(X$stanfit, ...)

  samps_all <- sso@samps_all
  sel <- grep(":_NEW_", dimnames(samps_all)[[3L]], fixed = TRUE)
  samps_all <- samps_all[, , -sel, drop = FALSE]
  param_names <- sso@param_names[-sel]
  param_dims <- vector(mode = "list", length = length(param_names))
  names(param_dims) <- param_names
  for(i in 1:length(param_names)) {
    param_dims[[i]] <- numeric(0)
  }
  sso@param_dims <- param_dims
  sso@param_names <- param_names
  sso@samps_all <- samps_all
  sso@summary <- sso@summary[-sel, , drop = FALSE]
  
  sso@misc$stanreg <- TRUE
  if (ppd) {
    ppc <- rstanarm::pp_check
    pp_check_plots <- list()
    SEED <- 0110
    pp_check_plots[["pp_check_hist"]] <- 
      do.call("ppc", list(object = X, check = "dist", nreps = 8, 
                          overlay = FALSE, seed = SEED))
    pp_check_plots[["pp_check_dens"]] <- 
      do.call("ppc", list(object = X, check = "dist", nreps = 8, 
                          overlay = TRUE, seed = SEED))
    pp_check_plots[["pp_check_resid"]] <- 
      do.call("ppc", list(object = X, check = "resid", nreps = 8, 
                          seed = SEED))
    pp_check_plots[["pp_check_scatter"]] <- 
      do.call("ppc", list(object = X, check = "scatter", nreps = NULL, 
                          seed = SEED))
    pp_check_plots[["pp_check_stat_mean"]] <- 
      do.call("ppc", list(object = X, check = "test", test = "mean", 
                          seed = SEED))
    pp_check_plots[["pp_check_stat_sd"]] <- 
      do.call("ppc", list(object = X, check = "test", test = "sd", 
                          seed = SEED))
    pp_check_plots[["pp_check_stat_min"]] <- 
      do.call("ppc", list(object = X, check = "test", test = "min", 
                          seed = SEED))
    pp_check_plots[["pp_check_stat_max"]] <- 
      do.call("ppc", list(object = X, check = "test", test = "max", 
                          seed = SEED))
    
    sso@misc$pp_check_plots <- pp_check_plots
  }
  sso
}
