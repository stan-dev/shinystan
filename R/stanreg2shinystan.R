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
  stopifnot(is.stanreg(X))
  sso <- stan2shinystan(X$stanfit, ...)
  param_names <- sso@param_names
  param_dims <- list()
  param_dims[1:length(param_names)] <- NA
  names(param_dims) <- param_names
  for(i in 1:length(param_names)) {
    param_dims[[i]] <- numeric(0)
  }
  sso@param_dims <- param_dims
  sso@misc$stanreg <- TRUE
  if (ppd) {
    if (!exists("pp_check", mode = "function"))
      stop("Please load or install the 'rstanarm' package.", call. = FALSE)
    pp_check_plots <- list()
    SEED <- 0110
    pp_check_plots[["pp_check_hist"]] <- do.call("pp_check", list(object = X, check = "dist", nreps = 8, overlay = FALSE, seed = SEED))
    pp_check_plots[["pp_check_dens"]] <- do.call("pp_check", list(object = X, check = "dist", nreps = 8, overlay = TRUE, seed = SEED))
    pp_check_plots[["pp_check_resid"]] <- do.call("pp_check", list(object = X, check = "resid", nreps = 8, seed = SEED))
    pp_check_plots[["pp_check_scatter"]] <- do.call("pp_check", list(object = X, check = "scatter", nreps = NULL, seed = SEED))
    pp_check_plots[["pp_check_stat_mean"]] <- do.call("pp_check", list(object = X, check = "test", test = "mean", seed = SEED))
    pp_check_plots[["pp_check_stat_sd"]] <- do.call("pp_check", list(object = X, check = "test", test = "sd", seed = SEED))
    pp_check_plots[["pp_check_stat_min"]] <- do.call("pp_check", list(object = X, check = "test", test = "min", seed = SEED))
    pp_check_plots[["pp_check_stat_max"]] <- do.call("pp_check", list(object = X, check = "test", test = "max", seed = SEED))
    sso@misc$pp_check_plots <- pp_check_plots
  }
  sso
}
