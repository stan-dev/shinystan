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


#' Add to shinystan object a new parameter as a function of one or two 
#' existing parameters
#' 
#' @export
#' @param sso shinystan object.
#' @param fun Function to call, i.e. \code{function(param1)} or
#'   \code{function(param1,param2)}. See \strong{Examples}, below.
#' @param param1 Name of first parameter as character string.
#' @param param2 Optional. Name of second paramter as character string.
#' @param new_name Name for the new parameter as character string.
#' 
#' @return sso, updated. See Examples.
#' 
#' @seealso \code{\link{as.shinystan}}
#'
#' @examples
#' \dontrun{
#' #################
#' ### Example 1 ###
#' #################
#'
#' # Below, assume X is a shinystan object and two of the
#' # parameters are alpha and beta.
#'
#' # Add parameter gamma = inverse-logit(beta) to X
#' inv_logit <- function(x) 1/(exp(-x) + 1)
#' X <- generate_quantity(sso = X,
#'                        fun = inv_logit,
#'                        param1 = "beta",
#'                        new_name = "gamma")
#'
#'
#' # Add parameter delta = (alpha-beta)^2 to X
#' X <- generate_quantity(sso = X,
#'                        fun = function(x,y) (x-y)^2,
#'                        param1 = "alpha",
#'                        param2 = "beta",
#'                        new_name = "delta")
#'
#' launch_shinystan(X)
#'}

generate_quantity <- function(sso, param1, param2, fun, new_name) {
  sso_check(sso)
  
  name_exists <- new_name %in% sso@param_names  
  if (name_exists) stop(paste("There is already a parameter named", new_name))
  
  message("\nThis might take a moment for large shinystan objects...\n")
  
  two_params <- !missing(param2)
  samps <- sso@samps_all
  dim_samps <- dim(samps)
  nDim <- length(dim_samps)
  if (nDim == 3) { # i.e. multiple chains
    x_samps <- samps[, , param1]
    if (two_params) y_samps <- samps[, , param2]
  }
  if (nDim == 2) { # i.e. only 1 chain
    x_samps <- samps[, param1]
    if (two_params) y_samps <- samps[, param2]
  }
  
  arglist <- if (two_params) list(x_samps, y_samps) else list(x_samps)
  temp <- do.call(fun, args = arglist)
  
  new_dim <- dim_samps
  new_dim[[nDim]] <- new_dim[[nDim]] + 1
  new_dim_names <- dimnames(samps)
  new_dim_names[[nDim]] <- c(new_dim_names[[nDim]], new_name)
  samps <- array(data = c(samps, temp), dim = new_dim, dimnames = new_dim_names)
  
  param_dims_new <- sso@param_dims
  param_dims_new[[new_name]] <- numeric(0)
  sso_new <- array2shinystan(samps,
                             model_name = sso@model_name,
                             burnin = sso@nWarmup,
                             param_dims = param_dims_new)
  sso_new@summary <- shinystan_monitor(samps, warmup = sso@nWarmup)
  
  slot_names <- c("stan_algorithm", "sampler_params", "model_code", "user_model_info")
  for (sn in slot_names) {
    slot(sso_new, sn) <- slot(sso, sn)
  }
  
  return(sso_new)
}
