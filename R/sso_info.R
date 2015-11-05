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

#' Print summary of shinystan object
#'
#' Prints basic summary info including number of parameters, chains, iterations, 
#' warmup iterations, etc. 
#'
#' @export
#' @param sso A \code{shinystan} object. 
#'

sso_info <- function(sso) {
  sso_check(sso)
  sso_name <- deparse(substitute(sso))
  has_notes <- 
    sso@user_model_info != "Use this space to store notes about your model"
  has_code <- 
    sso@model_code != "Use this space to store your model code" 
  
  cat(
    sso_name, "---------------------",
    paste("Model name:", sso@model_name),
    paste("Parameters:", length(sso@param_names)),
    paste("Parameter groups:", length(names(sso@param_dims))),
    paste("Chains:", sso@nChains),
    paste("Iterations:", sso@nIter),
    paste("Warmup:", sso@nWarmup),
    paste("Has model code:", has_code),
    paste("Has user notes:", has_notes),
    sep = "\n"
  )
}