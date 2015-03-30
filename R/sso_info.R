
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



#' Print summary of \code{shinystan} object
#'
#' Prints basic summary info including number of parameters, chains, iterations, 
#' warmup iterations, etc. 
#'
#' @param sso A \code{shinystan} object. 
#' @export
#'

sso_info <- function(sso) {
  cat(
    paste("Model name:", sso@model_name),
    paste("Parameters:", length(sso@param_names)),
    paste("Parameter groups:", length(sso@param_groups)),
    paste("Chains:", sso@nChains),
    paste("Iterations:", sso@nIter),
    paste("Warmup:", sso@nWarmup),
    sep = "\n"
  )
}