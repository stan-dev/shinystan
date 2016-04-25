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

#' Update an object created by the previous version of shinystan
#' 
#' If you encounter any errors when using an old shinystan object (\code{sso}) 
#' created by the previous version of \pkg{shinystan} you might need to run
#' \code{update_sso}. If \code{update_sso} does not resolve the problem and 
#' you still have the object (e.g. stanfit,  stanreg, mcmc.list) from which 
#' \code{sso} was originally created, you can create a new shinystan object 
#' using \code{\link{as.shinystan}}.
#' 
#' @export
#' @param sso An old shinystan object to update.
#' @return \code{sso}, updated.
#' 
update_sso <- function(sso) {
  stopifnot(is.shinystan(sso))
  sso@sampler_params <- .rename_sampler_param(sso@sampler_params, 
                                              oldname = "n_divergent__", 
                                              newname = "divergent__")
  sso
}
