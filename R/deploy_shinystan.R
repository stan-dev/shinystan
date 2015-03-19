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



#' Deploy to shinyapps.io
#'
#' @param sso The \code{shinystan} object to use.
#' @param directory Path to shinystan_for_shinyapps library See \strong{Details}.
#' @param ppcheck_data Optional. Vector of observations to use for graphical posterior 
#' predictive checking. 
#' @param ... Arguments to pass to \code{\link[shinyapps]{deployApp}}.
#' 
#' @details In order to deploy a shinyStan app to shinyapps.io you first 
#' need to download the \code{shinystan_for_shinyapps} library, which is 
#' available at https://github.com/stan-dev/shinystan/releases. 
#' 
#' @note With one exception, all shinyStan features should work properly on shinyapps.io. 
#' The exception is the trivariate 3D scatterplot, which is not available at this time.  
#' 
#' @seealso \code{\link[shinyapps]{deployApp}}
#' @export
#' 

deploy_shinystan <- function(sso, directory, ppcheck_data, ...) {
  
  has_shinyapps <- requireNamespace("shinyapps", quietly = TRUE)
  if (!has_shinyapps) stop("deploy_shinystan requires the shinyapps package.", call. = FALSE)
  
  # Missing: Need to clone shinystan_for_shinyapps repo from GitHub and place in directory
  
  shinystan_object <- sso
  y <- ppcheck_data
  save(shinystan_object, file = normalizePath(directory,"/shinystan_object.RData"))
  save(y, file = normalizePath(directory,"/y.RData"))
  shinyapps::deployApp(appDir = normalizePath(directory), ...)
}