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
#' @param directory path to directory to be created to store modified shinyStan for shinyapps.io
#' or \code{function(param1,param2)}. See strong(Examples), below.
#' @param ... Arguments to pass to \code{\link[shinyapps]{deployApp}}.
#' @param y Optional. Vector of observations to use for graphical posterior 
#' predictive checking. 
#' @seealso \code{\link[shinyapps]{deployApp}}
#' @export
#' @examples
#' \dontrun{
#' }

deploy_shinystan <- function(sso, directory, y, ...) {
  
  has_shinyapps <- requireNamespace("shinyapps", quietly = TRUE)
  if (!has_shinyapps) stop("deploy_shinystan requires the shinyapps package.", call. = FALSE)
  
  # Missing: Need to clone shinystan_for_shinyapps repo from GitHub and place in directory
  
  shinystan_object <- sso
  save(shinystan_object, file = normalizePath(directory,"/shinystan_object.RData"))
  save(shinystan_object, file = normalizePath(directory,"/y.RData"))
  shinyapps::deployApp(appDir = normalizePath(directory), ...)
}