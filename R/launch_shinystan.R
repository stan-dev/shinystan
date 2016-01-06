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

#' ShinyStan app
#' 
#' @export
#' @param object An object of class shinystan, stanfit, or stanreg. See
#'   \code{\link{as.shinystan}} for converting other objects to a shinystan
#'   object (sso).
#' @param rstudio Only relevant for RStudio users. The default 
#'   (\code{rstudio=FALSE}) is to launch the app in the default web browser 
#'   rather than RStudio's pop-up Viewer. Users can change the default to 
#'   \code{TRUE} by setting the global option \code{options(shinystan.rstudio = 
#'   TRUE)}.
#' @param ... Optional arguments to pass to \code{\link[shiny]{runApp}}.
#' @return An S4 shinystan object.
#'
#' @seealso \code{\link{as.shinystan}}, \code{\link{launch_shinystan_demo}}
#' @examples
#' \dontrun{
#' #######################################
#' # Example 1: 'sso' is a shinystan object
#' #######################################
#' 
#' # Just launch shinystan
#' launch_shinystan(sso)
#' 
#' # Launch shinystan and replace sso with an updated version of itself
#' # if any changes are made to sso while using the app
#' sso <- launch_shinystan(sso)
#' 
#' # Launch shinystan but save any changes made to sso while running the app
#' # in a new shinystan object sso2. sso will remained unchanged. 
#' sso2 <- launch_shinystan(sso) 
#' 
#' #######################################
#' # Example 2: 'sf' is a stanfit object
#' #######################################
#' 
#' # Just launch shinystan
#' launch_shinystan(sf)
#' 
#' # Launch shinystan and save the resulting shinystan object
#' sf_sso <- launch_shinystan(sf)
#' 
#' # Now sf_sso is a shinystan object and so Example 1 (above) applies when
#' # using sf_sso. 
#' 
#' #######################################
#' # Example 3: 'fit' is an mcmc.list, array or list of matrices
#' #######################################
#'
#' # First create shinystan object (see ?as.shinystan for full details)
#' fit_sso <- as.shinystan(fit, model_name = "Example")
#' 
#' # Now fit_sso is a shinystan object and so Example 1 (above) applies.
#' }
#'
launch_shinystan <- function(object, rstudio = getOption("shinystan.rstudio"), 
                             ...) {
  name <- deparse(substitute(object))
  no_name <- substr(name, 1, 12) == "as.shinystan"
  if (missing(object)) 
    stop("Please specify a shinystan or stanfit object.", call. = FALSE)
  message("\nLoading... \n", 
          "Note: for large models ShinyStan may take a few moments to launch.")
  
  if (inherits(object, "stanreg"))
    object <- stanreg2shinystan(object)
  if (inherits(object, "stanfit"))
    object <- stan2shinystan(object)
  if (!is.shinystan(object))
    stop(paste(name, "is not a valid input. See ?launch_shinystan"))
  invisible(launch(object, rstudio, ...))
}
