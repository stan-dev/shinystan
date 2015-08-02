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


#' Launch shinyStan app
#' 
#' @export
#' 
#' @param object An object of class \code{shinystan} or \code{stanfit}. See
#'   \code{\link[shinyStan]{as.shinystan}} for converting to a 
#'   \code{shinystan} object (sso).
#' @param ... Optional arguments to pass to \code{\link[shiny]{runApp}}. See 
#'   Details (RStudio users in particular).
#' @return An S4 object of class \code{shinystan}.
#' 
#' @details Unless you are using RStudio, \code{launch_shinystan} will open the
#'   app in your system's default web browser. For RStudio users
#'   \strong{shinyStan} will launch in RStudio's (pop-up) Viewer pane by
#'   default. If you prefer to use \strong{shinyStan} in your web browser (or if
#'   you are having trouble with the RStudio Viewer pane) you can 
#'   specify \code{launch.browser = TRUE} in \code{...}. Alternatively, if 
#'   \strong{shinyStan} is open in the RStudio Viewer pane you can click on 
#'   'Open in Browser' (at the top left of the Viewer pane).
#'
#' @seealso \code{\link[shinyStan]{as.shinystan}}, 
#'   \code{\link[shinyStan]{launch_shinystan_demo}}
#' @examples
#' \dontrun{
#' # If X is a stanfit object (or shinystan object (sso))
#' X_sso <- launch_shinystan(X)
#'
#' # If X is not an sso or stanfit object
#' X_sso <- launch_shinystan(as.shinystan(X, model_name = "Example"))
#' }
#'
launch_shinystan <- function(object, ...) {
  name <- deparse(substitute(object))
  no_name <- substr(name, 1, 12) == "as.shinystan"
  if (missing(object)) 
    stop("Please specify a shinystan or stanfit object.")
  is_stanfit_object <- is_stan(object)
  if (!is_stanfit_object & !is.shinystan(object)) 
    stop(paste(name, "is not a shinystan or stanfit object."))
  message(paste("\n Loading... \n", 
                "For large models shinyStan may take a few moments to launch."))
  on.exit(cleanup_shinystan())
  launch(object, ...)
  invisible(return_sso())
}
