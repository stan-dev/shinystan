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
#' @param object An object of class \code{shinystan} or \code{stanfit}.
#' See \code{\link[shinyStan]{as.shinystan}} for how to easily convert other types
#' of objects to \code{shinystan} objects.
#' 
#' @return If \code{object} is a \code{shinystan} object then \code{object} will be 
#' returned (or a slightly modified version of \code{object} reflecting any changes saved while 
#' using the app). If \code{object} is a \code{stanfit} object then, in addition to launching
#' the \strong{shinyStan} app, an S4 object of class \code{shinystan} is returned 
#' (see \strong{Note}).
#' 
#' @note The following note is only relevant for Stan users. 
#' 
#' If \code{object} is a \code{stanfit} object then the returned \code{shinystan} object
#' will be named \code{object_shinystan}. For example, if \code{object} is a \code{stanfit} object
#' named \code{my_stanfit} then the returned \code{shinystan} object will be named \code{my_stanfit_shinystan}. 
#' To avoid overwriting existing \code{shinystan} objects, if there is already an object named 
#' \code{my_stanfit_shinystan} then the \code{shinystan} objected returned will be named 
#' \code{my_stanfit_shinystan.1}. If there is also already an object named 
#' \code{my_stanfit_shinystan.1} then the returned \code{shinystan} object will be named 
#' \code{my_stanfit_shinystan.2}, etc.
#' 
#' To avoid creating multiple \code{shinystan} objects for a single \code{stanfit} object
#' you should use the returned \code{shinystan} object the next time you want to launch \strong{shinyStan}
#' to explore the same \code{stanfit} object. For example, running \code{launch_shinystan(my_stanfit)}
#' three times will create the \code{shinystan} objects \code{my_stanfit_shinystan}, 
#' \code{my_stanfit_shinystan.1}, and \code{my_stanfit_shinystan.2}. On the other hand, once
#' \code{my_stanfit_shinystan} is created after the first launch, it can then be used to launch 
#' \strong{shinyStan} as many times as you want without creating duplicate objects. 
#' \code{launch_shinystan(my_stanfit_shinystan)} will always return \code{my_stanfit_shinystan} 
#' (updated to reflect any saved changes while running the app). The only reasons to continue to use 
#' \code{my_stanfit} as the argument to \code{launch_shinystan} (instead of using \code{my_stanfit_shinystan})
#' are 1) you intentionally want to create extra \code{shinystan} objects, or 2) you keep deleting 
#' \code{my_stanfit_shinystan} each time it's created. 
#' 
#' @details Unless you are using RStudio, \code{launch_shinystan} will open the app
#' in your system's default web browser. For RStudio users \strong{shinyStan} will
#' launch in RStudio's (pop-up) Viewer pane by default. If you prefer to use \strong{shinyStan}
#' in your web browser (or if you are having trouble with the RStudio Viewer pane) you 
#' can click on 'Open in Browser' at the top of the Viewer pane.
#'
#' @seealso \code{\link[shinyStan]{as.shinystan}}, \code{\link[shinyStan]{launch_shinystan_demo}}
#' @export
#' @examples
#' \dontrun{
#' # If X is a shinystan object or stanfit object then
#' # to launch the app just run
#' launch_shinystan(X)
#'
#' # If X is not a shinystan or stanfit object then to launch the app first
#' # convert X to a shinystan object using the as.shinystan function
#' X_shinystan <- as.shinystan(X)
#' launch_shinystan(X_shinystan)
#' }
#'
launch_shinystan <- function(object) {
  message("\n Loading... \n For large models shinyStan may take a few moments to launch.")
  
  name <- deparse(substitute(object))
  no_name <- substr(name, 1, 12) == "as.shinystan"

  if (missing(object)) stop("Please specify a shinystan or stanfit object.")
  
  is_stanfit_object <- is_stan(object)

  if (!is_stanfit_object & !is.shinystan(object)) stop(paste(name, "is not a shinystan or stanfit object."))

  out_name <- if (is_stanfit_object) paste0(name,"_shinystan") else name
  if (no_name) out_name <- "unnamed_shinystan"
  
  on.exit(cleanup_shinystan(get("shinystan_object"), out_name, is_stanfit_object))
  launch(object)
}
