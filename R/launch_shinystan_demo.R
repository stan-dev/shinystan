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

#' Launch shinyStan app in demo mode
#'
#' @export
#'
#' @param ... Optional arguments to pass to \code{\link[shiny]{runApp}}. See
#' Details.
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
#' @seealso \code{\link[shinyStan]{launch_shinystan}},
#'   \code{\link[shinyStan]{as.shinystan}},
#' @examples
#' \dontrun{
#' launch_shinystan_demo()
#' ssdemo <- launch_shinystan_demo()
#' }
#'

launch_shinystan_demo <- function(...) {
  demo_name <- "eight_schools"
  on.exit(cleanup_shinystan())
  launch(get(demo_name), ...)
  invisible(return_sso())
}
