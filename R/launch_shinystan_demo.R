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

#' ShinyStan demo
#'
#' @export
#' @param rstudio Only relevant for RStudio users. The default 
#'   (\code{rstudio=FALSE}) is to launch the app in the default web browser 
#'   rather than RStudio's pop-up Viewer. Users can change the default to 
#'   \code{TRUE} by setting the global option \code{options(shinystan.rstudio = 
#'   TRUE)}.
#' @param ... Optional arguments to pass to \code{\link[shiny]{runApp}}.
#' @return An S4 shinystan object.
#'   
#' @seealso \code{\link{launch_shinystan}}, \code{\link{as.shinystan}}
#' 
#' @examples
#' \dontrun{
#' # launch demo but don't save a shinystan object
#' launch_shinystan_demo() 
#' 
#' # launch demo and save the shinystan object for the demo 
#' ssdemo <- launch_shinystan_demo()
#' }
#'

launch_shinystan_demo <- function(rstudio = getOption("shinystan.rstudio"), 
                                  ...) {
  demo_name <- "eight_schools"
  invisible(launch(get(demo_name), rstudio, ...))
}
