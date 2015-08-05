# This file is part of shinystan
# Copyright (C) Jonah Gabry
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
#' 
#' @param object An object of class shinystan, stanfit, or 
#'   \code{stanreg}. See \code{\link{as.shinystan}} for converting other
#'   objects to a shinystan object (sso).
#' @param rstudio For RStudio users, should the app launch in RStudio's Viewer? 
#'   The default is to launch the app in the user's default web browser instead 
#'   of the RStudio viewer unless the user has set 
#'   \code{options(shinystan.rstudio = TRUE)}, in which case the default is to
#'   launch in RStudio's Viewer.
#' @param ... Optional arguments to pass to \code{\link[shiny]{runApp}}.
#' @return An S4 shinystan object.
#'
#' @seealso \code{\link{as.shinystan}}, 
#'   \code{\link{launch_shinystan_demo}}, \code{\link{shinystan_options}}
#' @examples
#' \dontrun{
#' # If X is a stanfit object (or shinystan object (sso))
#' X_sso <- launch_shinystan(X)
#'
#' # If X is not an sso or stanfit object
#' X_sso <- launch_shinystan(as.shinystan(X, model_name = "Example"))
#' }
#'
launch_shinystan <- function(object, rstudio = getOption("shinystan.rstudio"), 
                             ...) {
  name <- deparse(substitute(object))
  no_name <- substr(name, 1, 12) == "as.shinystan"
  if (missing(object)) 
    stop("Please specify a shinystan or stanfit object.")
  if (inherits(object, "stanreg"))
    object <- object$stanfit
  if (inherits(object, "stanfit"))
    object <- stan2shinystan(object)
  if (!is.shinystan(object))
    stop(paste(name, "is not a valid input. See ?launch_shinystan"))
  message(paste("\n Loading... \n", 
                "For large models ShinyStan may take a few moments to launch."))
  on.exit(cleanup_shinystan())
  invisible(launch(object, rstudio, ...))
}
