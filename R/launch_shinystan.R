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


#' Launch the ShinyStan app
#' 
#' Launch the ShinyStan app in the default web browser. RStudio users also have
#' the option of launching the app in RStudio's pop-up Viewer.
#' 
#' @export
#' @param object An object of class shinystan, stanfit, or stanreg. To use other
#'   types of objects first create a shinystan object using
#'   \code{\link{as.shinystan}}.
#' @param rstudio Only relevant for RStudio users. The default 
#'   (\code{rstudio=FALSE}) is to launch the app in the default web browser 
#'   rather than RStudio's pop-up Viewer. Users can change the default to 
#'   \code{TRUE} by setting the global option \code{options(shinystan.rstudio = 
#'   TRUE)}.
#' @param ... Optionally, arguments to pass to \code{\link[shiny]{runApp}}.
#' 
#' @return The \code{launch_shinystan} function is used for the side effect of 
#'   starting the ShinyStan app, but it also returns a shinystan object, an
#'   instance of S4 class \code{"shinystan"}.
#'
#' @seealso \code{\link{as.shinystan}} for creating shinystan objects and 
#'   \code{\link{launch_shinystan_demo}} to launch a demo.
#'   
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
  message("\nLoading... ",
          "for large models ShinyStan may take a few moments to launch.")
  if (is.stanreg(object) || is.stanfit(object))
    object <- as.shinystan(object)
  if (!is.shinystan(object))
    stop("'object' is not a valid input. See help('launch_shinystan').")
  
  invisible(launch(object, rstudio, ...))
}


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
#' @seealso \code{\link{launch_shinystan}} for launching ShinyStan using your 
#'   own shinystan objects
#'   
#'   \code{\link{as.shinystan}} to create shinystan objects
#' 
#' @examples
#' \dontrun{
#' # launch demo but don't save a shinystan object
#' launch_shinystan_demo() 
#' 
#' # launch demo and save the shinystan object for the demo 
#' sso_demo <- launch_shinystan_demo()
#' }
#'
launch_shinystan_demo <- function(rstudio = getOption("shinystan.rstudio"), 
                                  ...) {
  demo_name <- "eight_schools"
  invisible(launch(get(demo_name), rstudio, ...))
}

# Internal launch function 
# @param sso shinystan object
# @param rstudio launch in rstudio viewer instead of web browser? 
# @param ... passed to shiny::runApp
launch <- function(sso, rstudio = FALSE, ...) {
  stopifnot(is.shinystan(sso))
  launch.browser <- if (!rstudio) 
    TRUE else getOption("shiny.launch.browser", interactive())
  .sso_env$.shinystan_temp_object <- sso  # see zzz.R for .sso_env
  on.exit(.sso_env$.shinystan_temp_object <- NULL, add = TRUE)
  shiny::runApp(system.file("ShinyStan", package = "shinystan"), 
                launch.browser = launch.browser, ...)
}
