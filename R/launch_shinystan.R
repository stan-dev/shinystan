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


#' Launch the 'ShinyStan' app
#' 
#' Launch the 'ShinyStan' app in the default web browser. 'RStudio' users also
#' have the option of launching the app in the pop-up Viewer.
#' 
#' @export
#' @param object The object to use. For the default method this can be an object
#'   of class \code{"shinystan"}, \code{"stanfit"}, or \code{"stanreg"}. To use
#'   other types of objects first create a shinystan object using
#'   \code{\link{as.shinystan}}.
#' @param rstudio Only relevant for 'RStudio' users. The default (\code{FALSE})
#'   is to launch the app in the user's default web browser rather than the 
#'   pop-up Viewer provided by 'RStudio'. Users can change the default to
#'   \code{TRUE} by setting the global option \code{options(shinystan.rstudio =
#'   TRUE)}.
#' @param ... Optional arguments passed to \code{\link[shiny]{runApp}}.
#' 
#' @return The \code{launch_shinystan} function is used for the side effect of 
#'   starting the 'ShinyStan' app, but it also returns a \code{shinystan}
#'   object, an instance of S4 class \code{"shinystan"}.
#'   
#' @template seealso-as.shinystan 
#' @template seealso-update_sso 
#' @template seealso-demo
#' 
#' @template reference-muth
#' @template reference-bayesvis
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
#' # First create shinystan object (see ?as.shinystan) for full details)
#' }
#'
launch_shinystan <- function(object, ...) {
  UseMethod("launch_shinystan")
}

#' @rdname launch_shinystan
#' @export
launch_shinystan.default <-
  function(object,
           ...,
           rstudio = getOption("shinystan.rstudio")) {
    if (!is.shinystan(object) && 
        !is.stanfit(object) && 
        !is.stanreg(object)) {
      stop("object not compatible with 'launch_shinystan'. ",
           "Try converting to a shinystan object first using 'as.shinystan'.")
    }
    object <- as.shinystan(object)
    message("\nLaunching ShinyStan interface... ",
            "for large models this  may take some time.")
    invisible(launch(object, rstudio, ...))
  }

#' @rdname launch_shinystan
#' @export
launch_shinystan.shinystan <-
  function(object,
           ...,
           rstudio = getOption("shinystan.rstudio")) {
    sso_check(object)
    message("\nLaunching ShinyStan interface... ",
            "for large models this  may take some time.")
    invisible(launch(object, rstudio, ...))
  }


#' 'ShinyStan' demo
#'
#' @aliases eight_schools
#' @export
#' @inheritParams launch_shinystan
#' @param demo_name The name of the demo. Currently \code{"eight_schools"} is 
#'   the only option, but additional demos may be available in future releases.
#'   \describe{
#'   \item{\code{eight_schools}}{Hierarchical meta-analysis model. See 
#'    \emph{Meta Analysis} chapter of the 'Stan' manual 
#'    \url{http://mc-stan.org/users/documentation/}.}
#'   }
#' @return An S4 shinystan object.
#'   
#' @template seealso-launch
#' @template seealso-as.shinystan
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
launch_shinystan_demo <- function(demo_name = "eight_schools",
                                  rstudio = getOption("shinystan.rstudio"),
                                  ...) {
  demo_name <- match.arg(demo_name)
  data(list = demo_name, package = "shinystan", envir = environment())
  invisible(launch(get(demo_name, inherits = FALSE), rstudio = rstudio, ...))
}

# Internal launch function 
# @param sso shinystan object
# @param rstudio launch in rstudio viewer instead of web browser? 
# @param ... passed to shiny::runApp
launch <- function(sso, rstudio = FALSE, ...) {
  launch.browser <- if (!rstudio) 
    TRUE else getOption("shiny.launch.browser", interactive())
  
  .sso_env$.SHINYSTAN_OBJECT <- sso  # see zzz.R for .sso_env
  on.exit(.sso_env$.SHINYSTAN_OBJECT <- NULL, add = TRUE)
  shiny::runApp(system.file("ShinyStanModules", package = "shinystan"), 
                launch.browser = launch.browser, ...)
}
