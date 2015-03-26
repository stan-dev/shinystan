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
#' Requires a ShinyApps account. Visit http://www.shinyapps.io/ to sign up.
#'
#' @param sso The \code{shinystan} object for the model you want to use. 
#' @param account ShinyApps account username. Not required if only one 
#' ShinyApps account is configured on the system. 
#' See \code{\link[shinyapps]{deployApp}} and \code{\link[shinyapps]{accounts}}. 
#' @param appName The name to use for the application as a character string. Application
#' names must be at least four characters long and may only contain letters, numbers,
#' dashes and underscores.
#' @param appDir Path to shinystan_for_shinyapps library. Defaults to working directory. 
#' See \strong{Details}.
#' @param ppcheck_data Optional vector of observations to use for graphical posterior 
#' predictive checking. 
#' @param ppcheck_yrep Optional character string naming the parameter in \code{sso}
#' containing the posterior predictive simulations/replications. This is only used to
#' preselect ppcheck_yrep as the parameter to use for the posterior predictive checking.
#' This can also be set manually while using the app. 
#' 
#' @details In order to deploy a shinyStan app to shinyapps.io you first 
#' need to download the \code{shinystan_for_shinyapps} library, which is 
#' available at https://github.com/stan-dev/shinystan/releases. You will 
#' also need to set up a ShinyApps account (see http://www.shinyapps.io/).  
#' 
#' @note With one exception, all shinyStan features should work properly on shinyapps.io. 
#' The exception is the trivariate 3D scatterplot, which is not available in shinyStan apps
#' on shinyapps.io at this time.  
#' 
#' @seealso \code{\link[shinyapps]{deployApp}}, \code{\link[shinyapps]{accounts}}
#' @export
#' @examples
#' \dontrun{
#' 
#' # For this example assume my_sso is the name of the shinystan object
#' # you want to use and that your ShinyApps username is 'username'.
#'
#' # If we first set the working directory to be 'shinystan_for_shinyapps' 
#' # we don't need to specify the appDir argument   
#' setwd(Path to shinystan_for_shinyapps folder) 
#' deploy_shinystan(my_sso, appName = "my_shinystan_app", account = "username")
#' }

deploy_shinystan <- function(sso, account, appName, appDir = getwd(), ppcheck_data, ppcheck_yrep) {
  
  has_shinyapps <- requireNamespace("shinyapps", quietly = TRUE)
  if (!has_shinyapps) stop("Deploying a shinyStan app requires the shinyapps package. 
                           To download the package use devtools::install_github('rstudio/shinyapps')", 
                           call. = FALSE)

  sso_name <- deparse(substitute(sso))
  if (!is.shinystan(sso)) stop(paste(sso_name, "is not a shinystan object"), call. = FALSE)
  if (missing(appName)) stop("Please specify a name for your app using the 'appName' argument", call. = FALSE)
  if (missing(account)) account <- NULL
  
  appDir <- normalizePath(appDir)
  shinystan_object <- sso
  save(shinystan_object, file = file.path(appDir, "shinystan_object.RData"))
  
  if (!missing(ppcheck_data)) {
    y <- ppcheck_data
    save(y, file = file.path(appDir, "y.RData"))
    
    if (!missing(ppcheck_yrep)) {
      set_ppcheck_defaults(appDir = appDir, yrep_name = ppcheck_yrep, y_name = "y")
    }
  }
  
  shinyapps::deployApp(appDir = appDir, appName = appName, account = account, lint = FALSE)
}