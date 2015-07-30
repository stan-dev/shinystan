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
#' Also see the 'Deploying to shinyapps.io' vignette for a 
#' step-by-step guide.  
#' 
#' @param sso The \code{shinystan} object (\code{sso}) for the model you want to use. 
#' @param account ShinyApps account username. Not required if only one 
#' ShinyApps account is configured on the system. 
#' @param appName The name to use for the application as a character string. 
#' Application names must be at least four characters long and may only contain 
#' letters, numbers, dashes and underscores.
#' @param ppcheck_data Optional vector of observations to use for 
#' graphical posterior predictive checking. 
#' @param ppcheck_yrep Optional character string naming the parameter in \code{sso}
#' containing the posterior predictive simulations/replications. This is only used to
#' preselect ppcheck_yrep as the parameter to use for the posterior predictive checking.
#' This can also be set interactively while using the app. 
#' 
#' @note See the 'Deploying to shinyapps.io' vignette for more detailed
#' examples. 
#' 
#' @seealso \code{\link[shinyapps]{deployApp}}, \code{\link[shinyapps]{accounts}}
#' @export
#' @examples
#' \dontrun{
#' # For this example assume my_sso is the name of the shinystan object for 
#' # the model you want to use. Assume also that you want to name your app 
#' # 'my-model' and that your ShinyApps username is 'username'. 
#'
#' deploy_shinystan(sso = my_sso, appName = "my-model", account = "username") 
#'
#' # If you only have one ShinyApps account configured then you also omit 
#' # the 'account' argument. 
#'
#' deploy_shinystan(sso = my_sso, appName = "my-model")
#' }
#' 

deploy_shinystan <- function(sso, account, appName, ppcheck_data, ppcheck_yrep) {
  sso_check(sso)
  
  # check for possible problems
  has_shinyapps <- requireNamespace("shinyapps", quietly = TRUE)
  if (!has_shinyapps) stop("Deploying a shinyStan app requires the shinyapps package. 
                           To download the package use devtools::install_github('rstudio/shinyapps')", 
                           call. = FALSE)
  
  if (missing(appName)) stop("Please specify a name for your app using the 'appName' argument", call. = FALSE)
  if (missing(account)) account <- NULL
  
  # copy from shinyStanApp_contents to temporary directory
  appDir <- tempdir()
  deployDir <- file.path(appDir, "shinyStan")
  contents <- system.file("shinyStan", package = "shinyStan")
  file.copy(from = contents, to = appDir, recursive = TRUE)
  rmv <- c("ui", "server", "global")
  file.remove(file.path(deployDir, paste0(rmv,".R")))
  oldnames <- paste0(rmv, "_for_shinyapps.R")
  for (j in seq_along(oldnames)) {
    file.rename(from = file.path(deployDir, "shinyapps", oldnames[j]), 
                to = file.path(deployDir, paste0(rmv[j],".R")))
  }

  # save shinystan_object to shinyStanApp_contents
  shinystan_object <- sso
  save(shinystan_object, file = file.path(deployDir, "shinystan_object.RData"))
  
  # save ppcheck_data and set ppcheck defaults 
  if (!missing(ppcheck_data)) {
    y <- ppcheck_data
    save(y, file = file.path(deployDir, "y.RData"))
    
    if (!missing(ppcheck_yrep)) {
      set_ppcheck_defaults(appDir = deployDir, yrep_name = ppcheck_yrep, y_name = "y")
    }
  }
  
  # deploy
  shinyapps::deployApp(appDir = deployDir, appName = appName, account = account, lint = FALSE)
}
