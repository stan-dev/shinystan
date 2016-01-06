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


#' Deploy a ShinyStan app on the web using shinyapps.io by RStudio
#' 
#' Requires a (free or paid) shinyapps.io account. Visit 
#' \url{http://www.shinyapps.io/} to sign up and for details on how to configure
#' your account on your local system using RStudio's \pkg{rsconnect} package.
#' 
#' @export
#' @param sso A shinystan object.
#' @param appName The name to use for the application. Application names must be
#'   at least four characters long and may only contain letters, numbers, dashes
#'   and underscores.
#' @param account shinyapps.io account username. Only required if more than one 
#'   account is configured on the system.
#' @param ... Optional arguments. See Details.
#' 
#' @details In \code{...}, the arguments \code{ppcheck_data} and 
#'   \code{ppcheck_yrep} can be specified. \code{ppcheck_data} should be a
#'   vector of observations to use for graphical posterior predictive checking
#'   and \code{ppcheck_yrep} should be a character string naming the parameter
#'   in \code{sso} containing the posterior predictive simulations/replications.
#'   The value of \code{ppcheck_yrep} is only used to preselect the appropriate
#'   parameter/generated quantity to use for the posterior predictive checking. 
#'   \code{ppcheck_yrep} (but not \code{ppcheck_data}) can also be set
#'   interactively on shinyapps.io when using the app.
#' 
#' @note See the 'Deploying to shinyapps.io' vignette for a more detailed 
#'   example.
#' 
#' @examples
#' \dontrun{
#' # For this example assume sso is the name of the shinystan object for 
#' # the model you want to use. Assume also that you want to name your app 
#' # 'my-model' and that your shinyapps.io username is 'username'. 
#'
#' deploy_shinystan(sso, appName = "my-model", account = "username") 
#'
#' # If you only have one ShinyApps account configured then you can also omit 
#' # the 'account' argument. 
#'
#' deploy_shinystan(sso, appName = "my-model")
#' }
#' 

deploy_shinystan <- function(sso, appName, account = NULL, ...) {
  sso_check(sso)
  if (missing(appName)) 
    stop("Please specify a name for your app using the 'appName' argument")

  # copy contents to temporary directory and write necessary additional lines to
  # ui, server, and global
  appDir <- tempdir()
  deployDir <- file.path(appDir, "ShinyStan")
  contents <- system.file("ShinyStan", package = "shinystan")
  file.copy(from = contents, to = appDir, recursive = TRUE)
  server_pkgs <- c("shiny", "shinyjs", "markdown", "shinythemes")
  ui_pkgs <- c(server_pkgs, "ggplot2", "gtools", "reshape2", 
               "dygraphs", "xts", "xtable", "gridExtra", "DT", "threejs")
  server_lines <- paste0("library(", server_pkgs,");")
  ui_lines <- paste0("library(", ui_pkgs,");")
  global_lines <- paste("load('shinystan_temp_object.RData');", 
                        "if (file.exists('y.RData')) load('y.RData')")
  for (ff in c("ui", "server", "global")) {
    file_name <- file.path(deployDir, paste0(ff, ".R"))
    fconn <- file(file_name, 'r+') 
    original_content <- readLines(fconn) 
    if (ff %in% c("ui", "server")) {
      sel <- grep(".shinystan_temp_object", original_content)
      original_content <- original_content[-sel] 
    }
    new_lines <- get(paste0(ff, "_lines"))
    writeLines(c(new_lines, original_content), con = fconn) 
    close(fconn) 
  }

  # save shinystan_object to deployDir
  object <- sso
  save(object, file = file.path(deployDir, "shinystan_temp_object.RData"))
  deploy <- getFromNamespace("deployApp", "rsconnect")
  # save ppcheck_data and set ppcheck defaults
  pp <- list(...)
  if ("ppcheck_data" %in% names(pp)) {
    y <- pp$ppcheck_data
    save(y, file = file.path(deployDir, "y.RData"))
    if ("ppcheck_yrep" %in% names(pp))
      set_ppcheck_defaults(appDir = deployDir, yrep_name = pp$ppcheck_yrep, 
                           y_name = "y")
  }
  deploy(appDir = deployDir, appName = appName, account = account, lint = TRUE)
}
