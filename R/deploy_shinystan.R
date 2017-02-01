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
#' Requires a (free or paid) ShinyApps account. Visit
#' \url{http://www.shinyapps.io/} to sign up.
#'
#' @export
#' @template args-sso
#' @param appName The name to use for the application. Application names must be
#'   at least four characters long and may only contain letters, numbers, dashes
#'   and underscores.
#' @param account shinyapps.io account username. Only required if more than one
#'   account is configured on the system.
#' @param ... Optional arguments. See Details.
#' @param deploy Should the app be deployed? The only reason for this to be
#'   \code{FALSE} is if you just want to check that the preprocessing before
#'   deployment is successful.
#'
#' @return \link[=invisible]{Invisibly}, \code{TRUE} if deployment succeeded
#'   (did not encounter an error) or, if \code{deploy} argument is set to
#'   \code{FALSE}, the path to the temporary directory containing the app ready
#'   for deployment (also invisibly).
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
#' @seealso The example in the \emph{Deploying to shinyapps.io} vignette that
#'   comes with this package.
#'
#'   \url{http://www.shinyapps.io/} to sign up for a free or paid ShinyApps
#'   account and for details on how to configure your account on your local
#'   system using RStudio's \pkg{\link[rsconnect]{rsconnect}} package.
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
#' @importFrom rsconnect deployApp
#'
deploy_shinystan <- function(sso, appName, account = NULL, ..., deploy = TRUE) {
  sso_check(sso)
  if (missing(appName))
    stop("'appName' is required.")

  # copy contents to temporary directory and write necessary additional lines to
  # ui, server, and global
  appDir <- tempdir()
  deployDir <- file.path(appDir, "ShinyStan")
  contents <- system.file("ShinyStan", package = "shinystan")
  file.copy(from = contents, to = appDir, recursive = TRUE)

  server_pkgs <- c(
    "shiny",
    "shinyjs",
    "colourpicker",
    "markdown",
    "shinythemes"
  )
  ui_pkgs <- c(
    server_pkgs,
    "ggplot2",
    "bayesplot",
    "gtools",
    "reshape2",
    "dygraphs",
    "xts",
    "xtable",
    "gridExtra",
    "DT",
    "threejs"
  )
  server_lines <- paste0("library(", server_pkgs, ");")
  ui_lines <- paste0("library(", ui_pkgs, ");")
  global_lines <- paste(
    "load('sso.RData');",
    "if (file.exists('y.RData')) load('y.RData')"
  )
  for (ff in c("ui", "server", "global")) {
    file_name <- file.path(deployDir, paste0(ff, ".R"))
    fconn <- file(file_name, 'r+')
    original_content <- readLines(fconn)
    new_lines <- get(paste0(ff, "_lines"))
    writeLines(c(new_lines, original_content), con = fconn)
    close(fconn)
  }

  # save sso to deployDir
  .SHINYSTAN_OBJECT <- sso
  save(.SHINYSTAN_OBJECT, file = file.path(deployDir, "sso.RData"))

  # save ppcheck_data and set ppcheck defaults
  pp <- list(...)
  if ("ppcheck_data" %in% names(pp)) {
    y <- pp$ppcheck_data
    save(y, file = file.path(deployDir, "y.RData"))
    if ("ppcheck_yrep" %in% names(pp))
      set_ppcheck_defaults(
        appDir = deployDir,
        yrep_name = pp$ppcheck_yrep,
        y_name = "y"
      )
  }

  if (!deploy)
    return(invisible(deployDir))

  rsconnect::deployApp(
    appDir = deployDir,
    appName = appName,
    account = account,
    lint = TRUE
  )
}



# functions to set defaults for ppcheck shiny::selectInput for y and y_rep
set_ppcheck_defaults <- function(appDir, yrep_name, y_name = "y") {
  stopifnot(is.character(yrep_name), is.character(y_name),
            length(yrep_name) == 1, length(y_name) == 1)
  fileDir <- file.path(appDir, "server_files", "pages", "diagnose", "ppcheck", "ui")
  ppc_file <- file.path(fileDir, "pp_get_y_and_yrep.R")
  if (file.exists(ppc_file)) {
    file.remove(ppc_file)
    file.create(ppc_file)
  }
  .write_files(files = ppc_file, lines = .ppc_lines(y_name, yrep_name))
}

.write_files <- function(files, lines) {
  stopifnot(length(files) == length(lines))
  for (f in seq_along(files)) {
    fileConn <- file(files[f])
    writeLines(lines[f], fileConn)
    close(fileConn)
  }
}

.ppc_lines <- function(y_name = "y", yrep_name) {
  paste0(
    "output$ui_pp_get_y <- renderUI({
      choices <- objects(envir = .GlobalEnv)
      selectizeInput('y_name', label = span(style = 'color: #337ab7;', 'y, a vector of observations'),
      choices = c('', choices),
      selected = '", y_name,"')
      })

    output$ui_pp_get_yrep <- renderUI({
      choices <- PARAM_NAMES
      choices <- strsplit(choices, split = '[', fixed = TRUE)
      choices <- lapply(choices, function(i) return(i[1]))
      choices <- unique(unlist(choices))
      selectizeInput('yrep_name',
      label = span(style = 'color: #337ab7;', 'y_rep, posterior predictive replications'),
      choices = c('', choices),
      selected = '", yrep_name,"')
    })"
  )
}
