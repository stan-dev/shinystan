#' Launch shinyStan app
#'
#' @param object An object of class \code{shinystan} or \code{stanfit}.
#' See \code{\link[shinyStan]{as.shinystan}} for how to easily convert other types
#' of objects to \code{shinystan} objects.
#' @details If \code{object} is a \code{stanfit} object then, in addition to launching
#' the shinyStan app, this function creates an S4 object of class \code{shinystan} in
#' the Global Environment.
#' @note Unless you are using RStudio, \code{launch_shinystan} will open the app
#' in your system's default web browser. For RStudio users \strong{shinyStan} will
#' launch in RStudio's (pop-up) Viewer pane by default. Once open in the Viewer pane
#' it can then be opened in your web browser if you prefer by clicking on 'Open in Browser'
#' at the top of the Viewer pane. 
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

  launch <- function(object) {
    shinystan_object <<- if (is.shinystan(object)) object else stan2shinystan(object)
    shiny::runApp(system.file("shinyStan", package = "shinyStan"))
  }

  cleanup_shinystan <- function(shinystan_object, out_name) {
    assign(out_name, shinystan_object, inherits = TRUE)
    shinystan_object <<- NULL
    rm(list = "shinystan_object", envir = globalenv())
  }

  is_stan <- function(X) inherits(X, "stanfit")
  name <- deparse(substitute(object))
  no_name <- substr(name, 1, 12) == "as.shinystan"

  if (missing(object)) stop("Please specify a shinystan or stanfit object.")

  if (!is_stan(object) & !is.shinystan(object)) {
    stop(paste(name, "is not a shinystan or stanfit object."))
  }

  out_name <- if (is_stan(object)) paste0(name,"_shinystan") else name
  if (no_name) out_name <- "unnamed_shinystan"
  
  on.exit(cleanup_shinystan(shinystan_object, out_name))
  launch(object)
}
