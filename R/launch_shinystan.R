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
#' launch in RStudio's (pop-up) Viewer pane by default. If you prefer to use \strong{shinyStan}
#' in your web browser (or if you are having trouble with the RStudio Viewer pane) you 
#' can click on 'Open in Browser' at the top of the Viewer pane.
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
  
  name <- deparse(substitute(object))
  no_name <- substr(name, 1, 12) == "as.shinystan"

  if (missing(object)) stop("Please specify a shinystan or stanfit object.")
  
  is_stanfit_object <- is_stan(object)

  if (!is_stanfit_object & !is.shinystan(object)) stop(paste(name, "is not a shinystan or stanfit object."))

  out_name <- if (is_stanfit_object) paste0(name,"_shinystan") else name
  if (no_name) out_name <- "unnamed_shinystan"
  
  on.exit(cleanup_shinystan(get("shinystan_object"), out_name, is_stanfit_object))
  launch(object)
}
