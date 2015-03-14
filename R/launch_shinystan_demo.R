#' Launch shinyStan app in demo mode
#'
#' The user will be presented with the option of launching the default
#' shinyStan demo (in which case the app will launch immediately)
#' or running a Stan demo model (\pkg{rstan}), after which shinyStan
#' will launch.
#'
#' @param ... Optional arguments to pass to \code{stan_demo} (\pkg{rstan}).
#'
#' @details After running \code{launch_shinystan_demo} you will also
#' have an S4 object of class \code{shinystan} in your Global Environment which can be
#' used with \code{launch_shinystan}.
#'
#' @note Unless you are using RStudio, \code{launch_shinystan} will open your
#' system's default web browser. For RStudio users \strong{shinyStan} will
#' launch in RStudio's (pop-up) Viewer pane. If you prefer to use \strong{shinyStan}
#' in your web browser (or if you are having trouble with the RStudio Viewer pane) you 
#' can click on 'Open in Browser' at the top of the Viewer pane.
#' @seealso \code{\link[shinyStan]{launch_shinystan}}, \code{\link[shinyStan]{as.shinystan}}, 
#' @export
#' @examples
#' \dontrun{
#' launch_shinystan_demo()
#' }
#'

launch_shinystan_demo <- function(...) {
  launch_demo <- function(object) {
    assign_shinystan(object)
    shiny::runApp(system.file("shinyStan", package = "shinyStan"))
  }
  cleanup_shinystan <- function(shinystan_object, out_name) {
    assign(out_name, shinystan_object, inherits = TRUE)
    rm(list = "shinystan_object", envir = globalenv())
  }

  choices <- c("Default shinyStan demo (launches immediately)",
               "Select a Stan demo (first runs RStan, then launches)")
  choice <- select.list(choices)
  if (choice == choices[1]) {
    demo_name <- "eight_schools"
    out_name <- paste0("shinystan_demo_object")
    on.exit(cleanup_shinystan(get("shinystan_object"), out_name))
    launch_demo(get(demo_name))
  } else {
    has_rstan <- requireNamespace("rstan", quietly = TRUE)
    if(!has_rstan) stop("You need to have the RStan package installed to use this option. Try runnning the default shinyStan demo instead.", call. = FALSE)
    rstan_demo <- rstan::stan_demo(...)
    launch_shinystan(rstan_demo)
  }
}
