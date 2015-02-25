#' Launch shinyStan app in demo mode
#'
#' The user will be presented with the option of launching the default
#' shinyStan demo (in which case the app will launch immediately)
#' or running a Stan demo model (\pkg{rstanDemo}), after which shinyStan
#' will launch.
#'
#' @param ... Optional arguments to pass to \code{rstanDemo::stan_demo}.
#'
#' @details After running \code{launch_shinystan_demo} you will also
#' have an S4 object of class \code{shinystan} in your Global Environment which can be
#' used with \code{launch_shinystan}.
#'
#' @note Unless you are using RStudio, \code{launch_shinystan} will open your
#' system's default web browser. For RStudio users \strong{shinyStan} will
#' launch in RStudio's (pop-up) Viewer pane. If you prefer to view \strong{shinyStan}
#' in your web browser you can then click on 'Open in Browser' at the top of
#' the Viewer pane.
#' @export
#' @examples
#' \dontrun{
#' launch_shinystan_demo()
#' }
#'
#'

launch_shinystan_demo <- function(...) {
  launch_demo <- function(object) {
    shinystan_object <<- object
    shiny::runApp(system.file("shinyStan", package = "shinyStan"))
  }
  cleanup_shinystan <- function(shinystan_object, out_name) {
    assign(out_name, shinystan_object, inherits = TRUE)
    shinystan_object <<- NULL
    rm(list = "shinystan_object", envir = globalenv())
  }

  choices <- c("Default shinyStan demo (launches immediately)",
               "Select a Stan demo (first runs RStan, then launches)")
  choice <- select.list(choices)
  if (choice == choices[1]) {
    demo_name <- "eight_schools"
    out_name <- paste0("shinystan_demo_", demo_name)
    on.exit(cleanup_shinystan(shinystan_object, out_name))
    launch_demo(eight_schools)
  } else {
    has_rstanDemo <- requireNamespace("rstanDemo", quietly = TRUE)
    if (has_rstanDemo) {
      stanfit <- rstanDemo::stan_demo(...)
    } else {
      has_rstan <- requireNamespace("rstan", quietly = TRUE)
      if(!has_rstan) stop("You need to have the RStan package installed to use this option.")
      stanfit <- rstan::stan_demo(...)
    }

    launch_shinystan(stanfit)
  }
}
