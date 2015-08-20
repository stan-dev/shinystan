stanreg2shinystan <- function(X, ...) {
  stopifnot(inherits(X, "stanreg"))
  ok <- requireNamespace("rstanarm", quietly = TRUE)
  if (!ok) {
    message("Please install the rstanarm package to use this feature.")
    return(invisible(NULL))
  }
  sso <- stan2shinystan(X$stanfit, ...)
  sso@misc$pp_y <- if ("y" %in% names(X)) 
    X$y else model.response(model.frame(X))
  sso@misc$pp_yrep <- rstanarm::posterior_predict(X)
  sso
}