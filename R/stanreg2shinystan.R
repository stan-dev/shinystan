stanreg2shinystan <- function(X, ...) {
  stopifnot(is.stanreg(X))
  ok <- requireNamespace("rstanarm", quietly = TRUE)
  if (!ok) {
    message("Please install the rstanarm package to use this feature.")
    return(invisible(NULL))
  }
  sso <- stan2shinystan(X$stanfit, ...)
  
  param_names <- sso@param_names
  param_dims <- list()
  param_dims[1:length(param_names)] <- NA
  names(param_dims) <- param_names
  for(i in 1:length(param_names)) {
    param_dims[[i]] <- numeric(0)
  }
  sso@param_dims <- param_dims
  
  sso@misc$pp_y <- if ("y" %in% names(X)) 
    X$y else model.response(model.frame(X))
  sso@misc$pp_yrep <- rstanarm::posterior_predict(X)
  sso
}