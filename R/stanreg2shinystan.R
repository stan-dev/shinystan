stanreg2shinystan <- function(X, ...) {
  stopifnot(is.stanreg(X))
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
  
  if (exists("posterior_predict")) {
    sso@misc$pp_yrep <- posterior_predict(X)
  } else {
    stop("Please load or install the 'rstanarm' package.")
  }
  sso
}