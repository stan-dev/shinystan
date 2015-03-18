# Convert 3D array to shinystan object
# 
# @param X A 3D array of posterior simulations with dimensions corresponding to 
# iterations, chains, and parameters, in that order.
# @param model_name A character string giving a name for the model
# @param burnin The number of burnin (warmup) iterations. Should only be specified if the 
# burnin samples are included in \code{X}.
# @param param_dims Rarely used and never necessary. A named list giving the dimensions for 
# all parameters. For scalar parameters use \code{0} as the dimension. 
# See Examples in \code{\link[shinyStan]{as.shinystan}}.
# @param model_code A character string with the code you used to run your model. This can 
# also be added to your \code{shinystan} object later using the 
# \code{\link[shinyStan]{include_model_code}} function. 
# See \code{\link[shinyStan]{include_model_code}} for additional formatting instructions. 
# After launching the app \code{model_code} will be viewable in the \strong{Model Code} tab.
# 
# @return An object of class \code{shinystan} that can be used with 
# \code{\link[shinyStan]{launch_shinystan}}. 
# 

array2shinystan <- function(X, model_name = "unnamed model", burnin = 0, param_dims = list(),
                            model_code) {

  Xname <- deparse(substitute(X))
  if (!is.array(X)) {
    stop (paste(Xname, "is not an array"))
  }
  if (length(dim(X)) != 3) {
    stop (paste(Xname, "must be an array with 3 dimensions"))
  }

  if (is.null(dimnames(X)[[3]])) {
    dimnames(X)[[3]] <- paste0("V", 1:dim(X)[3])
  }

  dimnames(X) <- list(iterations = 1:nrow(X),
                                chains = paste0("chain:",1:ncol(X)),
                                parameters = dimnames(X)[[3]])
  param_names <- dimnames(X)[[3]]
  param_dims <- param_dims
  if (length(param_dims) == 0) {
    param_dims <- list()
    param_dims[1:length(param_names)] <- NA
    names(param_dims) <- param_groups <- param_names
    for(i in 1:length(param_names)) {
      param_dims[[i]] <- numeric(0)
    }
  } else {
    zeros <- sapply(1:length(param_dims), function(i) {
      0 %in% param_dims[[i]]
    })
    for (i in which(zeros)) {
      param_dims[[i]] <- numeric(0)
    }

    param_groups <- names(param_dims)
  }

  slots <- list()
  slots$Class <- "shinystan"
  slots$model_name <- model_name
  slots$param_names <- param_names
  slots$param_dims <- param_dims
  slots$param_groups <- param_groups
  slots$samps_all <- X
  slots$summary <- shinystan_monitor(X, warmup = burnin)
  slots$sampler_params <- list(NA)
  slots$nChains <- ncol(X)
  slots$nIter <- nrow(X)
  slots$nWarmup <- burnin
  if (!missing(model_code)) slots$model_code <- model_code

  do.call("new", slots)
}
