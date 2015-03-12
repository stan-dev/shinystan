# convert mcmc.list object to shinystan object

mcmc2shinystan <- function(X, model_name = "unnamed model", burnin = 0, param_dims = list(),
                           model_code) {

  stopifnot(requireNamespace("coda", quietly = TRUE))

  Xname <- deparse(substitute(X))
  if (!inherits(X, "mcmc.list")) {
    stop (paste(Xname, "is not an mcmc.list."))
  }

  if (length(X) == 1) {
    return(chains2shinystan(list(coda:::as.matrix.mcmc(X))))
  }

  samps_array <- aperm(coda:::as.array.mcmc.list(X), c(1,3,2))
  dimnames(samps_array) <- list(iterations = 1:nrow(samps_array),
                                chains = paste0("chain:",1:ncol(samps_array)),
                                parameters = dimnames(samps_array)[[3]])
  param_names <- dimnames(X[[1]])[[2]]
  param_dims <- param_dims

  if (length(param_dims) != 0) {
    zeros <- sapply(1:length(param_dims), function(i) {
      0 %in% param_dims[[i]]
    })
    for (i in which(zeros)) {
      param_dims[[i]] <- numeric(0)
    }
  }

  if (length(param_dims) == 0) {
    param_dims <- list()
    param_dims[1:length(param_names)] <- NA
    names(param_dims) <- param_groups <- param_names
    for(i in 1:length(param_names)) {
      param_dims[[i]] <- numeric(0)
    }
  } else {
    param_groups <- names(param_dims)
  }
  slots <- list()
  slots$Class <- "shinystan"
  slots$model_name <- model_name
  slots$param_names <- param_names
  slots$param_dims <- param_dims
  slots$param_groups <- param_groups
  slots$samps_all <- samps_array
  slots$summary <- shinystan_monitor(samps_array, warmup = burnin)
  slots$sampler_params <- list(NA)
  slots$nChains <- ncol(samps_array)
  slots$nIter <- nrow(samps_array)
  slots$nWarmup <- burnin
  if (!missing(model_code)) slots$model_code <- model_code

  do.call("new", slots)
}
