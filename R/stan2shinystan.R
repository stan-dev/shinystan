# convert stanfit object to shinystan object

stan2shinystan <- function(stanfit, notes) {
  # notes: text to add to user_model_info slot

  stopifnot(requireNamespace("rstan", quietly = TRUE))

  if (!inherits(stanfit, "stanfit")) {
    name <- deparse(substitute(stanfit))
    stop(paste(name, "is not a stanfit object."))
  }

  samps_all <- rstan::extract(stanfit, permuted = FALSE, inc_warmup = TRUE)
  param_names <- dimnames(samps_all)[[3]]
  param_dims <- get_stanfit_param_dims(stanfit, param_names)

  stan_algorithm <- stanfit@stan_args[[1]]$algorithm
  if (!(stan_algorithm %in% c("NUTS", "HMC"))) {
    warning("Most features of shinyStan are only available for models using
            algorithm NUTS or algorithm HMC.")
  }

  slots <- list()
  slots$Class <- "shinystan"
  slots$model_name <- stanfit@model_name
  slots$param_names <- param_names
  slots$param_dims <- param_dims
  slots$param_groups <- names(param_dims)
  slots$samps_all <- samps_all
  slots$summary <- rstan::summary(stanfit)$summary
  slots$sampler_params <- rstan::get_sampler_params(stanfit)
  slots$nChains <- ncol(stanfit)
  slots$nIter <- nrow(samps_all) # total number of iterations (after thinning)
  slots$nWarmup <- floor(stanfit@sim$warmup / stanfit@sim$thin)
  if (!missing(notes)) slots$user_model_info <- notes
  slots$model_code <- rstan::get_stancode(stanfit)
  slots$stan_algorithm <- stan_algorithm

  do.call("new", slots)
}
