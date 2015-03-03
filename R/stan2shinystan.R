# convert stanfit object to shinystan object

stan2shinystan <- function(stanfit, model_name, notes) {
  # notes: text to add to user_model_info slot
  
  stopifnot(requireNamespace("rstan", quietly = TRUE))
  
  if (!inherits(stanfit, "stanfit")) {
    name <- deparse(substitute(stanfit))
    stop(paste(name, "is not a stanfit object."))
  }
  
  stan_args <- stanfit@stan_args[[1]]
  from_cmdstan_csv <- ("engine" %in% names(stan_args))
  
  stan_algorithm <- if (from_cmdstan_csv) toupper(stan_args$engine) else stan_args$algorithm
  warmup <- if (from_cmdstan_csv) stanfit@sim$warmup2 else stanfit@sim$warmup
  
  samps_all <- rstan::extract(stanfit, permuted = FALSE, inc_warmup = TRUE)
  param_names <- dimnames(samps_all)[[3]] # stanfit@sim$fnames_oi
  param_dims <- stanfit@sim$dims_oi
  
  if (!(stan_algorithm %in% c("NUTS", "HMC"))) {
    warning("Most features of shinyStan are only available for models using
            algorithm NUTS or algorithm HMC.")
  }
  
  mname <- if (!missing(model_name)) model_name else stanfit@model_name
  
  slots <- list()
  slots$Class <- "shinystan"
  slots$model_name <- mname
  slots$param_names <- param_names
  slots$param_dims <- param_dims
  slots$param_groups <- names(param_dims)
  slots$samps_all <- samps_all
  slots$summary <- rstan::summary(stanfit)$summary
  slots$sampler_params <- rstan::get_sampler_params(stanfit)
  slots$nChains <- ncol(stanfit)
  slots$nIter <- nrow(samps_all) # total number of iterations (after thinning)
  slots$nWarmup <- floor(warmup / stanfit@sim$thin)
  if (!missing(notes)) slots$user_model_info <- notes
  slots$model_code <- rstan::get_stancode(stanfit)
  slots$stan_algorithm <- stan_algorithm
  
  do.call("new", slots)
}