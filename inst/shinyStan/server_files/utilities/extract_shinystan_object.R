# Extract the content of the shiny_stan_object slots
object <- shinystan_object
model_name <- object@model_name
samps_all <- object@samps_all
sampler_params <- object@sampler_params
nIter <- object@nIter
nChains <- object@nChains
warmup_val <- object@nWarmup
samps_post_warmup <- samps_all[(warmup_val + 1):nIter,, ,drop = FALSE]
fit_summary <- object@summary
param_names <- object@param_names
stan_algorithm <- object@stan_algorithm
# dependence <- object@dependence
