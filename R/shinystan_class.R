#' An S4 class for \code{shinystan} objects
#'
shinystan <- setClass("shinystan",
                      # Slots
                         slots = list(model_name      = "character",
                                      param_names     = "character",
                                      param_groups    = "character",
                                      param_dims      = "list",
                                      samps_all       = "array",
                                      summary         = "matrix",
                                      sampler_params  = "list",
                                      nChains         = "numeric",
                                      nIter           = "numeric",
                                      nWarmup         = "numeric",
                                      user_model_info = "character",
                                      model_code      = "character",
                                      stan_algorithm  = "character" # either NUTS or HMC
                                      ),
                      # Prototype
                         prototype = list(model_name = "No name",
                                      param_names = "",
                                      param_groups = "",
                                      param_dims = list(),
                                      samps_all = array(NA, c(1,1)),
                                      summary = matrix(NA, nr=1,nc=1),
                                      sampler_params = list("Not Stan"),
                                      nChains = 0,
                                      nIter = 0,
                                      nWarmup = 0,
                                      user_model_info = "Use this space to store notes about your model",
                                      model_code = "No code found. After closing shinyStan you can use the include_model_code function in R to add your code.",
                                      stan_algorithm  = "NUTS"
                                      )
                           )
