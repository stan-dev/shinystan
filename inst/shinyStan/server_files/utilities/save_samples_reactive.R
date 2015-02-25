# save_samples <- reactive({
#   params <- input$save_params
#   inc_warmup <- input$save_inc_warmup
#   combine_chains <- input$save_combine_chains
#
#   samps <- samps_all
#   if (!inc_warmup) samps <- samps_post_warmup
#
#   params <- .update_params_with_groups(params, object@param_names)
#   nParams <- length(params)
#
#   samps_use <- samps[,, params]
#   if (combine_chains) {
#     n <- dim(samps_use)[1] * dim(samps_use)[2]
#     samps_use <- array(samps_use, c(n, nParams))
#     colnames(samps_use) <- params
#   }
#   samps_use
# })
#
