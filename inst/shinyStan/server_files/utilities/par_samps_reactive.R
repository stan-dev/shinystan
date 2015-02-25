# reactive function to get samples for a single parameter
par_samps_all <- reactive({
  param <- input$param
  p <- which(param_names == param)
  samps_all[,,p]
})
par_samps_post_warmup <- reactive({
  param <- input$param
  p <- which(param_names == param)
  samps_post_warmup[,,p]
})
