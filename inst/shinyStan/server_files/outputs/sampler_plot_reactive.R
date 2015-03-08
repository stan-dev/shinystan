sampler_plot <- reactive({
  validate(need(sampler_params[[1]] != "Not Stan", message = "Only available for Stan models"))
  do.call(".sampler_plot", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val,
    param           = input$sampler_plot_param,
    smooth          = input$sampler_plot_smooth
  ))
})
