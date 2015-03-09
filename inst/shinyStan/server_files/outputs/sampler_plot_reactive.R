sampler_plot <- reactive({
  validate(need(sampler_params[[1]] != "Not Stan", message = "Only available for Stan models"),
           need(!(stan_algorithm == "HMC" &&  input$sampler_plot_param %in% c("n_divergent__", "treedepth__", "n_leapfrog__")), message = "Only available for algorithm = NUTS"))
  
  do.call(".sampler_plot", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val,
    param           = input$sampler_plot_param,
    smooth          = input$sampler_plot_smooth,
    smoothness      = input$sampler_plot_smoothness
  ))
})
