sampler_plot_tests <- reactive({
  validate(
    need(sampler_params[[1]] != "Not Stan", message = "Only available for Stan models"),
    need(stan_algorithm == "NUTS", message = "Only available for algorithm = NUTS")
  )
})
sampler_plot_treedepth <- reactive({
  sampler_plot_tests()
  do.call(".sampler_plot_treedepth", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val,
    n_divergent     = "All"
  ))
})
sampler_plot_treedepth0 <- reactive({
  sampler_plot_tests()
  do.call(".sampler_plot_treedepth", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val,
    n_divergent     = 0
  ))
})
sampler_plot_treedepth1 <- reactive({
  sampler_plot_tests()
  do.call(".sampler_plot_treedepth", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val,
    n_divergent     = 1
  ))
})
sampler_plot_divergent <- reactive({
  sampler_plot_tests()
  do.call(".sampler_plot_divergent", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val
  ))
})
