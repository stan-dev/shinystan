sampler_plot_tests <- reactive({
  validate(
    need(sampler_params[[1]] != "Not Stan", message = "Only available for Stan models"),
    need(stan_algorithm == "NUTS", message = "Only available for algorithm = NUTS")
  )
})
sampler_plot_treedepth_hist <- reactive({
  sampler_plot_tests()
  
  do.call(".sampler_plot", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val,
    param           = "treedepth__",
    type            = "bar"
  ))
})

sampler_plot_treedepth_freqpoly <- reactive({
  sampler_plot_tests()
  
  do.call(".sampler_plot", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val,
    param           = "treedepth__",
    type            = "freqpoly"
  ))
})

sampler_plot_divergent <- reactive({
  sampler_plot_tests()
  
  do.call(".sampler_plot", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val,
    param           = "n_divergent__"
  ))
})
