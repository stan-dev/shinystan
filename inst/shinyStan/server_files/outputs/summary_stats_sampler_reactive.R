summary_stats_sampler <- reactive({
  validate(need(sampler_params[[1]] != "Not Stan", message = "Only available for Stan models"))
  do.call(".sampler_summary", args = list(
    sampler_params  = sampler_params,
    inc_warmup      = input$sampler_warmup == "include",
    warmup_val      = warmup_val,
    report          = input$sampler_report,
    algorithm       = stan_algorithm,
    digits          = input$sampler_digits
  ))
})
