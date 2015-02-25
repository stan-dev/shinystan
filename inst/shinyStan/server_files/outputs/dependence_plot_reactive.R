# two functions: density_plot, density_plot_multiview

dependence_plot <- reactive({
  if (is.null(input$dependence_lag) | is.null(input$dependence_span)) {
    return()
  }
  lag_text <- input$dependence_lag
  lag <- eval(parse(text = lag_text))

  do.call(".dependence_plot", args = list(
    session         = session, # needed for updating progress bar
    dependence      = dependence,
    lag             = lag,
    samps           = samps_all,
    warmup          = warmup_val,
    model_name      = model_name,
    sampler_params  = sampler_params,
    pars            = dependence$pars_oi,
    f               = input$dependence_span,
    rug             = input$dependence_rug == "show"
  ))
})
