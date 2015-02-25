# calc_height_autocorr_plot
calc_height_autocorr_plot <- reactive({
  params <- input$ac_params
  params <- .update_params_with_groups(params, param_names)
  LL <- length(params)
  LL <- ifelse(LL < 8, 8, LL)
  round(60*LL)
})

# autocorr_plot
autocorr_plot <- reactive({

  validate(need(input$ac_lags, message = "Loading..."))

  samps <- if (input$ac_warmup == TRUE) samps_all else samps_post_warmup

  do.call(".autocorr_plot", args = list(
    samps           = samps,
    params          = input$ac_params,
    all_param_names = param_names,
    lags            = input$ac_lags,
    flip            = input$ac_flip,
    combine_chains  = input$ac_combine,
    nChains         = nChains
  ))
})
