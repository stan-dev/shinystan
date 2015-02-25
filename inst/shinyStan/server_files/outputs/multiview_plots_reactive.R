
trace_plot_multiview <- reactive({
  validate(need(input$param, message = FALSE))
  #   need(input$multiview_warmup, message = FALSE)
  
  
  do.call(".param_trace", args = list(
    param       = input$param,
    dat         = par_samps_all(),
    chain       = 0,
    warmup_val  = warmup_val,
    inc_warmup  = input$multiview_warmup, # input$multiview_warmup == "include",
    palette     = "Default",
    style       = "line",
    rect        = "None",
    rect_color  = "#d9e7f4",
    rect_alpha  = 0.5,
    x1          = NA,
    x2          = NA,
    y1          = NA,
    y2          = NA
  ))
})


autocorr_plot_multiview <- reactive({
  validate(need(input$param, message = FALSE))
  
  if (is.null(input$multiview_warmup)) warmup <- FALSE
  else warmup <- input$multiview_warmup

  lags <- min(25, round((nIter-warmup_val)/2))

  if (warmup == FALSE) {
    samps <- par_samps_post_warmup()
  } else {
    samps <- par_samps_all()
  }
  
  do.call(".autocorr_single_plot", args = list(
    samps = samps,
    lags = lags
  ))
})


density_plot_multiview <- reactive({
  
  validate(need(input$param, message = FALSE))
  #   need(input$multiview_warmup, message = FALSE))
  
  if (input$multiview_warmup == FALSE) {
    samps <- par_samps_post_warmup()
  } else {
    samps <- par_samps_all()
  }
  
  do.call(".param_dens", args = list(
    param       = input$param,
    dat         = samps,
    chain       = 0,
    chain_split = FALSE,
    fill_color  = "gray35",
    line_color  = "lightgray",
    point_est   = "None",
    CI          = "None",
    #     y_breaks    = "None",
    x_breaks    = "Some",
    title       = FALSE
  ))
})
