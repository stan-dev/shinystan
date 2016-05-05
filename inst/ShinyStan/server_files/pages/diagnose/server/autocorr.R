calc_height_autocorr_plot <- reactive({
  params <- input$ac_params
  params <- .update_params_with_groups(params, PARAM_NAMES)
  LL <- length(params)
  LL <- ifelse(LL < 8, 8, LL)
  round(60 * LL)
})

autocorr_plot <- reactive({
  validate(
    need(input$ac_lags, message = "Loading..."),
    need(!is.null(input$ac_warmup), message = "Loading...")
  )
  samps <- if (!input$ac_warmup)
    SAMPS_post_warmup else SAMPS_all
  params <- .update_params_with_groups(input$ac_params, PARAM_NAMES)
  if (!length(params))
    params <- dimnames(samps)$parameters[1] # default to first parameter
  params <- unique(params)
  samps <- samps[, , params, drop = FALSE]
  do.call(
    ".autocorr_plot",
    args = list(
      samps           = samps,
      lags            = input$ac_lags,
      flip            = input$ac_flip,
      combine_chains  = input$ac_combine,
      partial         = input$ac_partial
    )
  )
})

output$autocorr_plot_out <- renderPlot({
  autocorr_plot()
}, bg = "transparent")

# download the plot
output$download_autocorr <- downloadHandler(
  filename = paste0('shinystan-autocorr-gg.RData'),
  content = function(file) {
    shinystan_autocorr_gg <- autocorr_plot()
    save(shinystan_autocorr_gg, file = file)
})
output$save_pdf_autocorr = downloadHandler(
  filename = "shinstan-autocorr.pdf",
  content = function(file) {
    ggsave(file, plot = autocorr_plot(), device = pdf)
})
