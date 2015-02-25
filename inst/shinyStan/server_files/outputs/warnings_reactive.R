n_eff_warnings <- reactive({
  paste(.n_eff_warnings(fit_summary, threshold = input$n_eff_threshold), collapse = "\n")
})

rhat_warnings <- reactive({
  paste(.rhat_warnings(fit_summary, threshold = input$rhat_threshold), collapse = "\n")
})

mcse_over_sd_warnings <- reactive({
  paste(.mcse_over_sd_warnings(fit_summary, threshold = input$mcse_threshold), collapse = "\n")
})