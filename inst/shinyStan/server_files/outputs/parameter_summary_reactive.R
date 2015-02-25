# summary stats (single parameter)
parameter_summary <- reactive({
  validate(need(input$param != "", message = FALSE))

  do.call(".param_summary", args = list(
    param       = input$param,
    summary     = fit_summary
  ))
})
