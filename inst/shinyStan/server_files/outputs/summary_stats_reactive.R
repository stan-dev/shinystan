# reactive function to make table of summary stats with user-selected quantiles
summary_stats <- reactive({
  do.call(".all_summary", args = list(
    fit_summary = fit_summary,
    digits      = input$stats_digits,
    cols        = input$stats_columns
  ))
})
