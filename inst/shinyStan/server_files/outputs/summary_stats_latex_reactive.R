# reactive function to make latex table of summary stats
summary_stats_latex <- reactive({
  
  params <- .update_params_with_groups(input$tex_params, param_names)
  if (length(params) == 0) params <- param_names
  if (length(params) == 1) {
    x <- do.call(".param_summary", args = list(
      param       = params,
      summary     = fit_summary
    ))
  } else {
    x <- do.call(".all_summary", args = list(
      fit_summary = fit_summary[params, ],
      digits      = input$stats_digits,
      cols        = input$stats_columns
    ))
  }

  
  tab_env <- if (input$tex_long) "longtable" else getOption("xtable.tabular.environment", "tabular")
  
  xtable::print.xtable(xtable::xtable(x), 
                       booktabs = input$tex_booktabs,
                       tabular.environment = tab_env,
                       include.rownames = FALSE
                       )
})

