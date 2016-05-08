summary_stats_latex <- reactive({
  params <- unique(.update_params_with_groups(input$tex_params, PARAM_NAMES))
  nParams <- length(params)
  if (nParams == 0)
    params <- PARAM_NAMES
  if (nParams == 1) {
    x <- do.call(".param_summary", args = list(
      param = params, 
      summary = SUMMARY
    ))
  } else {
    x <- do.call(".tex_summary", args = list(
      summary = SUMMARY[params,], 
      cols = input$tex_columns
    ))
  }
  
  pkgs <- input$tex_pkgs
  tab_env <- if ("Longtable" %in% pkgs)
    "longtable" else getOption("xtable.tabular.environment", "tabular")
  caption <- if (nzchar(input$tex_caption)) 
    input$tex_caption else NULL
  xt <- xtable::xtable(x, caption = caption)
  xtable::digits(xt) <- input$tex_digits
  if ("n_eff" %in% colnames(xt))
    xtable::display(xt)[1 + which(colnames(xt) == "n_eff")] <- "d"
  xtable::print.xtable(
    xt,
    booktabs = "Booktabs" %in% pkgs,
    tabular.environment = tab_env,
    include.rownames = FALSE
  )
})

output$summary_stats_latex_out <- renderPrint({
  input$tex_go
  isolate(summary_stats_latex())
})
