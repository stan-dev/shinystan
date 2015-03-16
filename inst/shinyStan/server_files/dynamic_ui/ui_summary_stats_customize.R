output$ui_summary_stats_customize <- renderUI({
  bsCollapse(
    bsCollapsePanel(title = "View Table Options", id = "stats_table_options_collapse",
                    #                                                             bsButton("btn_open_glossary", "Open glossary", style = "link"),
                    actionLink("btn_open_glossary", "Open glossary", icon = icon("book", lib = "glyphicon")),
                    uiOutput("glossary_modal"),
                    br(),
                    numericInput("stats_digits", label = strong(style = "color: black;", "Decimal places"), value = 1, min = 0, max = 7, step = 1),
                    checkboxInput("user_regex",strong(style = "color: black;","Regex searching"), value = TRUE),
                    checkboxGroupInput("stats_columns", label = strong(style = "color: black;", "Columns"),
                                       choices = c("Rhat", "Effective sample size (n_eff)" = "n_eff", "Posterior mean" = "mean", "Posterior standard deviation" = "sd", "Monte Carlo uncertainty (se_mean)" = "se_mean", "Quantile: 2.5%" = "2.5%", "Quantile: 25%" = "25%", "Quantile: 50%" = "50%", "Quantile: 75%" = "75%", "Quantile: 97.5%" = "97.5%"),
                                       selected = c("Rhat", "n_eff", "mean", "sd", "2.5%", "50%", "97.5%")),
                    
                    downloadButton("download_all_summary", "Save"),
                    actionButton("tex_options", "LaTeX", icon = icon("print", lib = "glyphicon")),
                    bsModal("tex", title = "Print LaTeX table", trigger = "tex_options",
                            helpText("The LaTeX table will print in the R console and can be pasted into a .tex file"),
                            selectizeInput("tex_params", width = "100%", label = h5("Select or enter parameter names"), choices = .make_param_list_with_groups(object), multiple = TRUE,
                                           options = list(placeholder = "Leave blank for all parameters")),
                            checkboxInput("tex_booktabs", "Booktabs", value = TRUE),
                            checkboxInput("tex_long", "Longtable", value = FALSE),
                            hr(),
                            actionButton("tex_go", "Print LaTeX")
                    )
    )
  )
  
})