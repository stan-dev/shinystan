output$ui_tex_modal <- renderUI({
  bsModal("tex", title = "Print LaTeX table", trigger = "tex_options",
          helpText(style = "color: #337ab7;","The LaTeX table will print in the R console and can be pasted into a .tex file"),
          selectizeInput("tex_params", width = "100%", label = h5("Select or enter parameter names"), choices = .make_param_list_with_groups(object), multiple = TRUE,
                         options = list(placeholder = "Leave blank for all parameters")),
          flowLayout(checkboxInput("tex_booktabs", strong("Booktabs"), value = TRUE),
                     helpText("Use toprule, midrule, and bottomrule tags from LaTex package 'booktabs'.")),
          flowLayout(checkboxInput("tex_long", strong("Longtable"), value = FALSE),
                     helpText("Use LaTeX package 'longtable' for tables longer than a single page.")),
          hr(),
          actionButton("tex_go", "Print LaTeX", icon = icon("print", lib = "glyphicon"))
  )
})

