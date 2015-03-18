output$ui_multiparam_selectize <- renderUI({
  choices <- make_param_list_with_groups_sort()
  selected <- c(input$params_to_plot)
  
  wellPanel(
    fluidRow(
      column(6, selectizeInput("params_to_plot",
                               label = h5("Select or enter parameter names"),
                               width = '100%',
                               choices = choices,
                               multiple = TRUE)),
      column(3, offset = 1, sliderInput("param_plot_ci_level", h5("Credible interval"), width = "75%", ticks = FALSE, min = 50, max = 95, value = 50, step = 5, post = "%")),
      column(2, tags$div(h5("Customize"), includeHTML("html/multiparam_options.html")))
    )
  )
})



