output$ui_multiparam_selectize <- renderUI({
  choices <- make_param_list_with_groups_sort()
  selected <- c(input$params_to_plot)
  column(6, selectizeInput("params_to_plot",
                           label = h5("Select or enter parameter names"),
                           width = '100%',
                           choices = choices,
                           multiple = TRUE))
})
